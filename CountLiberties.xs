// Emacs: -*- mode: C++;-*-

// Activate assertions
// #undef NDEBUG

// Use tcmalloc, part of Google Performance Tools:
//   http://goog-perftools.sourceforge.net/
// This is optional. Standard system malloc works fine but is slower and bloats
// Notice that this define is NOT what decides that the program runs with
// tcmalloc. That is done with a preload (use count_liberties --tcmalloc)
// This define merely makes some tcmalloc memory statistics available
#define TCMALLOC

// Use jemalloc
//   http://www.canonware.com/jemalloc/
// This is optional. Standard system malloc works fine but is slower and bloats
// Notice that this define is NOT what decides that the program runs with
// jemalloc. That is done with a preload (use count_liberties --jemalloc)
// This define merely makes some jemalloc memory statistics available
#define JEMALLOC

// If CONDITION_VARIABLE is not defined mutexes will be used in an undefined way
// In particular the thread that unlocks will be different from the thread that
// locked. This is undefined behaviour in the C++ standard, but it works on all
// systems I have access to
// If however you are on a system where locking threads indeed "own" mutexes
// simply set CONDITION_VARIABLE and well defined C++ condition variables will
// be used instead. The resulting code will be a very tiny bit slower
#ifndef __linux
# define CONDITION_VARIABLE
#endif /* __linux */

#define PERL_NO_GET_CONTEXT	/* we want efficiency */
#define __STDC_LIMIT_MACROS

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#define NEED_newRV_noinc_GLOBAL
#include "ppport.h"
#define NEED_sv_2pv_flags_GLOBAL
#define NEED_vnewSVpvf_GLOBAL
#define NEED_warner_GLOBAL

#undef seed

#include "constants.hpp"
#include "vector.hpp"

#ifdef TCMALLOC
# include <gperftools/malloc_extension.h>
#endif /* TCMALLOC */

#ifdef JEMALLOC
# include <jemalloc/jemalloc.h>
#endif /* JEMALLOC */

#include <errno.h>
#include <assert.h>

#include <cmath>
#include <cstring>
#include <cstdlib>
#include <system_error>
#include <array>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <vector>
#include <thread>
#include <atomic>
#include <mutex>
#include <condition_variable>

#include <cerrno>
#include <sched.h>

// These are actually in revision.cpp
extern char const revision_system[];
extern char const parent_revision[];
extern char const current_revision[];
extern char const compile_date[];
extern char const compile_time[];

// For some reason this does not get defined in C++ even though gcc supports it
#undef PERL_UNUSED_DECL
#define PERL_UNUSED_DECL __attribute__unused__

/* Workaround for older perls without packWARN */
#ifndef packWARN
# define packWARN(a) (a)
#endif

/* Workaround for older perls without HvENAME */
#ifndef HvENAME
# define HvENAME(a)	HvNAME(a)
#endif /* HvENAME */

bool const DEBUG_FLOW     = false;
bool const DEBUG_STORE    = false;
bool const DEBUG_FETCH    = false;
bool const DEBUG_THREAD   = false;
bool const INITIAL_INSERT = false;
bool const COST           = true;

// Immediately drop a column if we notice that we closed a loop
// The program will soon discover this for itself, but it gains some speed
// It's easy to prove that this optimization is proper for a loop with a hole
// and for a tight 2x2 cluster if it is connected to somewhere else. The lonely
// 2x2 block is less obvious. For some non rectangular boards it actually is
// optimal. For rectangular boards it would need at least a 4x4 board to not
// be obviously non-optimal, and on 4x4 we know the solution is 9 liberties, not
// 8, so even embedded in even larger boards this remains true.
// Therefore this pruning is proper. It may suppress some optimal
// solutions but at least one optimal solution is guaranteed to survive
bool const PRUNE_LOOPS  = true;

/*
 Suppose you have 2 consecutive stones on a top (or bottom) row
 If it is a line not connected to anything else this can only happen if
 height < 3, otherwise it is better to shift the row one down. So from here on
 only consider boards with height >= 3 where the row is connected to another row
 Consider the left-most directly connected stone on the second row and the most
 the top row extends to the right. If this is to the right of the this left-most
 connection to the second row the cases are:

  1.  There is also a connection to the second row at the extreme right:
      -----                                        -----
      XXXXX  can without any loss (and often gain) XLLLX
      X???X  be changed to                         XXXXX

  2.  There is no connection to the second row at the extreme right

      2a. The top row extends to the edge of the board
           -----+                                        -----+
           XXXXX|  can without any loss (and often gain) XLLLL|
           X????|  be changed to                         XXXXX|

      2b. the top row does not reach to the end so it ends on a liberty
          the end of the top row is not connected down, so also a liberty
          (board height >= 3). So it is of the form:
           ------
           XXXXXL
           X???L?
          Consider 3 cases for the bottem right of the figure
          2b1. A stone at the bottom right
               ------                                   ------
               XXXXXL Can never be optimal compared to  XXXXLL
               X???LX                                   X???LX

          2b2. Empty at the bottom right
               ------                                   ------
               XXXXXL Can without loss (and often gain) XLLLL?
               X???LE be changed to                     XXXXXL

          2b3. Liberty at the bottom right. This liberty must come from contact
               with a stone (so we can extend that stone)
               ------                                   ------
               XXXXXL Can without loss be changed to    XXXXLL
               X???LL (then maybe repeat reasoning)     X???LX

 So any extension to the right of the top row can be removed
 By symmetry any extension to the left can be removed too. So the only way
 the top (or bottom) row maybe need to be reached is as a column of width 1:
           ---
            X
            X
 So the first thing PRUNE_SIDES does is:
   A. if a stone on the top of bottom row, don't put a stone to the right

 Next consider a top row vertex that is already known to be a liberty with a
 stone to the right:
           ---
           LX?
            ?
 Due to the reasoning above we will never put a stone on the top right ?
 And if height >= 3 we MUST put a stone at the bottom ? or the top stone will
 be disconnected, so we have:
           ---
           LXL
            X?
 We can without loss (and possible gain) replace that with:
           ---
           LLL
            XX
 So the second thing PRUNE_SIDES does is:
   B. if a liberty on the top or bottom row, don't put a stone to the right

 Both rules only apply if height >= 3. Since we will only apply them in _process
 when direction > 0 or direction < 0 we can ignore height == 1 (only called with
 direction == 0) and height == 2 is only called with direction >= 0 so we only
 have to check height != 2 in the direction > 0 case
*/
bool const PRUNE_SIDES  = true;

size_t const PAGE_SIZE = 4096;

bool const ARENA_MALLOC = true;
// size_t const ARENA_ALIGNMENT = _CACHE_LINE;
size_t const ARENA_ALIGNMENT = PAGE_SIZE;

void fatal(std::string const message) NORETURN COLD;
void sys_fatal(std::string const message) NORETURN COLD;

NOINLINE void fatal(std::string message) {
    if (true) {
        std::cerr << message << std::endl;
        abort();
    } else {
        throw std::logic_error(message);
    }
}

NOINLINE void sys_fatal(std::string message) {
    message += ": ";
    message += strerror(errno);
    fatal(message);
}

// Only call on unsigned types or be sure that the top it is not set
template <class any>
ALWAYS_INLINE int clz(any x) {
#ifdef __GNUC__
    if (sizeof(x) == sizeof(unsigned int))
        return __builtin_clz(x);
    if (sizeof(x) == sizeof(unsigned long))
        return __builtin_clzl(x);
    if (sizeof(x) == sizeof(unsigned long long))
        return __builtin_clzll(x);
#endif /* __GNUC__ */
    int r = sizeof(x) * 8 - 1;
    if (x >> 32) {
        x >>= 32;
        r -= 32;
    }
    if (x >> 16) {
        x >>= 16;
        r -= 16;
    }
    if (x >> 8) {
        x >>= 8;
        r -= 8;
    }
    if (x >> 4) {
        x >>= 4;
        r -= 4;
    }
    if (x >> 2) {
        x >>= 2;
        r -= 2;
    }
    if (x >> 1) {
        // x >>= 1;
        r -= 1;
    }
    return r;
}

class CountLiberties {
  public:
    typedef uint8_t Liberties;
    typedef uint8_t State;

    enum {
        BLACK		= 0,
        BLACK_UP	= 1,
        BLACK_DOWN	= 2,
        BLACK_UP_DOWN	= 3,
	// EMPTY and LIBERTY must take postions 4 and 7 because they get
        // processed with STONE_MASK applied and therefore map to BLACK and
        // BLACK_UP_DOWN. And only these two don't change under bit reverse
        // We chose LIBERTY = 4 so that the initial "all liberties" has value 0
        LIBERTY		= 4,
        EMPTY		= 7,
        STATES		= 8,

        BITS_PER_VERTEX = 2,
        // Mask to select the 2 bits of a vertex
        STONE_MASK	= (1 << BITS_PER_VERTEX) -1,		// 0x03,

        // MAX_SIZE will effectively be rounded up to the next multiple of 4
        // (result available as EXPANDED_SIZE)
        // MAX_SIZE can be increased up to 24, but 21 or above leave only
        // 8 HISTORY_BITS, so finding an actual solution will be slow
        MAX_SIZE	= 19,					// 19
        // MAX_SIZE	= 24,					// 24
        MAX_BITS	= MAX_SIZE*BITS_PER_VERTEX,		// 38
        COMPRESSED_SIZE	= (MAX_BITS+7)/8,			//  5
        EXPANDED_SIZE   = COMPRESSED_SIZE*8/BITS_PER_VERTEX,	// 20

        HISTORY_BYTES	= sizeof(uint64_t) - COMPRESSED_SIZE - sizeof(Liberties),      		//  2
        HISTORY_BITS	= HISTORY_BYTES*8,			// 16

        // The most liberty count can grow during one round (up, left, down)
        MAX_GROWTH	= 3,
        // Exact value doesn't matter, just needed to put some reasonable limit
        MAX_THREADS	= 1024,
    };

    static constexpr float MAP_LOAD_FACTOR	= 0.5;
    static constexpr float BACKBONE_LOAD_FACTOR	= 0.4;

    enum Operation {
        WAITING = 0,
        CALL_DOWN,
        CALL_UP,
        CALL_ASYM_FINAL,
        CALL_SYM_FINAL,
        SIGNATURE,
        FINISH,
    };

    static_assert(HISTORY_BYTES >= 0,
                  "We won't be able to fit an Entry in an uint64_t");

    // Represent a column as an array of intersections that each have a State
    class Column {
      public:
        Column() {};
        Column(char const* from, int height);

        auto      & operator[](int i)       { return column_[i]; }
        auto const& operator[](int i) const { return column_[i]; }

        // Construct a string as a sequence of the actual numeric state values
        std::string to_raw_string(size_t height) const;

        // Construct a "smart" string indicating which stones are connected
        // and which empties are liberties or not
        char to_string(char* result, size_t height) const;
        auto to_string(const size_t height) const {
            char buffer[EXPANDED_SIZE+1];
            to_string(buffer, height);
            return std::string(buffer, height);
        }

      private:
        std::array<State, EXPANDED_SIZE> column_;
    };

    // Represent a whole column state inside a single integer
    // This is not enough information to recover the full state in itself
    // It needs to be combined with information with which intersections
    // have stones or are empty
    //  MSB                                          LSB
    // +-------------------+----------------+-----------+
    // + Column (2 bits    | history        | number of |
    // + per intersection) | (8 or 16 bits) | liberties |
    // +-------------------+----------------+-----------+
    //                      <------ Mask::shift64 ------
    //  1111111111111111111 0000000000000000 000000000000: Mask::column_mask64

    // number of liberties will often be an offset from some fixed value to be
    // able to handle liberties >= 256

    class Entry;
    class EntrySet;

    class CompressedColumn {
      public:
        class Mask {
            friend class CompressedColumn;
            friend class Entry;
            friend class EntrySet;
          private:
            static uint const shift64 = 8*(sizeof(uint64_t) - COMPRESSED_SIZE);
            static uint64_t const column_mask64	= UINT64_C(-1) << shift64;

            // Repeated 01 bit pattern in the upper COMPRESSED_SIZE bytes
            static uint64_t const FILL_MULTIPLIER = UINT64_C(-1) / 3 << shift64;

          public:
            ALWAYS_INLINE
            Mask() {}
            // Create a proper 2 bit mask for position "pos" to be used in
            // set_liberty, set_empty and set_black
            ALWAYS_INLINE
            explicit operator bool() const { return mask_ != 0; }
            //Mask operator~() const {
            //    return Mask{~mask_};
            //}
            template <class any>
            ALWAYS_INLINE
            Mask operator<<=(any const& shift) {
                mask_ <<= shift;
                return *this;
            }
            template <class any>
            ALWAYS_INLINE
            Mask operator>>=(any const& shift) {
                mask_ >>= shift;
                return *this;
            }
            ALWAYS_INLINE
            Mask& operator&=(Mask const& rhs) {
                mask_ &= rhs.mask_;
                return *this;
            }
            ALWAYS_INLINE
            static Mask stone_mask(uint pos) {
                // Take care! do not use if shift can be >= sizeof(uint64_t)
                return _stone_mask(stone_shift(pos));
            }
            ALWAYS_INLINE
            static uint stone_shift(uint pos) {
                return pos * BITS_PER_VERTEX + shift64;
            }
            ALWAYS_INLINE
            static Mask _stone_mask(uint pos, uint64_t mask= STONE_MASK) {
                // Take care! do not use if pos can be >= sizeof(uint64_t)
                return Mask{mask << pos};
            }
            // Create a proper mask for use in backbone() from a position bitmask
            // e.g "1001" becomes "11000011xxxxxxx" (xxx are 0 for history and bits)
            static Mask backbone_mask(uint index) {
                uint64_t mask{0};
                uint64_t add{static_cast<uint64_t>(STONE_MASK) << Mask::shift64};
                while (index) {
                    if (index & 1) mask += add;
                    index >>= 1;
                    add <<= BITS_PER_VERTEX;
                }
                return Mask{mask};
            }
            auto black_mask() const;
            auto black_up_mask() const;
            auto black_down_mask() const;
            auto black_up_down_mask() const;

          private:
            ALWAYS_INLINE
            Mask(uint64_t mask) : mask_{mask} {}
            ALWAYS_INLINE
            Mask& operator|=(uint64_t mask) {
                mask_ |= mask;
                return *this;
            }
            ALWAYS_INLINE
            Mask& operator>>=(uint shift) {
                mask_ >>= shift;
                return *this;
            }
            ALWAYS_INLINE
            auto value() const { return mask_; }

            uint64_t mask_;
        };
        // At some point make this a separate type.
        // Rethink the Mask/ColumnOnly/CompressedColumn hierarchy
        typedef Mask ColumnOnly;

        // Number of bytes in the Column part
        constexpr size_t length() const { return COMPRESSED_SIZE; }
        // Fetch the Column bits
        uint64_t column() const {
            return _column() >> Mask::shift64;
        }
        // Set only the Column bits, leave history and liberties as is
        void column(uint64_t column) {
            value_ = (value_ & ~Mask::column_mask64) | column << Mask::shift64;
        }
        static auto __fast_hash(uint64_t column, uint shift = Mask::shift64) -> uint64_t {
            return column * lcm_multiplier >> shift;
        }
        static auto _fast_hash(uint64_t column, uint shift = Mask::shift64) -> uint64_t {
            return __fast_hash(column >> Mask::shift64, shift);
        }
        // Calculate a low quality hash over the Column bits
        // (excluding liberties and history)
        // Result is of the same length as the Column bits if shift not given
        // In general shift can be used to limit the range of the result while
        // using the high bits (which are of better quality)
        // This hash is used to place elements in the EntrySet
        auto fast_hash(uint shift = Mask::shift64) const -> uint64_t {
            // return murmur_mix(column());
            // return column();	// Very bad collisions
            // Terrible hash, but seems to perform really well for our case
            return _fast_hash(_column(), shift);
        }
        // Calculate a higher quality hash over Column bits
        // (excluding liberties and history)
        // This hash is used to calculate the signature of a column set
        auto murmur(uint64_t seed = murmur_seed) const {
            // Derived from the glibc variant of a 64-bit Murmur hash
            // This is NOT any of the "official" murmur hashes
            // I just needed a fast reasonably good full 64 bit hash
            uint64_t hash = seed ^ column();
            hash = murmur_mix(hash);
            hash = murmur_mix(hash);
            return hash;
        }
        auto hash(uint64_t seed) const { return murmur(seed); }
        auto hash()              const { return murmur(); }

#ifdef __POPCNT__
        static uint half_popcount64(uint64_t v) {
            return _mm_popcnt_u64(v) / 2;
        }
#endif /* __POPCNT__ */
        // Clears a full column and (optionally) sets liberties
        void clear(Liberties liberties = 0) {
            value_ = liberties;
        }
        // Are there ane EMPTY (NOT LIBERTY) vertices that are not EMPTY in mask
        auto any_empty(Mask index_mask, CompressedColumn mask, uint shift = Mask::shift64) const -> bool {
            return any_empty(index_mask, mask._column(), shift);
        }
        // The number of EMPTY (NOT LIBERTY) vertices that are not EMPTY in mask
        auto nr_empty(Mask index_mask, 
                      CompressedColumn mask) const -> uint {
            return nr_empty(index_mask, mask._column());
        }
        auto nr_empty(Mask index_mask) const -> uint {
            return nr_empty(index_mask, 0);
        }

        // Return true if and only if Column contains more than 1 chain
        // "mask" is a backbone_mask indicating which intersections are stones
        bool multichain(Mask mask) const;
        // Recover a full column from a compressed column and a mask "from"
        // "from" must have a 1 in each position that has a stone
        // Height indicates how many fields we have to fill in
        void expand(Column& column, int from, int height) const;

        ColumnOnly test_vertex(Mask mask) const {
            return  Mask{_column() & mask.value()};
        }
        auto _get_vertex(uint shift) const {
            // Take care! do not use if shift can be >= sizeof(uint64_t)
            return  _column() >> shift & STONE_MASK;
        }
        auto get_vertex(uint shift) const {
            // Take care! do not use if final shift can be >= sizeof(uint64_t)
            _get_vertex(shift * BITS_PER_VERTEX);
        }
        // Force position (2 set bits in mask) to an empty that is a liberty
        void set_liberty(Mask mask) {
            clear_stone_mask(mask);
        }
        // Force position (2 set bits in mask) to an empty that is not a liberty
        void set_empty(Mask mask) {
            set_stone_mask(mask);
        }
        // Force position (2 set bits in mask) to be an isolated stone
        // (if some of the mask bits are zero then keep pre-existing direction)
        void set_black(Mask mask) {
            clear_stone_mask(mask);
        }
        // Add direction determined by 1 bits to stone set using set_black()
        void add_direction(Mask mask) {
            set_stone_mask(mask);
        }
        std::string raw_column_string() const;

        // Remove the down pointer to the current group looking up
        void _terminate_up(Mask down_mask, ColumnOnly value);
        // Remove the up pointer to the current group looking down
        void _terminate_down(Mask up_mask, ColumnOnly value);
        // Go to the top of the current group and make it point up
        void _join_up(Mask stone_mask, ColumnOnly value);
        // Go to the bottom of the current group and make it point down
        void _join_down(Mask stone_mask, ColumnOnly value);

        // Check if 2 compressed columns have equal Column bits
        friend bool equal(CompressedColumn const& lhs, CompressedColumn const& rhs);
        // Check if 2 compressed columns are completely equal
        // (including history and liberties)
        friend bool _equal(CompressedColumn const& lhs, CompressedColumn const& rhs);
        // Check if a compressed columns comes before another
        // (only looking at Column bits, ignoring history and liberties)
        friend bool less(CompressedColumn const& lhs, CompressedColumn const& rhs);
        // Check if a compressed columns comes before another
        // (looking at Column bits, then history and then liberties)
        friend bool _less(CompressedColumn const& lhs, CompressedColumn const& rhs);
      protected:
        static uint const shift8  = 8*(sizeof(uint64_t) - 1);
        static uint64_t const murmur_seed       = UINT64_C(0xc70f6907);
        static uint64_t const lcm_multiplier    = UINT64_C(6364136223846793005);
        static uint64_t const murmur_multiplier = UINT64_C(0xc6a4a7935bd1e995);

        // Get raw column value
        uint64_t _column() const {
            return value_;
        }
        // Set raw column value
        void _column(uint64_t value) {
            value_ = value;
        }
        static uint64_t murmur_mix(uint64_t v) {
            v *= murmur_multiplier;
            return v ^ (v >> 47);
        }
        void set_stone_mask(Mask mask) {
            _column(_column() |  mask.value());
        }
        void clear_stone_mask(Mask mask) {
            _column(_column() & ~mask.value());
        }
        // Add the bits from mask to index_mask (or operation)
        // Then check if any *NON* correspondig columns bits are set
        auto any_empty(Mask index_mask, uint64_t mask = 0, uint shift = Mask::shift64) const -> bool;
        auto nr_empty(Mask index_mask,
                      uint64_t mask,
                      uint shift = Mask::shift64) const -> uint;

      private:
        // The actual column data is in the COMPRESSED_SIZE MSBs
        // For Entry: liberties will be in the LSB
        // For Entry: history bits will be inbetween,
        //            byte [1, 8-COMPRESSED_SIZE-1]
        uint64_t value_;
    };

    // Entry is a CompressedColumn for use inside an EntrySet
    class Entry: public CompressedColumn {
        friend EntrySet;
      public:
        // Get liberties but shifted into the high bits
        auto _liberties() const { return _column() << shift8; }
        // Get liberties
        auto liberties() const { return _column() & liberty_mask; }
        // Get liberties shifted by offset
        uint liberties(int offset) const {
            return static_cast<int>(liberties()) + offset;
        }
        // Increase liberties by add
        void liberties_add(uint64_t add) {
            // Caller should make sure this doesn't overflow
            _column(_column() + add);
        };
        // Decrease liberties by sub
        void liberties_subtract(uint64_t sub) {
            // Caller should make sure this doesn't underflow
            _column(_column() - sub);
        };

        // Calculate hash signature of a finished column
        // Used to recognize repeated columns
        uint64_t signature(Liberties max, int from) const {
            // Notice that diff can be "negative" because max is only over
            // the entries with at most 1 chain. In such case the result will
            // wrap since Liberties is an unsigned type, so diff will end up
            // positive. We don't care, it still leads to a unique result
            Liberties diff = max - liberties();

            uint64_t seed =
                murmur_seed ^
                (static_cast<uint64_t>(from) << sizeof(Liberties) * 8) ^
                diff;
            return hash(seed);
        }

        // Set all history bits to 0
        void history_clear() { _column(_column() & ~history_mask); }
        // Set history bit "record" to 0
        void record0(int record) {
            if (record >= 0)
                _column(_column() & ~(1 << (record + 8)));
        }
        // Set history bit "record" to 1
        void record1(int record) {
            if (record >= 0)
                _column(_column() |  (1 << (record + 8)));
        }
        // Get history bit "bit"
        auto history(int bit) const { return _column() >> (bit+8) & 1; }
        // Get the string of history bits (grouped by 8)
        std::string history_bitstring() const;

        // Return a new entry created from the current one after applying a mask
        Entry backbone(uint64_t mask) const {
            Entry temp;
            temp._column(_column() & mask);
            return temp;
        }
        // An Entry that is not equal to any valid column
        static Entry invalid(uint64_t base = BLACK_UP) {
            Entry temp;
            temp._column(base << Mask::shift64);
            return temp;
        }
        // An Entry containing index_mask with the first and last 1 removed
        // E.g 0001100...01100 becomes 0000100...01000
        static Entry full(CompressedColumn::Mask index_mask) {
            uint64_t mask = index_mask.value();
            mask &= (UINT64_C(-1) >> 1) >> clz(mask);
            mask &= mask - 1;
            Entry result;
            result._column(mask);
            return result;
        }
      private:
        static uint64_t const liberty_mask = UINT64_C(0xff);
        static uint64_t const history_mask = UINT64_C(-1) >> 8*(COMPRESSED_SIZE+1) << 8;
    };
    typedef std::vector<Entry> EntryVector;

    // Very specialised version of std::unordered_set<Entry>
    // Never resize except if empty
    // Never give back memory
    // Never erase
    // Knows what an invalid Entry looks like
    // As a result it's WAY faster than any normal hash implementation
    class EntrySet {
      public:
        typedef size_t size_type;
        class value_type {
          public:
            // EntrySet starts filled with UNSET entries. Check
            bool unset()      const { return entry._column() == UNSET; }
            Entry entry;
        };

        class iterator {
          public:
            iterator(value_type* ptr): ptr_{ptr} {}
            bool operator== (iterator const& rhs) const {
                return ptr_ == rhs.ptr_;
            }
            bool operator!= (iterator const& rhs) const {
                return ptr_ != rhs.ptr_;
            }
            // Skip to the next set entry (could be the TERMINATOR)
            iterator& operator++() {	// prefix ++
                do {
                    ++ptr_;
                    // std::cout << "Consider " << *ptr_ << "\n";
                } while (ptr_->unset());
                // std::cout << "Iterated to " << *ptr_ << "\n";
                return *this;
            }
            iterator operator++(int) {	// postfix ++
                iterator old{*this};
                ++*this;
                return old;
            }
            value_type& operator*() {
                return *ptr_;
            }
            value_type* operator->() {
                return ptr_;
            }
          private:
            value_type* ptr_;
        };
        class const_iterator {
          public:
            const_iterator(value_type const* ptr): ptr_{ptr} {}
            bool operator== (const_iterator const& rhs) const {
                return ptr_ == rhs.ptr_;
            }
            bool operator!= (const_iterator const& rhs) const {
                return ptr_ != rhs.ptr_;
            }
            // Skip to the next set entry (could be the TERMINATOR)
            const_iterator& operator++() {	// prefix ++
                do {
                    ++ptr_;
                    // std::cout << "Consider " << *ptr_ << "\n";
                } while (ptr_->unset());
                // std::cout << "Const Iterated to " << *ptr_ << "\n";
                return *this;
            }
            const_iterator operator++(int) {	// postfix ++
                const_iterator old{*this};
                ++*this;
                return old;
            }
            value_type const& operator*() const {
                return *ptr_;
            }
            value_type const* operator->() const {
                return ptr_;
            }
          private:
            value_type const* ptr_;
        };

        auto size()  const { return size_; }
        auto empty() const { return size_ == 0; }
        auto used()  const { return mask_+1; }

        EntrySet():
            arena_{nullptr},
            mask_{0},
            size_{0}
            {
            }
        ~EntrySet() {
            // No memory is freed because we don't allocate any
        };
        // Takes space for max+1 Entries rounded up to CACHE_LINE size from ptr
        // (The +1 is for the TERMINATOR)
        void alloc_arena(value_type* &ptr, size_t max) {
            if (size_) fatal("Cannot alloc if not empty");
            //  std::cout << "arena " << ptr << ", size " << max << "+1 value_types\n";
            arena_ = ptr;
            ptr += ((max+1) * sizeof(value_type) + _CACHE_LINE - 1) / _CACHE_LINE * _CACHE_LINE / sizeof(value_type);
            mask_ = 0;
        }

        // Make sure we don't copy a map by accident
        EntrySet(EntrySet const& map)  = delete;
        EntrySet(EntrySet const&& map) = delete;
        EntrySet& operator= (EntrySet const&) = delete;

        iterator	begin()		{
            // dump();
            // std::cout << "Iterate over " << (void *) this << "\n";
            iterator pos{&arena_[-1]};
            return ++pos;
        }
        const_iterator	begin() const	{
            // dump();
            // std::cout << "Const Iterate over " << (void *) this << "\n";
            const_iterator pos{&arena_[-1]};
            return ++pos;
        }
        iterator	end()		{ return &arena_[used()]; }
        const_iterator	end() const	{ return &arena_[used()]; }
        // Debug: Raw dump of all Entries in EntrySet
        void dump() const {
            std::cout << "dump " << (void*) this << " =";
            for (size_type i=0; i < used(); ++i)
                std::cout << "\t" << arena_[i].entry._column();
            std::cout << "\n";
        }
        // Make EntrySet empty again
        void clear() {
            // std::cout << "Clear " << (void*) this << "\n";
            // repeated UNSET8 leads to UNSET
            if (size_) {
                std::memset(&arena_[0], UNSET8, used() * sizeof(value_type));
                size_ = 0;
            }
        }
        // Reserve space for elements*load_multiplier Entries
        // (rounded up to the next power of 2 entries)
        // Does NOT clear(), but does make sure of a proper TERMINATOR and that
        // the old TERMINATOR is changed to UNSET (on resize)
        // (The load_multiplier is needed to reduce hash collisions)
        void reserve(size_type elements, float load_multiplier) {
            // Most reserves are for size 0
            // Ignoring size 0, most reserves end up at 1 << (height()+1)/2
            // std::cout << "Reserve " << (void*) this << ": " << elements << "\n";
            if (size_) fatal("reserve only supported on empty EntrySets");
            size_type target = elements * load_multiplier;
            if (target) {
                --target;
                if (target < elements) target = elements;
                // We must have at least 1 empty to prevent find() from looping
                assert(target > 0);
                shift_ = clz(target);
                // Set all bits after the first one
                target = (static_cast<size_type>(0) - 1) >> shift_;
                shift_ += (sizeof(uint64_t) - sizeof(target)) * 8;
            }
            // std::cout << "Really Reserve " << target+1 << "\n";
            if (target == mask_) return;
            arena_[used()].entry._column(UNSET);
            mask_ = target;
            arena_[used()].entry._column(TERMINATOR);
        }
        // Normally insert returns pair of iterator and bool
        // We return true in case the entry already existed
        // (based on only the Column bits without history and liberties)
        // In all cases we set the address for the relevant old/new entry
        bool insert(Entry entry, value_type*& where) {
            assert(entry._column() != UNSET);
            assert(entry._column() != TERMINATOR);
            // if (size_ >= used() * max_load_factor_) fatal("size " + std::to_string(size_) + ", used=" + std::to_string(used()));
            // if (used_ <= 2) fatal("Insert while not enough reserved");
            size_type pos = entry.fast_hash(shift_);
            if (arena_[pos].unset()) {
                arena_[pos].entry = entry;
                // std::cout << "Insert " << arena_[pos] << " at " << (void *) this << "[" << pos << "] (try 1)\n";
                // std::cout << "Insert at DIB 1\n";
                ++size_;
                where = &arena_[pos];
                return false;
            }
            // Quadratic probing
            uint64_t column = entry.column();
            size_type add  = 1;
            size_type add2 = 2;
            while (true) {
                if (arena_[pos].entry.column() == column) {
                    // std::cout << "Clash " << arena_[pos] << " at " << (void *) this << "[" << pos << "] (try " << add2-1 << ") versus " << entry._column() << "\n";
                    // std::cout << "Clash at DIB " << add2-1 << "\n";
                    where = &arena_[pos];
                    return true;
                }
                pos = (pos + add) & mask_;
                if (arena_[pos].unset()) {
                    arena_[pos].entry = entry;
                    // std::cout << "Insert " << arena_[pos] << " at " << (void *) this << "[" << pos << "] (try " << add2 << ")\n";
                    // std::cout << "Insert at DIB " << add2 << "\n";
                    ++size_;
                    where = &arena_[pos];
                    return false;
                }
                add += add2;
                ++add2;
            }
        }
        // Same as normal insert, but we detect already existing entries
        // by only looking at the bits determined by mask
        // (always use the same mask or no mask)
        bool insert(Entry entry, value_type*& where, CompressedColumn::Mask mask) {
            assert(entry._column() != UNSET);
            assert(entry._column() != TERMINATOR);
            // if (size_ >= used() * max_load_factor_) fatal("size " + std::to_string(size_) + ", used=" + std::to_string(used()));
            // if (used() <= 2) fatal("Insert while not enough reserved");
            uint64_t column = entry._column() & mask.value();
            size_type pos = Entry::_fast_hash(column, shift_);
            if (arena_[pos].unset()) {
                arena_[pos].entry = entry;
                // std::cout << "Insert " << arena_[pos] << " at " << (void *) this << "[" << pos << "] (try 1)\n";
                // std::cout << "Insert at DIB 1\n";
                ++size_;
                where = &arena_[pos];
                return false;
            }
            // Quadratic probing
            size_type add  = 1;
            size_type add2 = 2;
            while (true) {
                if ((arena_[pos].entry._column() & mask.value()) == column) {
                    // std::cout << "Clash " << arena_[pos] << " at " << (void *) this << "[" << pos << "] (try " << add2-1 << ") versus " << entry._column() << "\n";
                    // std::cout << "Clash at DIB " << add2-1 << "\n";
                    where = &arena_[pos];
                    return true;
                }
                pos = (pos + add) & mask_;
                if (arena_[pos].unset()) {
                    arena_[pos].entry = entry;
                    // std::cout << "Insert " << arena_[pos] << " at " << (void *) this << "[" << pos << "] (try " << add2 << ")\n";
                    // std::cout << "Insert at DIB " << add2 << "\n";
                    ++size_;
                    where = &arena_[pos];
                    return false;
                }
                add += add2;
                ++add2;
            }
        }
        // Normally find returns an iterator to the position or end()
        // Instead we return a direct pointer to the position or nullptr
        // (match based on only the Column bits without history and liberties)
        value_type* find(Entry entry) {
            // dump();
            size_type pos = entry.fast_hash(shift_);
            // std::cout << "Try pos " << pos << "\n";
            if (arena_[pos].unset()) {
                // std::cout << "Not found at DIB 1\n";
                return nullptr;
            }
            // Quadratic probing
            uint64_t column = entry.column();
            size_type add  = 1;
            size_type add2 = 2;
            while (true) {
                if (arena_[pos].entry.column() == column) {
                    // std::cout << "Found at DIB " << add2-1 << "\n";
                    return &arena_[pos];
                }
                pos = (pos + add) & mask_;
                // std::cout << "Retry pos " << pos << "\n";
                if (arena_[pos].unset()) {
                    // std::cout << "Not found at DIB " << add2 << "\n";
                    return nullptr;
                }
                add += add2;
                ++add2;
                // if (add2 > 10) fatal("Too much looping");
            }
        }
        // Same as normal find, but we find entries by only looking at the bits
        // determined by mask
        // (always use the same mask or no mask)
        value_type* find(Entry entry, CompressedColumn::Mask mask) {
            // dump();
            uint64_t column = entry._column() & mask.value();
            size_type pos = Entry::_fast_hash(column, shift_);
            // std::cout << "Try pos " << pos << "\n";
            if (arena_[pos].unset()) {
                // std::cout << "Not found at DIB 1\n";
                return nullptr;
            }
            // Quadratic probing
            size_type add  = 1;
            size_type add2 = 2;
            while (true) {
                if ((arena_[pos].entry._column() & mask.value()) == column) {
                    // std::cout << "Found at DIB " << add2-1 << "\n";
                    return &arena_[pos];
                }
                pos = (pos + add) & mask_;
                // std::cout << "Retry pos " << pos << "\n";
                if (arena_[pos].unset()) {
                    // std::cout << "Not found at DIB " << add2 << "\n";
                    return nullptr;
                }
                add += add2;
                ++add2;
                // if (add2 > 10) fatal("Too much looping");
            }
        }
      private:
        static constexpr uint8_t  UNSET8     = -1;
        static constexpr uint64_t UNSET      = -1;
        // Entry set just beyond the Entry array
        static constexpr uint64_t TERMINATOR =  0;

        value_type* arena_;	// Entry array
        size_type mask_;        // size of Entry array - 1
        size_type size_;        // Number of set entries in Entry array
        int shift_;		// Helper constant for fast_hash() so that
                                // the result falls exactly in [0, mask_+1[
    };

    typedef EntrySet::size_type size_type;
    typedef EntrySet::iterator iterator;
    typedef EntrySet::const_iterator const_iterator;

    // Data managed by each single thread
    class ThreadData {
      public:
        ThreadData();
        ThreadData(ThreadData&&) {
            // Needed because you can't simply resize a vector of objects
            // that contain a std::mutex
            // (because you cannot move a locked mutex)
            fatal("ThreadData is not designed to be movable");
        }
        auto& operator[](uint i) {
            return maps_[i];
        }
        auto const& operator[](uint i) const {
            return maps_[i];
        }
        auto  begin()       { return maps_.begin(); }
        auto  begin() const { return maps_.begin(); }
        auto  end  ()       { return maps_.end(); }
        auto  end  () const { return maps_.end(); }
	~ThreadData();
#ifdef CONDITION_VARIABLE
        void work_init() {
        }
        void work_finish() {
        }
#else  /* CONDITION_VARIABLE */
        void work_init() {
            work_mutex_.lock();
        }
        void work_finish() {
            work_mutex_.unlock();
        }
#endif /* CONDITION_VARIABLE */
        void alloc_arenas(EntrySet::value_type*& ptr, size_t max_map, size_t max_backbone) {
            maps_[0].alloc_arena(ptr, max_map);
            maps_[1].alloc_arena(ptr, max_map);
            maps_[2].alloc_arena(ptr, max_map);
            backbone_set.alloc_arena(ptr, max_backbone);
        }
      public:
        uint real_max CACHE_ALIGNED;
        uint new_max;
        uint new_min;
        int max_entries;
      private:
        std::array<EntrySet, 3> maps_;
      public:
        EntrySet backbone_set;
        Entry max_entry;
        int  max_index;
        uint64_t result;
        int filter, record;
#ifdef CONDITION_VARIABLE
        std::condition_variable work_condition_;
#endif /* CONDITION_VARIABLE */
        std::mutex work_mutex_;
        int operation_;
    };

    // Manage all threads
    class Threads {
      public:
        Threads(uint nr_threads, bool save_thread = true);
        ~Threads();
        auto nr_threads() const { return threads_data_.size(); }
        auto& operator[](uint i) {
            return threads_data_[i];
        }
        auto const& operator[](uint i) const {
            return threads_data_[i];
        }
        auto  begin()       { return threads_data_.begin(); }
        auto  begin() const { return threads_data_.begin(); }
        auto  end  ()       { return threads_data_.end(); }
        auto  end  () const { return threads_data_.end(); }

#ifdef CONDITION_VARIABLE
        // Get ready to send work to all threads
        void work_prepare() {
            std::unique_lock<std::mutex> lock{left_mutex_};
            left_waiting_ = 0;
        }
        // Thread with thread_data waits until work is sent to it
        void work_wait(ThreadData* thread_data) {
            std::unique_lock<std::mutex> lock{thread_data->work_mutex_};
            while (thread_data->operation_ == WAITING)
                thread_data->work_condition_.wait(lock);
        }
        // Wait for all threads to finish their work
        void work_done_wait() {
            std::unique_lock<std::mutex> lock{left_mutex_};
            while (!left_waiting_)
                left_condition_.wait(lock);
        }
        // Tell thread with thread_data to start its work
        void work_start(ThreadData& thread_data) {
            {
                std::unique_lock<std::mutex> lock{thread_data.work_mutex_};
                thread_data.operation_ = operation_;
            }
            thread_data.work_condition_.notify_one();
        }
        // Thread thread_data uses this to say it is done with its work
        void work_done(ThreadData* thread_data) {
            thread_data->operation_ = WAITING;
            if (DEBUG_THREAD)
                std::cout <<
                    "Thread " << thread_data - &threads_data_[0] <<
                    " Job finished\n";
            if (save_thread_)
                --threads_left_;
            else if (--threads_left_ == 0) {
                {
                    std::unique_lock<std::mutex> lock{left_mutex_};
                    left_waiting_ = 1;
                }
                left_condition_.notify_one();
            }
        }
#else  /* CONDITION_VARIABLE */
        void work_prepare() {}
        void work_wait(ThreadData* thread_data) {
            thread_data->work_mutex_.lock();
        }
        void work_done_wait() {
            left_mutex_.lock();
        }
        void work_start(ThreadData& thread_data) {
            thread_data.operation_ = operation_;
            thread_data.work_mutex_.unlock();
        }
        void work_done(ThreadData* thread_data) {
            thread_data->operation_ = WAITING;
            if (DEBUG_THREAD)
                std::cout <<
                    "Thread " << thread_data - &threads_data_[0] <<
                    " Job finished\n";
            if (save_thread_)
                --threads_left_;
            else if (--threads_left_ == 0)
                left_mutex_.unlock();
        }
#endif /* CONDITION_VARIABLE */
        // Go back to waiting
        void waiting() {
            operation_ = WAITING;
        }

        // Main loop for each thread (except the main thread)
        // wait for commands, execute them, wait again
        // exit if the command is finish()
        void thread_loop(CountLiberties* count_liberties, ThreadData* thread_data);
        // Used by threads to pick up one unique unit of work
        // The job is in some datastructure indexed by the result of this call
        // If the returned index is negatibe there is no more work and the
        // thread should go back to sleep
        int get_work() {
	    return --atop_;
	    // return atop_--;
	    // return atop_.fetch_sub(1, std::memory_order_relaxed);
	}
        // Start all needed threads and have them enter their thread_loop()
        void start(CountLiberties* count_liberties);
        // Start one operation. This is only meant for the "save_thread"
        void do_work(CountLiberties* countliberties, ThreadData& threads_data);
        // Next operation: calculate signatures
        void signature() {
            operation_ = SIGNATURE;
        }
        // Next operation: Move a bump down
        //  .X       .X
        //  X    to  .X
        //  X        X
        void call_down(uint pos) {
            pos_ = pos;
            operation_ = CALL_DOWN;
        }
        // Next operation: Move a bump up
        //  X        X
        //  X    to  .X
        //  .X       .X
        void call_up(uint pos) {
            pos_ = pos;
            operation_ = CALL_UP;
        }
        // Next operation: Move the final bump which is not in the middle
        // (even length columns
        //  .X       .X
        //  .X       .X
        //  X    to  .X
        //  .X       .X
        void call_asym_final(uint pos) {
            pos_ = pos;
            operation_ = CALL_ASYM_FINAL;
        }
        // Next operation: Move the final bump which is in the middle
        // (odd length columns
        //  .X       .X
        //  X    to  .X
        //  .X       .X
        void call_sym_final(uint pos) {
            pos_ = pos;
            operation_ = CALL_SYM_FINAL;
        }
        // Next operation: exit all threads
        void finish() {
            operation_ = FINISH;
        }
        uint save_execute(CountLiberties* count_liberties, uint ttop = 1) {
            auto tmp = save_thread_;
            auto op = operation_;
            save_thread_ = 1;
            auto rc = execute(count_liberties, ttop);
            operation_ = op;
            save_thread_ = tmp;
            return rc;
        }
        // Tell all threads to start working on the currently set operation
        // There are ttop new work units to be finished
        // Only start as many threads as needed if there is not enough work
        // If any thread had an exception rethrow it in the main thread
        // (if multiple threads die only the last exception is rembered)
        uint execute(CountLiberties* count_liberties, uint ttop) {
            if (ttop == 0) fatal("No work to start (operation " + std::to_string(operation_) + ")");
            uint threads = ttop < nr_threads() ? ttop : nr_threads();
            work_prepare();
            threads_left_ = threads;
            atop_ = ttop;
            if (DEBUG_THREAD)
                std::cout << "Execute, threads=" << threads << ", ttop=" << ttop << ", operation=" << operation_ << "\n";

            for (uint t = save_thread_; t < threads; ++t) {
                if (DEBUG_THREAD)
                    std::cout << "Notify thread " << t << "\n";
                work_start(threads_data_[t]);
            }
            if (save_thread_) {
                do_work(count_liberties, threads_data_[0]);

                // Do a busy wait. Since we got here we know there is no work
                // left so the other threads are at most finishing their last
                // work packet
                while (threads_left_);
            } else {
                if (DEBUG_THREAD)
                    std::cout << "Master thread starts to wait\n";
                work_done_wait();
            }
            waiting();
            if (has_eptr_) {
                std::cout << "rethrow exception\n";
                auto e = eptr_;
                eptr_ = nullptr;
                has_eptr_ = false;
                std::rethrow_exception(e);
            }
            if (DEBUG_THREAD)
                std::cout << "Executed\n";
            return threads;
        }
        void catch_exception();

      private:
        // Special exception a thread can throw to cause an immediate clean exit
        class finish_exception: public std::exception {
        };

        // Internal helper: this thread will now execute one operation
        // (Keep selecting one unit of work from the queue until no more work)
        void _do_work(CountLiberties* countliberties, ThreadData& threads_data);

        // Counter of work still te be done
        std::atomic_int atop_;
        // How many threads are left that still haven't returned to WAITING
        std::atomic_int threads_left_;
        // Per thread data
        std::vector<ThreadData> threads_data_;
        // Notice that threads_ can have one element less than threads_data_
        std::vector<std::thread> threads_;
        // Last not yet rethrown exception any thread had
        std::exception_ptr eptr_;
#ifdef CONDITION_VARIABLE
        // left_waiting_ == 1 indicates all threads are ready and waiting. The
        // variable is protected by left_mutex_ and signalled by left_condition_
        int left_waiting_;
        std::condition_variable left_condition_;
#endif /* CONDITION_VARIABLE */
        std::mutex left_mutex_;
        // Mutex so eptr_ is set by only one thread
        std::mutex eptr_mutex_;
        // The operation to be done next (WAITING if nothing to be done yet)
        int operation_;
        uint pos_;
        uint save_thread_;
        // This boolean exists because checking eptr_ directly is slow
        bool has_eptr_;
    };

    struct Args {
        EntrySet *map0, *map1;
        int pos;
        uint index0, rindex0;
        uint index1, rindex1;
        int filter, record;
        uint64_t old_min;
    };

    // A position on the board .(0, 0) is the top left
    class Coordinate {
      public:
        Coordinate(int x, int y) : x_{x}, y_{y} {}
        auto x() const { return x_; }
        auto y() const { return y_; }
        bool operator<(Coordinate const& rhs) const {
                  if (x() < rhs.x()) return true;
                  if (x() > rhs.x()) return false;
                  return y() < rhs.y();
        }
      private:
        int x_, y_;
    };

    // Memory usage by the current process in bytes
    static size_t get_memory();
    // Maximum board height we support
    static auto max_height() { return EXPANDED_SIZE; }

    CountLiberties(int height, uint nr_threads = 1, bool save_thread = true);
    ~CountLiberties();
    void clear();
    void clear_filter();
    auto reversed() const { return reversed_; }
    auto height() const { return height_; }
    auto target_width() const { return target_width_; }
    void target_width(int target_width);
    auto max_real_max() const { return max_real_max_; }
    auto real_max() const { return old_real_max_; }
    auto real_min() const { return old_min_+1; }
    auto max_size() const { return max_size_+1; }

    void sym_compress(CompressedColumn& compressed, int index, int rindex) const HOT;

    void expand(Column& column, CompressedColumn const& compressed, int from) const;
    // Return true if and only if "compressed" contains more than 1 chain
    // mask is a backbone_mask indicating which intersections are stones
    bool multichain(CompressedColumn const& compressed, CompressedColumn::Mask mask) const {
        return compressed.multichain(mask);
    }

    void insert(ThreadData& thread_data, EntrySet* map, Entry const entry) HOT;
    uint64_t signature() HOT;
    auto nr_classes() const { return nr_classes_; }
    auto nr_keys(int i) const {
        // std::cout << "    nr_keys(" << i << ")=" << entries_[i].size() << "\n";
        return entries_[i].size();
    }
    auto nr_keys() const {
        size_type size = 0;
        for (auto const& entries: entries_)
            size += entries.size();
        return size;
    }
    auto nr_keys_max() const {
        size_type size = 0;
        // We consider the injection entry seperately here
        for (auto const& entries: entries_)
            if (entries.size() > size) size = entries.size();
        return size;
    }
    auto nr_keys_min() const {
        // We combine the injection entry with entries_[0] here otherwise we
        // will always get 0 or 1
        size_type size = nr_keys(0) + entries_[nr_classes()].size();
        for (int i=1; i < nr_classes(); ++i)
            if (nr_keys(i) < size && nr_keys(i)) size = nr_keys(i);
        return size;
    }
    auto nr_classes_non_empty() const {
        int nr_classes_non_empty = nr_keys(0) + entries_[nr_classes()].size() ?
            1 : 0;
        for (int i=1; i < nr_classes(); ++i)
            if (nr_keys(i)) ++nr_classes_non_empty;
        return nr_classes_non_empty;
    }
    auto maximum_history(int bit) const {
        return max_entry_.history(bit);
    }
    auto maximum_column()  const { return max_index_; }
    auto maximum() const { return old_max_ - old_min_ + offset_; }
    auto no_solution() const { return old_max_ == 0; }
    auto _offset() const { return offset_ - old_min_; }
    auto  begin()       { return entries_.begin(); }
    auto  begin() const { return entries_.begin(); }
    auto  end  ()       { return entries_.end(); }
    auto  end  () const { return entries_.end(); }
    auto  index(EntryVector const& entries) const {
        return (&entries - &entries_[0]) & (nr_classes()-1);
    }
    auto filter(int x, int y) const { return filter_.at(x).at(y); }
    void filter(int x, int y, int filter) {
        if (filter_.at(x).at(y))
            throw std::logic_error("Filter already set");
        if (!filter)
            throw std::logic_error("Filter not being set");
        filter_[x][y] = filter;
        --filter_need_;
        if (COST) cost_propagate(x, y);
    }
    auto record_need() const { return filter_need_ - record_size(); }
    auto record_map(int x, int y) const { return record_map_.at(x).at(y); }
    void record_map(int x, int y, int record) { record_map_.at(x).at(y) = record; }
    auto record_map(Coordinate const& c) const {
        return record_map(c.x(), c.y());
    }
    void record_map(Coordinate const& c, int record) {
        record_map(c.x(), c.y(), record);
    }
    void record(int x, int y);
    uint record_last_column(int index, int* y0);
    void record_sort();
    void record_by_cost();
    void record_flush(Entry const& entry);
    void record_flush() { record_flush(max_entry_); }
    auto record_size() const -> size_t { return record_.size(); }
    auto const& records() const { return record_; }
    double cost(int pos);
    void cost(int pos, double cost);

    char to_string(char* result, Column const& column);
    std::string to_string(Column const& column);
    char column_string(char* result, CompressedColumn const& compressed, int from);
    std::string column_string(CompressedColumn const& compressed, int from);

    void new_round() HOT;
    int run_round(int x, int y) HOT;
    void call_signature(ThreadData& thread_data) HOT;
    void call_down(int pos, ThreadData& thread_data) HOT;
    void call_up(int pos, ThreadData& thread_data) HOT;
    void call_sym_final(int pos, ThreadData& thread_data) HOT;
    void call_asym_final(int pos, ThreadData& thread_data) HOT;

    void inject(int direction, Args args, ThreadData& thread_data, int index, int up) HOT;
    void process(int direction, Args const args, ThreadData& thread_data) HOT;
    void process_down(Args const args, ThreadData& thread_data) HOT;
    void process_up(Args const args, ThreadData& thread_data) HOT;
    void process_final(Args const args, ThreadData& thread_data) HOT;
    void process_asym(int direction, Args const args, ThreadData& thread_data) HOT;

    void map_load_factor     (float factor) {
        map_load_multiplier_ = 1. / factor;
    }
    void backbone_load_factor(float factor) {
        backbone_load_multiplier_ = 1. / factor;
    }
  private:
    void _call_asym(int direction, int pos, ThreadData& thread_data) HOT;

    void _process(bool inject, int direction, Args const args,
                  uint from, bool left_black, ThreadData& thread_data) HOT;
    void reserve_thread_maps(size_t max);
    template <class any>
    void map_reserve(EntrySet* set, any size) {
        set->reserve(size, map_load_multiplier_);
        if (UNLIKELY(set->used() > max_map_))
            fatal("map overflow used " + std::to_string(set->used()) + " > max_map " + std::to_string(max_map_));
    }
    template <class any>
    void backbone_reserve(EntrySet& set, any size) {
        set.reserve(size, backbone_load_multiplier_);
        if (UNLIKELY(set.used() > max_backbone_)) fatal("backbone overflow");
    }
    void entry_clear(int index) HOT {
        entry_clear(entries_[index]);
    }
    static void entry_clear(EntryVector& entries) HOT {
        entries.clear();
        entries.reserve(0);
        entries.shrink_to_fit();
    }
    void entry_transfer(ThreadData& thread_data, EntrySet* map, int index, uint pos, int up, int down) HOT;
    void cost_propagate(int x, int y) { cost_propagate(x * height() + y); }
    void cost_propagate(int pos);

    static uint8_t const bit_reverse_table_[256];
    static uint const start_table_[STATES];
    static Liberties const MAX_LIBERTIES = -1;
    static double const cost_divider;
    static double const cost_multiplier;

    // Stuff shared among threads
    int const height_ CACHE_ALIGNED;
    int const nr_classes_;
    std::vector<EntryVector> entries_;
    int* reverse_bits_ = nullptr;
    CompressedColumn::Mask* index_masks_ = nullptr;
    int* indices_ = nullptr;
    EntryVector entry00_;

    // Stuff not accessed from within a thread or constant during a thread
    Threads threads_;
    std::vector<std::vector<int>> filter_;
    std::vector<std::vector<int>> record_map_;
    std::vector<double> cost_;
    std::vector<Coordinate> record_;
    std::vector<uint64_t> full_liberties_;
    uint64_t current_full_liberties_;
    Entry full_entry_;
    Entry max_entry_;
    int offset_;		// Current Liberty renormalization
    int max_index_;
    uint old_real_max_;		// Use to detect risk of Liberty overflow
    uint new_real_max_;
    uint max_real_max_;
    uint old_max_;
    uint new_max_;
    // Used to renormalize the liberty counts so we don't overflow Liberty
    uint64_t old_min_;
    uint new_min_;
    uint filter_need_;
    int target_width_;
    int max_entries_ = 0;
    float map_load_multiplier_;
    float backbone_load_multiplier_;
    size_t max_map_;
    size_t max_backbone_;
    size_t threads_arena_map_ = 0;
    size_t threads_arena_backbone_ = 0;
    size_t threads_arena_allocated_ = 0;
    EntrySet::value_type* threads_arena_ = nullptr;
    size_t max_size_;
    // Reversed is a threads shared non atomic variable unprotected by any lock
    // We only ever will use it if there is only one column in the current
    // EntrySets, in which case only one thread will have updated it
    // Mutable since its just a logging variable that does not change the
    // logical object state and I don't want to declare rcompress() to not
    // be a const method
    mutable bool reversed_;

    // These mutable vectors don't really belong in the object and could be
    // allocated on stack in case of need. However to avoid repeated mallocs
    // and frees we put them inside the object
    // sizes_ should in principle be a size_t, but the data is spread over the
    // entries_ vector and bin size remains modest even for the largest board
    // size this program can hope to handle. Therefore an uint will be ok.
    // (saves some memory and makes the program about 1% faster)
    class Size {
      public:
        int  index = 0;
        uint size  = 0;
    };
    Size* sizes_ = nullptr;
    // Height  1: max_size=     4
    // Height  2: max_size=     7
    // Height  3: max_size=     5
    // Height  4: max_size=     7
    // Height  5: max_size=     8
    // Height  6: max_size=    11
    // Height  7: max_size=    14
    // Height  8: max_size=    27
    // Height  9: max_size=    29
    // Height 10: max_size=    72
    // Height 11: max_size=    82
    // Height 12: max_size=   234
    // Height 13: max_size=   250
    // Height 14: max_size=   762
    // Height 15: max_size=   767
    // Height 16: max_size=  2435
    // Height 17: max_size=  2464
    // Height 18: max_size=  7811
    // Height 19: max_size=  8156
    // Height 20: max_size= 27109
    // Height 21: max_size= 27977
    // Height 22: max_size= 93268
    // Height 23: max_size= 94567
    std::vector<int> indices0_;
};

/* ========================================================================= */

ALWAYS_INLINE
CountLiberties::CompressedColumn::Mask operator&(CountLiberties::CompressedColumn::Mask lhs, const CountLiberties::CompressedColumn::Mask& rhs) {
    return lhs &= rhs;
}

template <class any>
ALWAYS_INLINE
CountLiberties::CompressedColumn::Mask operator<<(CountLiberties::CompressedColumn::Mask lhs, any const& rhs) {
    return lhs <<= rhs;
}

template <class any>
ALWAYS_INLINE
CountLiberties::CompressedColumn::Mask operator>>(CountLiberties::CompressedColumn::Mask lhs, any const& rhs) {
    return lhs >>= rhs;
}

ALWAYS_INLINE
auto CountLiberties::CompressedColumn::Mask::black_mask() const {
    return Mask{mask_ & FILL_MULTIPLIER * BLACK };
}
ALWAYS_INLINE
auto CountLiberties::CompressedColumn::Mask::black_up_mask() const {
    return Mask{mask_ & FILL_MULTIPLIER * BLACK_UP };
}
ALWAYS_INLINE
auto CountLiberties::CompressedColumn::Mask::black_down_mask() const {
    return Mask{mask_ & FILL_MULTIPLIER * BLACK_DOWN };
}
ALWAYS_INLINE
auto CountLiberties::CompressedColumn::Mask::black_up_down_mask() const {
    // return Mask{mask_ & FILL_MULTIPLIER * BLACK_UP_DOWN };
    // All bits will be 1, so just pass on value
    // Not *exactly* the same. This doesn't mask the history/liberty bits
    // but these should not matter in any valid use of the Mask type
    return *this;
}

/* ========================================================================= */

uint8_t const CountLiberties::bit_reverse_table_[256] =
{
#   define R2(n)     n,     n + 2*64,     n + 1*64,     n + 3*64
#   define R4(n) R2(n), R2(n + 2*16), R2(n + 1*16), R2(n + 3*16)
#   define R6(n) R4(n), R4(n + 2*4 ), R4(n + 1*4 ), R4(n + 3*4 )
    R6(0), R6(2), R6(1), R6(3)
};

// Count  BLACK,BLACK_UP (since that combination is also usable as <= BLACK_UP)
uint const CountLiberties::start_table_[STATES] =
{ 1, 1, 0, 0, 0, 0, 0, 0 };

double const CountLiberties::cost_divider = 2.;
double const CountLiberties::cost_multiplier = 1. / cost_divider;

/* ========================================================================= */

CountLiberties::ThreadData::ThreadData() :
    operation_{WAITING}
{
    work_init();
}

// Not inline. We absolutely do not care about ThreadData destructor speed
CountLiberties::ThreadData::~ThreadData() {
    work_finish();
}

/* ========================================================================= */

ALWAYS_INLINE
char CountLiberties::Column::to_string(char* result, size_t height) const {
    char stack[EXPANDED_SIZE];
    char unused = 'A';
    int stack_top = -1;

    if (height > EXPANDED_SIZE)
        throw std::out_of_range("Column cannot be of that size");

    for (uint i = 0; i<height; ++i)
        switch(column_[i]) {
            case BLACK:
              result[i] = unused++;
              break;
            case BLACK_UP:
              if (stack_top < 0)
                  fatal("Invalid encoding value BLACK_UP in " + to_raw_string(height));
              result[i] = stack[stack_top--];
              break;
            case BLACK_DOWN:
              result[i] = stack[++stack_top] = unused++;
              break;
            case BLACK_UP_DOWN:
              if (stack_top < 0)
                  fatal("Invalid encoding value BLACK_UP_DOWN in " + to_raw_string(height));
              result[i] = stack[stack_top];
              break;
            case EMPTY:
              result[i] = '0';
              break;
            case LIBERTY:
              result[i] = '1';
              break;
            default:
              fatal("Invalid encoding value " + std::to_string(column_[i]) + " in " + to_raw_string(height));
        }
    if (UNLIKELY(stack_top != -1)) {
        std::stringstream ss;
        ss << "<";
        for (uint i = 0; i<height; ++i) ss << (uint) column_[i];
        ss << ">\n";

        fatal("Invalid encoded string " +
              to_raw_string(height) +
              " (unterminated chains)");
    }
    result[height] = 0;
    return unused;
}

std::string CountLiberties::Column::to_raw_string(size_t height) const {
    std::stringstream ss;

    ss << "<";
    for (size_t i = 0; i<height; ++i) ss << static_cast<uint>(column_[i]);
    ss << ">\n";
    return ss.str();
}

CountLiberties::Column::Column(char const* from, int height) {
    char unused = 'A';
    for (int i=0; i<height; ++i)
        switch(from[i]) {
            case '0':
              (*this)[i] = EMPTY;
              break;
            case '1':
              (*this)[i] = LIBERTY;
              break;
            default:
              if (from[i] < 'A' || from[i] > 'Z')
                  throw std::logic_error("Unknown character " + std::to_string(from[i]));
              State result = BLACK;
              if (from[i] > unused)
                  throw std::logic_error("Out of sequence character " + std::to_string(from[i]));

              if (from[i] == unused) ++unused;
              else result |= BLACK_UP;

              if (std::memchr(&from[i+1], from[i], height-1-i))
                  result |= BLACK_DOWN;

              (*this)[i] = result;
              break;
        }
}

/* ========================================================================= */

ALWAYS_INLINE
auto CountLiberties::CompressedColumn::any_empty(Mask index_mask, uint64_t mask, uint shift) const -> bool {
    index_mask |= mask;
    auto value = (_column() & ~index_mask.value()) >> shift;
    return value != 0;
}

ALWAYS_INLINE
auto CountLiberties::CompressedColumn::nr_empty(Mask index_mask, uint64_t mask, uint shift) const -> uint {
    index_mask |= mask;
#ifdef __POPCNT__
    auto value = (_column() & ~index_mask.value()) >> shift;
    return half_popcount64(value);
#else  /* __POPCNT__ */
    auto value = _column() & ~index_mask.value();
    value &= UINT64_C(0x5555555555555555) << shift;
    // Special implementation of popcount32 for our use case
    uint32_t v = value + (value >> 32);
    v  = (v & 0x33333333) + ((v >> 2) & 0x33333333);
    v  = (v + (v >> 4)) & 0xF0F0F0F;
    return v * 0x1010101 >> 24;
#endif  /* __POPCNT__ */
}

// Remove the down pointer to the current group looking up
void CountLiberties::CompressedColumn::_terminate_up(Mask down_mask, ColumnOnly value) {
    int depth = 0;
    auto value2 = value << 1;
    while (true) {
        down_mask >>= BITS_PER_VERTEX;
        if (value & down_mask) {
            // BLACK_DOWN or BLACK_UP_DOWN
            if (depth <= 0) {
                value_ &= ~down_mask.value();
                return;
            }
            if (!(value2 & down_mask))
                // BLACK_DOWN
                --depth;
        } else if (value2 & down_mask)
            // BLACK_UP
            ++depth;
    }
}

// Remove the up pointer to the current group looking down
void CountLiberties::CompressedColumn::_terminate_down(Mask up_mask, ColumnOnly value) {
    int depth = 0;
    auto value2 = value >> 1;
    while (true) {
        up_mask <<= BITS_PER_VERTEX;
        if (value & up_mask) {
            // BLACK_UP or BLACK_UP_DOWN
            if (depth <= 0) {
                value_ &= ~up_mask.value();
                return;
            }
            if (!(value2 & up_mask))
                // BLACK_UP
                --depth;
        } else if (value2 & up_mask)
            // BLACK_DOWN
            ++depth;
    }
}

// Go to the top of the current group and make it point up
void CountLiberties::CompressedColumn::_join_up(Mask stone_mask, ColumnOnly value) {
    int depth = 0;
    while (true) {
        stone_mask >>= BITS_PER_VERTEX;
        auto vertex = value & stone_mask;
        if (vertex.black_down_mask()) {
            if (!(vertex.black_up_mask())) {
                // BLACK_DOWN
                if (depth-- == 0) {
                    // set to BLACK_UP_DOWN
                    value_ |= stone_mask.value();
                    return;
                }
            }
        } else if (vertex.black_up_mask())
            // BLACK_UP
            ++depth;
    }
}

// Go to the bottom of the current group and make it point down
void CountLiberties::CompressedColumn::_join_down(Mask stone_mask, ColumnOnly value) {
    int depth = 0;
    while (true) {
        stone_mask <<= BITS_PER_VERTEX;
        auto vertex = value & stone_mask;
        if (vertex.black_up_mask()) {
            // BLACK_UP or BLACK_UP_DOWN
            if (!(vertex.black_down_mask())) {
                // BLACK_UP
                if (depth-- == 0) {
                    // set to BLACK_UP_DOWN
                    value_ |= stone_mask.value();
                    return;
                }
            }
        } else if (vertex.black_down_mask())
            // BLACK_DOWN
            ++depth;
    }
}

// Return true if and only if Column contains more than 1 chain
// mask is a backbone_mask indicating which intersections are stones
inline bool CountLiberties::CompressedColumn::multichain(Mask mask) const {
    // ~ column changes BLACK and BLACK_UP to 11 and 10 respectively
    // & 0xAAAAAAAAAAAAAAAA changes them both to 10 (and other blacks are 00)
    // & Mask::column_mask64 gets rid of history and liberties
    // & mask gets rid of EMPTY/LIBERTY
    uint64_t bits = (~_column() & (UINT64_C(0xAAAAAAAAAAAAAAAA) & Mask::column_mask64) & mask.value());
    // bits now contains as many 1 bits as there are chains
    // reverse power of 2 check (0 is considered a power of 2 which is wanted)
    return (bits & (bits - 1)) != 0;
}

std::string CountLiberties::CompressedColumn::raw_column_string() const {
    std::stringstream ss;

    ss << "{";
    auto value = column();
    for (size_t i = 0; i<length(); ++i) {
        if (i) ss << " ";
        for (int j=0; j < 4; ++j) {
            if (j) ss << ":";
            ss << (value & STONE_MASK);
            value >>= BITS_PER_VERTEX;
        }
    }
    ss << "}";
    return ss.str();
}

ALWAYS_INLINE
bool equal(CountLiberties::CompressedColumn const& lhs, CountLiberties::CompressedColumn const& rhs) {
    return lhs.column() == rhs.column();
}

ALWAYS_INLINE
bool _equal(CountLiberties::CompressedColumn const& lhs, CountLiberties::CompressedColumn const& rhs) {
    return lhs._column() == rhs._column();
}

ALWAYS_INLINE
bool less(CountLiberties::CompressedColumn const& lhs, CountLiberties::CompressedColumn const& rhs) {
    return lhs.column() < rhs.column();
}

ALWAYS_INLINE
bool _less(CountLiberties::CompressedColumn const& lhs, CountLiberties::CompressedColumn const& rhs) {
    return lhs._column() < rhs._column();
}

/* ========================================================================= */
std::string CountLiberties::Entry::history_bitstring() const {
    std::stringstream ss;
    for (int i=0; i<HISTORY_BITS; ++i) {
        if (i && i%8==0) ss << " ";
        ss << (history(i) ? 1 : 0);
    }
    return ss.str();
}

/* ========================================================================= */

CountLiberties::Threads::Threads(uint nr_threads, bool save_thread) :
    operation_{WAITING},
    save_thread_{save_thread ? 1U : 0U},
    has_eptr_{false}
{
    if (!atop_.is_lock_free())
        throw std::logic_error("std::atomic_int is not lockfree");
    if (nr_threads < 1)
        throw std::out_of_range("Need at least 1 thread");
    if (nr_threads > MAX_THREADS)
        throw std::out_of_range("Cannot use more than " + std::to_string(MAX_THREADS) + " threads");

    threads_data_.resize(nr_threads);

#ifdef CONDITION_VARIABLE
    left_waiting_ = 1;
#else  /* CONDITION_VARIABLE */
    left_mutex_.lock();
#endif  /* CONDITION_VARIABLE */
}

CountLiberties::Threads::~Threads() {
    if (threads_.size()) {
        finish();
        execute(nullptr, nr_threads());
        for (auto& thread: threads_)
            thread.join();
    }
#ifndef CONDITION_VARIABLE
    left_mutex_.unlock();
#endif  /* CONDITION_VARIABLE */
}

void CountLiberties::Threads::catch_exception() {
    std::cout << "Caught exception\n";
    std::unique_lock<std::mutex> lock(eptr_mutex_);
    eptr_ = std::current_exception();
    has_eptr_ = true;
}

ALWAYS_INLINE
void CountLiberties::Threads::_do_work(CountLiberties* count_liberties,
                                       ThreadData& thread_data) {
    switch(thread_data.operation_) {
        case CALL_DOWN:
          count_liberties->call_down(pos_, thread_data);
          break;
        case CALL_UP:
          count_liberties->call_up(pos_, thread_data);
          break;
        case CALL_ASYM_FINAL:
          count_liberties->call_asym_final(pos_, thread_data);
          break;
        case CALL_SYM_FINAL:
          count_liberties->call_sym_final(pos_, thread_data);
          break;
        case SIGNATURE:
          count_liberties->call_signature(thread_data);
          break;
        case FINISH:
          throw finish_exception();
          break;
        default:
          fatal("Unknown operation " + std::to_string(operation_));
    }
    if (DEBUG_THREAD)
        std::cout <<
            "Thread " << &thread_data - &threads_data_[0] <<
            " Job finished\n";
    work_done(&thread_data);
}

ALWAYS_INLINE
void CountLiberties::Threads::do_work(CountLiberties* count_liberties,
                                       ThreadData& thread_data) {
    thread_data.operation_ = operation_;
    try {
        _do_work(count_liberties, thread_data);
        return;
    } catch(finish_exception &e) {
        // Do nothing
    } catch(...) {
        catch_exception();
    }
    work_done(&thread_data);
}

void CountLiberties::Threads::thread_loop(CountLiberties* count_liberties, ThreadData* thread_data) {
    bool busy = true;

    if (DEBUG_THREAD)
        std::cout <<
            "Thread " << thread_data - &threads_data_[0] <<
            " start" << std::endl;

    while (busy) {
        try {
            while (true) {
                if (DEBUG_THREAD)
                    std::cout <<
                        "Thread " << thread_data - &threads_data_[0] <<
                        " goes into wait" << std::endl;
                work_wait(thread_data);
                if (DEBUG_THREAD)
                    std::cout <<
                        "Thread " << thread_data - &threads_data_[0] <<
                        " Wake " << thread_data->operation_ << "\n";
                _do_work(count_liberties, *thread_data);
            }
        } catch(finish_exception &e) {
            busy = false;
        } catch(...) {
            catch_exception();
        }
        if (DEBUG_THREAD)
            std::cout <<
                "Thread " << thread_data - &threads_data_[0] <<
                " Job finished\n";
        work_done(thread_data);
    }
}

inline void CountLiberties::Threads::start(CountLiberties* count_liberties) {
    for (uint t = save_thread_; t < nr_threads(); ++t)
        threads_.emplace_back(&CountLiberties::Threads::thread_loop, this, count_liberties, &threads_data_[t]);
    if (DEBUG_THREAD)
        std::cout << "Started\n";
}

/* ========================================================================= */

void CountLiberties::target_width(int target_width) {
    if (target_width <= 0)
        throw std::logic_error("target_width must be positive");
    // It's actually ok to have a target_width > height(), we just don't
    // use that functionality currently so trying it indicates a bug
    if (target_width > height())
        throw std::logic_error("target_width must not be above height (optional constraint)");

    target_width_ = target_width;

    if (COST) {
        int pos = target_width * height();
        cost_.resize(pos + height());
        for (int i=0; i < height(); ++i)
            cost_[pos+i] = 0.;
    }

    filter_.resize(target_width);
    for (auto& row: filter_)
        row.resize(height());

    record_map_.resize(target_width);
    for (auto& row: record_map_)
        row.resize(height());

    clear_filter();
}

void CountLiberties::record(int x, int y) {
    if (record_map(x, y) >= 0)
        throw std::logic_error("Duplicate record attempt");
    if (record_.size() >= HISTORY_BITS)
        throw std::logic_error("Record overflows HISTORY_BITS");
    if (record_.size() >= filter_need_)
        throw std::logic_error("Record overflows filter need");
    record_map(x, y, record_.size());
    record_.emplace_back(Coordinate{x, y});
}

uint CountLiberties::record_last_column(int index, int* y0) {
    int x   = target_width() - 1;
    int top = height()-1;
    uint hits = 0;
    for (int  low = 0, high = top;
         low <= high;
         ++low, --high) {
        int bit = index >> low & 1;
        int y = 2*low;
        if (bit == (index >> high & 1)) {
            if (!filter_[x][y]) {
                filter_[x][y] = bit ? 1 : -1;
                y0[hits++] = y;
                if (COST) cost_propagate(x, y);
            }
            if (low != high && !filter_[x][y+1]) {
                filter_[x][y+1] = bit ? 1 : -1;
                y0[hits++] = y+1;
                if (COST) cost_propagate(x, y+1);
            }
        } else if (!filter_[x][y] && filter_[x][y+1]) {
            // This currently never triggers due to our order
            filter_[x][y] = -filter_[x][y+1];
            y0[hits++] = y;
            if (COST) cost_propagate(x, y);
        } else if (filter_[x][y] && !filter_[x][y+1]) {
            // This very rarely triggers
            filter_[x][y+1] = -filter_[x][y];
            y0[hits++] = y+1;
            if (COST) cost_propagate(x, y+1);
        }
    }
    filter_need_ -= hits;
    return hits;
}

void CountLiberties::record_by_cost() {
    if (!COST) throw std::logic_error("Cost not implemented");

    while (record_size() < HISTORY_BITS && record_need()) {
        double sum = 0.;
        double max = -1.;
        int head = target_width() * height();
        int tail = head + height();
        int max_x0{0}, max_y0{0};
        for (int x0 = target_width()-1; x0 >= 0 ; --x0) {
            for (int y0 = height()-1; y0 >= 0; --y0) {
                --head;
                --tail;
                sum += cost_[head];
                sum -= cost_[tail];
                if (sum > max && record_map_[x0][y0] < 0 && !filter_[x0][y0]) {
                    max = sum;
                    max_x0  = x0;
                    max_y0  = y0;
                }
            }
        }
        if (max < 0.) fatal("No max");
        // print "Add record [$max_x0, $max_y0] at $max_pos [@nr_keys_guess]\n";
        record(max_x0, max_y0);
        cost_propagate(max_x0, max_y0);
    }
}

void CountLiberties::record_sort() {
    std::sort(record_.begin(), record_.end());
    int pos = 0;
    for (Coordinate const&c: record_) {
        record_map(c, pos);
        ++pos;
    }
}

void CountLiberties::record_flush(Entry const& entry) {
    int pos = 0;
    for (Coordinate const&c: record_) {
        record_map(c, -1);
        filter(c.x(), c.y(), entry.history(pos) ? 1 : -1);
        ++pos;
    }
    record_.clear();
}

double CountLiberties::cost(int pos) {
    if (!COST)
        throw std::logic_error("Cost not implemented");
    if (pos < 0)
        throw std::logic_error("cost index must not be negative");
    if (pos >= target_width() * height())
        throw std::logic_error("cost index too high");
    return cost_[pos];
}

void CountLiberties::cost(int pos, double cost) {
    if (!COST)
        throw std::logic_error("Cost not implemented");
    if (pos < 0)
        throw std::logic_error("cost index must not be negative");
    if (pos >= target_width() * height())
        throw std::logic_error("cost index too high");
    cost_[pos] = cost;
}

void CountLiberties::cost_propagate(int pos) {
    if (!COST) throw std::logic_error("Cost not implemented");

    for (int i = 0; i < height(); ++i)
        cost_[pos+i] *= cost_multiplier;
}

ALWAYS_INLINE
void CountLiberties::expand(Column& column,
                            CompressedColumn const& compressed, int from) const {
    compressed.expand(column, from, height());
}

// Recover a full column from a compressed column and a mask "from"
// "from" must have a 1 in each position that has a stone
// Height indicates how many fields we have to fill in
ALWAYS_INLINE
void CountLiberties::CompressedColumn::expand(Column& expanded,
                                              int from, int height) const {
    int from_mask = ~from << 2;
    auto value = column();
    for (int i = 0; i < height; ++i) {
        expanded[i] = (from_mask & 0x4) | (value & STONE_MASK);
        from_mask >>= 1;
        value >>= BITS_PER_VERTEX;
    }
}

// This is basically a bitwise reverse and compare of the result
// Caller already made sure that index >= rindex
void CountLiberties::sym_compress(CompressedColumn& compressed, int index, int rindex) const {
    int64_t value  = compressed.column();
    int64_t rvalue = 0;
    int compressed_height = (height()+3) / 4;
    for (int i=0; i < compressed_height; ++i) {
        rvalue = rvalue << 8 | bit_reverse_table_[value & 0xff];
        value >>= 8;
    }
    rvalue >>= (-height() & (8/BITS_PER_VERTEX-1)) * BITS_PER_VERTEX;
    if (index == rindex) {
        value = compressed.column();
        if (value <= rvalue) return;
    }
    compressed.column(rvalue);
    reversed_ = true;
}

ALWAYS_INLINE
void CountLiberties::insert(ThreadData& thread_data, EntrySet* map, Entry const entry) {
    // std::cout << "         Insert count " << (uint) entry.liberties() << "\n";
    // std::cout << "         Out: " << column_string(entry, index) << " -> " << (uint) entry.liberties() << "\n";

    EntrySet::value_type* result;
    if (map->insert(entry, result)) {
        // Already existed
        // std::cout << "           Already exists with count " << (uint) result->liberties() << "\n";
        if (entry.liberties() > result->entry.liberties())
          result->entry = entry;
    }
}

void CountLiberties::call_signature(ThreadData& thread_data) {
    auto* indices = &indices_[0];
    uint64_t signature = 0;
    while (true) {
        int i = threads_.get_work();
        if (i < 0) break;
        int index = indices[i];
        // std::cout << "  select " << index << "\n";

        for (auto const& entry : entries_[index]) {
            signature += entry.signature(old_max_, index);
        }
    }
    thread_data.result = signature;
}

auto CountLiberties::signature() -> uint64_t {
    threads_.signature();

    auto* indices0 = &indices0_[0];

    size_t max = nr_keys(0);
    auto* sizes = &sizes_[0];
    for (int i = 0; i <= max_entries_; ++i) {
        auto size = nr_keys(i);
        if (size > 0) {
            sizes->index = i;
            sizes->size  = size;
            ++sizes;
            if (size > max) {
                max = size;
                if (max >= indices0_.size()) {
                    indices0_.resize(2*max);
                    indices0 = &indices0_[0];
                }
            }
            ++indices0[size];
        }
    }

    uint64_t signature = 0;

    if (max) {
        // Process counting results to get a sorted list
        ++max;
        if (max > max_size_) max_size_ = max;
        // std::cout << "ttop=" << ttop << ", max=" << max << "\n";
        uint accu = 0;
        for (uint i=0; i < max; ++i) {
            auto tmp = indices0[i];
            indices0[i] = accu;
            accu += tmp;
        }
        auto* indices  = &indices_[0];
        for (auto s = &sizes_[0]; s < sizes; ++s)
            indices[indices0[s->size]++] = s->index;
        std::memset(indices0, 0, max*sizeof(indices0[0]));

        int ttop = sizes - &sizes_[0];
        if (0) {
            std::cout << "Signature=" << ttop << "non zero buckets\n";
            for (int i=0; i<ttop; ++i) {
                std::cout << "    index " << indices[i] << ": size " << nr_keys(i) << "\n";
            }
        }

        threads_.execute(this, ttop);

        for (auto const& thread_data: threads_)
            signature += thread_data.result;
    }
    // std::cout << "-> Sig " << signature << "\n";
    return signature;
}

void CountLiberties::entry_transfer(ThreadData& thread_data, EntrySet* map, int index, uint pos, int up, int down) {
    if (DEBUG_STORE)
        std::cout << "   Write entryset " << index << "\n";
    auto& entries	= entries_[index];
    if (map->empty()) {
        entry_clear(entries);
        if (DEBUG_STORE)
            std::cout << "   Close entryset " << index << "\n";
        return;
    }

    auto& backbone_set	= thread_data.backbone_set;
    auto index_mask	= index_masks_[index];

    entries.clear();
    entries.reserve(map->size());
    backbone_reserve(backbone_set, map->size());
    auto   up_mask = CompressedColumn::Mask::stone_mask(pos);
    auto down_mask = CompressedColumn::Mask::stone_mask(height() - 1 - pos);
    // std::cout << "map size=" << map->size() << "\n";

    // Maximum over all indices
    uint64_t full_liberties = current_full_liberties_;
    // std::cout << "Full: raw liberties=" << full_liberties << " (libs=" << full_liberties+offset_ << ")\n";
    for (auto const& element: *map) {
        auto entry = element.entry;
        uint64_t liberties{entry.liberties()};

        if (liberties <= full_liberties) {
            auto libs = liberties + entry.nr_empty(index_mask);
            // std::cout << "Entry " << column_string(entry, index) << " raw liberties " << liberties << ", libs=" << libs << "\n";
            if (libs < full_liberties) continue;
            if (libs == full_liberties && !equal(entry, full_entry_))
                continue;
        }

        if (up) {
            if (entry.test_vertex(up_mask)) {
                // EMPTY
                Entry probe{entry};
                probe.set_liberty(up_mask);	// Set LIBERTY
                auto found = map->find(probe);
                if (found)
                    if (liberties < found->entry.liberties()) {
                        // std::cout << "up 1 " << column_string(entry, index) << " raw libs=" << liberties << " pruned by " << column_string(found->entry, index) << " raw libs=" << found->entry.liberties() << std::endl;
                        continue;
                    }
            } else {
                // LIBERTY
                Entry probe{entry};
                probe.set_empty(up_mask);	// Set EMPTY
                auto found = map->find(probe);
                if (found)
                    if (liberties <= found->entry.liberties()) {
                        // std::cout << "up 2 " << column_string(entry, index) << " raw libs=" << liberties << " pruned by " << column_string(found->entry, index) << " raw libs=" << found->entry.liberties() << std::endl;
                        continue;
                    }
            }
        }
        if (down) {
            if (entry.test_vertex(down_mask)) {
                // EMPTY
                Entry probe{entry};
                probe.set_liberty(down_mask);	// Set LIBERTY
                auto found = map->find(probe);
                if (found)
                    if (liberties < found->entry.liberties()) {
                        // std::cout << "down 1 " << column_string(entry, index) << " raw libs=" << liberties << " pruned by " << column_string(found->entry, index) << " raw libs=" << found->entry.liberties() << std::endl;
                        continue;
                    }
            } else {
                // LIBERTY
                Entry probe{entry};
                probe.set_empty(down_mask);	// Set EMPTY
                auto found = map->find(probe);
                if (found)
                    if (liberties <= found->entry.liberties()) {
                        // std::cout << "down 2 " << column_string(entry, index) << " raw libs=" << liberties << " pruned by " << column_string(found->entry, index) << " raw libs=" << found->entry.liberties() << std::endl;
                        continue;
                    }
            }
        }

        //if (0)
        //    std::cout <<
        //        "I Entry: "     << column_string(entry,    index) <<
        //        ", Bacbone: " << column_string(backbone, index) <<
        //        " (raw liberties=" << backbone.liberties() << ")\n";
        EntrySet::value_type* result;

        if (backbone_set.insert(entry, result, index_mask)) {
            if (liberties > result->entry.liberties())
                result->entry = entry;
            else if (liberties == result->entry.liberties()) {
                auto new_empty = entry.nr_empty(index_mask);
                auto old_empty = result->entry.nr_empty(index_mask);
                if (new_empty > old_empty)
                    result->entry = entry;
                else if (new_empty == old_empty) {
                    // This shouldn't really matter too much But we want:
                    // - more canonical result not depending on hash order
                    // - very slight reduction of the probability of a reverse
                    // - possible better bit sharing with smaller boards
                    if (_less(entry, result->entry))
                        result->entry = entry;
                }
            }
        }

        entries.emplace_back(entry);
    }
    // std::cout << "backbone size=" << backbone_set.size() << "\n";

    // Maximum within this index
    Entry full = Entry::full(index_mask);
    auto found_full = backbone_set.find(full, index_mask);
    full_liberties = 0;
    if (found_full) {
        full = found_full->entry;
        full_liberties = full.liberties();
    }
    size_t nr_entries = 0;

    // The program logic ensures that entries is already cleared and shrunk
    uint64_t new_min = thread_data.new_min;
    uint64_t new_max = thread_data.new_max;
    if (backbone_set.size() == entries.size()) {
        // If the sizes are equal no backbone pruning can happen
        // But we can still get full based pruning
        for (auto const entry: entries) {
            uint64_t liberties{entry.liberties()};

            if (liberties <= full_liberties) {
                uint64_t nr_empty = entry.nr_empty(index_mask, full);
                if (liberties + nr_empty <= full_liberties &&
                    !equal(entry, full)) continue;
            }

            if (liberties < new_min) new_min = liberties;
            // std::cout << "           New\n";
            if (liberties > new_max) {
                if (liberties > thread_data.real_max)
                    thread_data.real_max = liberties;
                if (!multichain(entry, index_mask)) {
                    // If we get here there are NOT two or more different chains
                    // So no chains or 1 connected chain
                    new_max   = liberties;
                    thread_data.new_max   = new_max;
                    thread_data.max_entry = entry;
                    thread_data.max_index = index;
                    if (DEBUG_FLOW) {
                        std::cout << "            New maximum " << liberties + offset_ << " for '" << column_string(entry, index) << " (" << entry.history_bitstring() << ")\n";
                    }
                }
            }

            if (DEBUG_STORE)
                std::cout << "      Store " << column_string(entry, index) <<
                    " " << entry.raw_column_string() << " (" <<
                    entry.history_bitstring() << ", raw libs=" <<
                    liberties << ")\n";
            entries[nr_entries++] = entry;
        }
    } else {
        for (auto const entry: entries) {
            uint64_t liberties{entry.liberties()};
            //if (0)
            //    std::cout <<
            //        "O Entry: "     << column_string(entry,    index) <<
            //        ", Bacbone: " << column_string(backbone, index) <<
            //        " (raw liberties=" << backbone.liberties() << ")\n";

            if (liberties <= full_liberties) {
                uint64_t nr_empty = entry.nr_empty(index_mask, full);
                if (liberties + nr_empty <= full_liberties &&
                    !equal(entry, full)) continue;
            }

            auto found = backbone_set.find(entry, index_mask);
            // Interesting. Doing this test makes the program marginally faster
            if (!found) fatal("Did not find entry backbone");
            if (!equal(entry, found->entry)) {
                uint64_t max_liberties = found->entry.liberties();
                uint64_t nr_empty = entry.nr_empty(index_mask, found->entry);
                // std::cout << "nr_empty=" << nr_empty << "\n";
                // if (liberties + nr_empty <= max_liberties) continue;
                if (liberties + nr_empty <= max_liberties) {
                    if (liberties + nr_empty < max_liberties || nr_empty ||
                        found->entry.any_empty(index_mask, entry)) {
                        // std::cout << column_string(entry, index) << " raw libs=" << liberties << " pruned by " << column_string(found->entry, index) << " raw libs=" << max_liberties << " nr_empty=" << nr_empty << std::endl;
                        continue;
                    }
                }
            }

            if (liberties < new_min) new_min = liberties;
            // std::cout << "           New\n";
            if (liberties > new_max) {
                if (liberties > thread_data.real_max)
                    thread_data.real_max = liberties;
                if (!multichain(entry, index_mask)) {
                    // If we get here there are NOT two or more different chains
                    // So no chains or 1 connected chain
                    new_max   = liberties;
                    thread_data.new_max   = new_max;
                    thread_data.max_entry = entry;
                    thread_data.max_index = index;
                    if (DEBUG_FLOW) {
                        std::cout << "            New maximum " << liberties + offset_ << " for '" << column_string(entry, index) << " (" << entry.history_bitstring() << ")\n";
                    }
                }
            }

            if (DEBUG_STORE)
                std::cout << "      Store " << column_string(entry, index) <<
                    " " << entry.raw_column_string() << " (" <<
                    entry.history_bitstring() << ", raw libs=" <<
                    liberties << ")\n";
            entries[nr_entries++] = entry;
        }
    }
    entries.resize(nr_entries);
    entries.shrink_to_fit();
    thread_data.new_min = new_min;
    if (index > thread_data.max_entries) thread_data.max_entries = index;
    // std::cout << "entries size=" << entries.size() << "\n";

    map->clear();
    backbone_set.clear();
    // This is a slight speedup. Possibly because it does not delay fixing
    // up the TERMINATOR position (it's still in cache now)
    backbone_reserve(backbone_set, 0);
    // Shrink since we probably pruned
    if (DEBUG_STORE)
        std::cout << "   Close entryset " << index << "\n";
}

// inject() puts initial stones on an empty board. This is in principle needed
// since the program logic should distinguish between empty columns where a
// stone has never been placed and where it has. The program logic however does
// not distinguish these so it could be that we improperly prune an empty column
// that never had stones (if it ever had stones it will have more liberties so
// that one won't be pruned). To compensate for the possible loss of empty
// columns with no liberties this routine injects them back in.
// Running the program without this function and comparing the column checksums
// shows that the bad pruning never happens so this code is not actually needed.
// But I am unable to prove that this is so for column positions < 3. So I leave
// this method in. The slowdown this causes is extremely minor anyways
void CountLiberties::inject(int direction, Args args,
                            ThreadData& thread_data, int index, int up) {
    map_reserve(args.map1, 1 + entries_[index].size());
    // We could optimize this.
    // If there already is an entry in entries_[index] it will always prune the
    // entry created by inject EXCEPT on the second column along the sides
    // Since our caller however filters the left corners for height >= 3 even
    // this can only happen for height 1 and 2 boards. But there is at most 1
    // entry in entries_[index] anyways and this code never even triggers after
    // column 2. There is no point in making the code more fragile for a speedup
    // that can hardly even be measured for any interesting board size
    EntrySet::value_type* result;
    // std::cout << "Inject in:\n";
    for (auto entry: entries_[index]) {
        // std::cout << " Entry " << column_string(entry, index) << " raw libs=" << entry.liberties() << "\n";
        args.map1->insert(entry, result);
    }

    if      (direction < 0) _process(true, -1, args, 0, false, thread_data);
    else if (direction > 0) _process(true,  1, args, 0, false, thread_data);
    else                    _process(true,  0, args, 0, false, thread_data);
    entries_[nr_classes()].clear();
    if (entry00_.size()) {
        for (auto const& entry: entry00_) {
            // At most one entry so don't move stuff out of the loop
            uint liberties = entry.liberties();
            // liberties will always be 1, but let's not depend on the program logic
            if (liberties < thread_data.new_min) thread_data.new_min = liberties;
            if (liberties > thread_data.new_max) {
                thread_data.new_max = liberties;
                thread_data.max_entry = entry;
                thread_data.max_index = 0;
            }
        }
        entries_[nr_classes()].reserve(1);
        entries_[nr_classes()].swap(entry00_);
    }
    entry_transfer(thread_data, args.map1, index, args.pos, up, false);
    //std::cout << "Inject out:\n";
    //for (auto entry: entries_[index])
    //    std::cout << " Entry " << column_string(entry, index) << " raw libs=" << entry.liberties() << "\n";
    //std::cout << std::endl;
}

ALWAYS_INLINE
void CountLiberties::process(int direction, Args const args,
                             ThreadData& thread_data) {
    _process(false, direction, args, args.index0, false, thread_data);
    _process(false, direction, args, args.index1, true,  thread_data);
}

// Inline because it only has 1 call site
ALWAYS_INLINE
void CountLiberties::process_down(Args const args, ThreadData& thread_data) {
    process( 1, args, thread_data);
}

NOINLINE
void CountLiberties::process_up(Args const args, ThreadData& thread_data) {
    process(-1, args, thread_data);
}

NOINLINE
void CountLiberties::process_final(Args const args, ThreadData& thread_data) {
    process( 0, args, thread_data);
}

ALWAYS_INLINE
void CountLiberties::process_asym(int direction, Args const args, ThreadData& thread_data) {
    if (direction)
        process_up(args, thread_data);
    else
        process_final(args, thread_data);
}

// This is the core logic of the whole program.
// Add a bump in the given direction (0 combines two bumps to a flat column)
// Apply symmetry at the end (except when going down, direction > 0)
ALWAYS_INLINE
void CountLiberties::_process(bool inject, int direction, Args args,
                              uint from, bool left_black,
                              ThreadData& thread_data) {
#if NDEBUG
    // Make sure these tests get shortcircuited
    if (!__builtin_constant_p(inject))
        fatal("inlining did not make inject a constant");
    if (!__builtin_constant_p(direction))
        fatal("inlining did not make direction a constant");
    if (!__builtin_constant_p(left_black))
        fatal("inlining did not make left_black a constant");
#endif /* NDEBUG */

    // std::cout << "   From: " << from << "[[" << args.index0 << ", " << args.index1 << "], [" << args.rindex0 << ", " << args.rindex1 << "]] (final)\n";

    uint pos2 = CompressedColumn::Mask::stone_shift(args.pos);

    uint up_black, down_black, up_or_down_black;
    CompressedColumn::Mask up_mask, down_mask;
    bool liberty_prune = false;
    // Short circuit the test. compiler dead code elimination will do it for us
    if (true || direction >= 0) {
        // Make sure that args.pos == 0 works and results in up_black == 0
        // Should really be based on from, but args.index0 has the same bits
        // at position args.pos-1
        // up_black = (2*from >> args.pos) & 1;
        up_black = (2*args.index0 >> args.pos) & 1;

    	up_mask = up_black ?
            CompressedColumn::Mask::_stone_mask(pos2 - BITS_PER_VERTEX, BLACK_DOWN) :
            args.pos ? CompressedColumn::Mask::_stone_mask(pos2 - BITS_PER_VERTEX) : CompressedColumn::Mask::_stone_mask(0, 0);
        // auto const up_black_down	= up_mask & BLACK_DOWN_MASK;
        // Test should be against (from & 0x2), but args.index0 has the same bit
        // Need height != 2 otherwise the reverses make both sides impossible
        if (PRUNE_SIDES && direction > 0 && args.pos == 0 && height() != 2) {
            // std::cout << "Hit up" << std::endl;
             liberty_prune = true;
            if (left_black) args.filter = -1;
        }
    }

    // Short circuit the test. compiler dead code elimination will do it for us
    if (true || direction <= 0) {
        // Should really be based on from, but args.index0 has the same bits
        // at position args.pos+1
        // down_black = (from >> args.pos) & 2;
        down_black = (args.index0 >> args.pos) & 2;

        // We need to test args.pos. Normally you would think that if too big
        // the stone mask will shift out. However with the barrel shifter of
        // modern CPUs it will actually not shift at all
        down_mask	= down_black ?
            CompressedColumn::Mask::_stone_mask(pos2 + BITS_PER_VERTEX, BLACK_UP) :
            args.pos < EXPANDED_SIZE-1 ?
                       CompressedColumn::Mask::_stone_mask(pos2 + BITS_PER_VERTEX) : CompressedColumn::Mask::_stone_mask(0,0);
        // auto const down_black_up	= down_mask & BLACK_UP_MASK;
        // No need to check for height != 2 since that never has direction < 0
        if (PRUNE_SIDES && direction < 0 && args.pos == height()-1) {
            liberty_prune = true;
            // std::cout << "Hit down" << std::endl;
            if (left_black) args.filter = -1;
        }
    }

    // Short circuit the test. compiler dead code elimination will do it for us
    if (true || direction == 0) up_or_down_black = up_black | down_black;

    bool sym0 = direction <= 0 && args.index0 >= args.rindex0;
    bool sym1 = direction <= 0 && args.index1 >= args.rindex1;

    auto const stone_mask	= CompressedColumn::Mask::_stone_mask(pos2);
    auto const black_up		= stone_mask.black_up_mask();
    auto const black_down	= stone_mask.black_down_mask();
    auto const black_up_down	= stone_mask; // stone_mask & BLACK_UP_DOWN_MASK

    // index_mask should be based on from, but except at the current position
    // args.index0 has the same bits and we will never look at the different bit
    // auto index_mask	= index_masks_[from];
    auto index_mask	= index_masks_[args.index0];

    if (DEBUG_FETCH) std::cout << "   Read entryset " << from << "\n";

    // std::cout << "\tentries " << from << " size " << entries_[inject ? nr_classes() : from].size() << "\n";
    for (auto entry: entries_[inject ? nr_classes() : from]) {
        if (DEBUG_FETCH) std::cout << "      Entry: " << entry.raw_column_string() << ", raw libs=" << static_cast<uint>(entry.liberties()) << "\n";
        entry.liberties_subtract(args.old_min);
        if (DEBUG_FLOW) {
            std::cout << "      " << (inject ? "Inject" : "In") << ": '" <<
                column_string(entry, from) << "' -> " <<
                entry.liberties(offset_) <<
                " dir " << direction << " (" <<
                entry.history_bitstring() << ")\n";
        }

        auto left = entry._get_vertex(pos2);
        // std::cout << "left=" << left << ", left_black = " << left_black << "\n";

        // Set empty
        if (args.filter <= 0) {
            Entry result = entry;
            if (left_black) {
                if (left == BLACK) {
                    // We just lost a chain. In general we don't accept
                    // disconnection. Losing the last chain is ok however
                    // as long as we won't accept putting a new stone later
                    // (except if no stones at all have been added yet)
                    if (args.index0) goto BLACK_STONE;
                } else if (left == BLACK_UP) {
                    result._terminate_up(black_down, result.test_vertex(index_mask));
                } else if (left == BLACK_DOWN) {
                    result._terminate_down(black_up, result.test_vertex(index_mask));
                }
                // BLACK_UP_DOWN stays connected so nothing to do

                result.set_liberty(stone_mask);	// sets LIBERTY
                result.liberties_add(1);
            } else if (direction >  0 ?   up_black :
                       direction <  0 ? down_black :
                       up_or_down_black) {
                result.set_liberty(stone_mask);	// sets LIBERTY
                result.liberties_add(1);
            } else {
                // left >= EMPTY, up >= EMPTY, down >= EMPTY
                result.set_empty(stone_mask);	// sets EMPTY
            }

            if (sym0) sym_compress(result, args.index0, args.rindex0);
            // The history map is initialized with zeroes so record0 is a no op
            // result.record0(args.record);
            if (DEBUG_FLOW) {
                std::cout << "         Empty: '" <<
                    column_string(result, sym0 ? args.rindex0 : args.index0) <<
                    "' -> " << result.liberties(offset_) << " (" <<
                    result.history_bitstring() << ") set=" << (sym0 ? args.rindex0 : args.index0) << "\n";
            }
            if (inject) entry00_.emplace_back(result);
            else insert(thread_data, args.map0, result);
        }

      BLACK_STONE:
        // Set black

        // Avoid complete disconnect. Notice that we used to also test
        // for libs + offset_ here so we would keep empty columns were no
        // stone was ever placed. These however get added back by inject()
        if (inject || from) {
            if (args.filter < 0) continue;

            Entry& result = entry;

            // nogain should be eliminated by the compiler unless direction == 0
            int nogain = 3;
            if (!left_black) {
                if (left != (LIBERTY & STONE_MASK)) {
                    // EMPTY
                    // Set to LIBERTY,except that we will immediately set BLACK
                    // result.set_liberty(stone_mask);
                    result.set_black(stone_mask);	// sets BLACK
                    result.liberties_add(1);
                } else {
                    // LIBERTY
                    if (direction != 0 && liberty_prune) continue;
                    if (direction == 0 && !inject) --nogain;
                    // We don't need to set to BLACK since LIBERTY and BLACK
                    // use the same bits
                    // result.set_black(stone_mask);	// sets BLACK
                }
                left = BLACK;
            }

            if (direction >= 0) {
                // std::cout << "up=" << up << ", up_black = " << up_black << "\n";
                if (up_black) {
                    // Join
                    auto up = entry.test_vertex(up_mask);
                    if (up) {
                        if (left & BLACK_UP) {
                            // They were already connected, so we just created
                            // a loop. No new connection bits need to be set.
                            // We can prune the loop because it is never
                            // optimal, though the program would soon discover
                            // this for itself
                            if (PRUNE_LOOPS) continue;
                        } else {
                            result.add_direction(black_up_down);
                            if (left & BLACK_DOWN)
                                result._join_down(stone_mask, result.test_vertex(index_mask));
                            left = BLACK_UP_DOWN;
                        }
                    } else {
                        // up |= BLACK_DOWN
                        result.add_direction(up_mask);
                        if (left & BLACK_UP) {
                            result._join_up(stone_mask, result.test_vertex(index_mask));
                        } else {
                            left |= BLACK_UP;
                            result.add_direction(black_up);
                        }
                    }
                } else {
                    auto up = entry.test_vertex(up_mask);
                    if (up) {
                        // We made sure up is 0 if pos is too small
                        // up = LIBERTY
                        result.set_liberty(up_mask);
                        result.liberties_add(1);
                    } else {
                        // LIBERTY
                        if (direction == 0 && !inject) --nogain;
                    }
                }
            }

            if (direction <= 0) {
                // std::cout << "down=" << down << ", down_black = " << down_black << "\n";
                if (down_black) {
                    // Join
                    auto down = entry.test_vertex(down_mask);
                    if (down) {
                        if (left & BLACK_DOWN) {
                            // They were already connected, so we just created
                            // a loop. No new connection bits need to be set.
                            // We can prune the loop because it is never
                            // optimal, though the program would soon discover
                            // this for itself
                            if (PRUNE_LOOPS) continue;
                        } else {
                            result.add_direction(black_up_down);
                            if (left & BLACK_UP)
                                result._join_up(stone_mask, result.test_vertex(index_mask));
                            // left = BLACK_UP_DOWN;
                        }
                    } else {
                        // down |= BLACK_UP
                        result.add_direction(down_mask);
                        if (left & BLACK_DOWN) {
                            result._join_down(stone_mask, result.test_vertex(index_mask));
                        } else {
                            // left |= BLACK_DOWN;
                            result.add_direction(black_down);
                        }
                    }
                } else {
                    auto down = entry.test_vertex(down_mask);
                    if (down) {
                        // We made sure down is 0 if pos is too big
                        // down = LIBERTY
                        result.set_liberty(down_mask);
                        result.liberties_add(1);
                    } else {
                        // LIBERTY
                        if (direction == 0 && !inject) --nogain;
                    }
                }
            }

            if (direction == 0 && !inject && !left_black && nogain == 0)
                continue;

            if (sym1) sym_compress(result, args.index1, args.rindex1);

            result.record1(args.record);

            if (DEBUG_FLOW) {
                std::cout << "         Black: '" <<
                    column_string(result, sym1 ? args.rindex1 : args.index1) <<
                    "' -> " << result.liberties(offset_) << " (" <<
                    result.history_bitstring() << ") set=" << (sym1 ? args.rindex1 : args.index1) << "\n";
            }
            insert(thread_data, args.map1, result);
        }
    }
    // This would be the place to clear the entry vector
    // But we delay that to entry_transfer so the reuse there will be faster
    // if (!inject) entry_clear(from);
}

void CountLiberties::call_down(int pos, ThreadData& thread_data) {
    uint bits  = 1 << pos;

    auto map0 = &thread_data[0];
    auto map1 = &thread_data[1];

    Args args;
    args.map0    = map0;
    args.map1    = map1;
    args.filter  = thread_data.filter;
    args.record  = thread_data.record;
    args.old_min = old_min_;
    args.pos     = pos;
    args.index0  = -1;

    while (true) {
        int i = threads_.get_work();
        if (i < 0) break;
        int j = indices_[i];

        // std::cout << "call_down\n";
        args.index0  = j;
        args.index1  = j+bits;
        args.rindex0 = j;
        args.rindex1 = j+bits;

        size_t grow = entries_[j].size() + entries_[j+bits].size();
        map_reserve(map0, grow);
        map_reserve(map1, grow);

        process_down(args, thread_data);

        // If neighbour the given position is a guaranteed LIBERTY
        // Consider only the up direction since the bump is not down yet
        int neighbour = ~(j << 1) & bits;
        entry_transfer(thread_data, map0, j,      pos, neighbour, false);
        entry_transfer(thread_data, map1, j+bits, pos, false,     false);
    }
    if (args.index0 == 0 && entries_[nr_classes()].size())
        inject(1, args, thread_data, bits, false);
    // std::cout << "end" << std::endl;
}

void CountLiberties::call_sym_final(int pos, ThreadData& thread_data) {
    uint bits  = 1 << pos;

    auto map0 = &thread_data[0];
    auto map1 = &thread_data[1];

    Args args;
    args.map0    = map0;
    args.map1    = map1;
    args.filter  = thread_data.filter;
    args.record  = thread_data.record;
    args.old_min = old_min_;
    args.pos     = pos;
    args.index0  = -1;

    while (true) {
        int i = threads_.get_work();
        if (i < 0) break;
        int j  = indices_[i];
        int rj = reverse_bits_[j];

        // std::cout << "call_sym_final\n";
        assert(j <= rj);
        // Normally we would need to distinguish between j == rj or not
        // in the j != rj case we would have to worry about reversing
        // However sym_final is only ever called from a symmetrized position
        // so the extra case simply does not happen. The assert checks this
        assert(j == rj || entries_[rj].size() == 0);
        assert(j == rj || entries_[rj+bits].size() == 0);

        args.index0  = j;
        args.index1  = j+bits;
        args.rindex0 = rj;
        args.rindex1 = rj+bits;

        size_t grow = entries_[j].size() + entries_[j+bits].size();
        map_reserve(map0, grow);
        map_reserve(map1, grow);

        process_final(args, thread_data);

        // Consider both directions since the bump is getting straightened here.
        int neighbours = ~(j << 1 | j >> 1) & bits;
        entry_transfer(thread_data, map0, j,       pos, neighbours, false);
        entry_transfer(thread_data, map1, j+bits,  pos, false,      false);
        // rj and rj + bits are already empty (for j != rj) so no clear needed
        // if (j != rj) {
        //     entry_clear(rj);
        //     entry_clear(rj+bits);
        // }
    }
    if (args.index0 == 0 && entries_[nr_classes()].size())
        inject(0, args, thread_data, bits, false);
    // std::cout << "end" << std::endl;
}

ALWAYS_INLINE
void CountLiberties::_call_asym(int direction, int pos, ThreadData& thread_data) {
    uint  bits = 1 << pos;
    uint rbits = 1 << (height() - 1 - pos);
    uint cbits = bits + rbits;

    assert(bits > rbits);

    auto map0 = &thread_data[0];
    auto map1 = &thread_data[1];
    auto map2 = &thread_data[2];

    Args args;
    args.filter  = thread_data.filter;
    args.record  = thread_data.record;
    args.old_min = old_min_;
    args.pos     = pos;
    args.index0  = -1;

    while (true) {
        int i = threads_.get_work();
        if (i < 0) break;
        int j  = indices_[i];
        int rj = reverse_bits_[j];

        if (j == rj) {
            // std::cout << "call_asym j==rj, direction=" << direction << "\n";

            size_t grow1 = entries_[j].size() + entries_[j+bits].size();
            size_t grow2 = entries_[j+rbits].size() + entries_[j+cbits].size();
            map_reserve(map1, grow1 + grow2);
            map_reserve(map0, grow1);

            args.map0    = map0;
            args.map1    = map1;
            args.index0  = j;
            args.index1  = j+bits;
            args.rindex0 = rj;
            args.rindex1 = rj+rbits;

            process_asym(direction, args, thread_data);

            int neighbour = ~(j >> 1) & bits;
            entry_transfer(thread_data, map0, j, pos, neighbour, neighbour);

            map_reserve(map0, grow2);

            args.map0    = map1;
            args.map1    = map0;
            args.index0  = j+rbits;
            args.index1  = j+cbits;
            args.rindex0 = rj+bits;
            args.rindex1 = rj+cbits;
            process_asym(direction, args, thread_data);

            // The j + bits map can never get filled since rj + bits is smaller
            entry_clear(j+bits);
            entry_transfer(thread_data, map1, j+rbits, pos,  neighbour & direction, false);
        } else {
            // std::cout << "call_asym j!=rj, direction=" << direction << "\n";
            assert(j < rj);
            // These two can never have gotten filled during the previous
            // call_down round (call_down only changes bits at the top half
            // of a column, that is the low bits)
            assert(entries_[rj+bits].size() == 0);
            assert(entries_[rj+cbits].size() == 0);

            // One of j+bits and rj+rbits will be unused
            // size_t grow3 = entries_[rj+rbits].size() + entries_[rj+cbits].size();
            size_t grow3 = entries_[rj+rbits].size();
            size_t grow4 = entries_[j+rbits].size() + entries_[j+cbits].size();
            // size_t grow2 = entries_[rj].size() + entries_[rj+bits].size();
            size_t grow2 = entries_[rj].size();
            map_reserve(map1, grow2+grow4);
            size_t grow1 = entries_[j].size() + entries_[j+bits].size();
            map_reserve(map2, grow1+grow3);
            map_reserve(map0, grow1+grow2);

            args.map0    = map0;
            args.map1    = map2;
            args.index0  = j;
            args.index1  = j+bits;
            args.rindex0 = rj;
            args.rindex1 = rj+rbits;
            process_asym(direction, args, thread_data);

            if (grow2) {
                args.map0    = map0;
                args.map1    = map1;
                args.index0  = rj;
                args.index1  = rj+bits;
                args.rindex0 = j;
                args.rindex1 = j+rbits;
                process_asym(direction, args, thread_data);
            }

            int   up_neighbour = ~(j >> 1) &  bits;
            int down_neighbour = ~(j << 1) & rbits;
            entry_transfer(thread_data,  map0, j, pos,  up_neighbour,  down_neighbour);
            // The rj map can never get filled since j is smaller
            entry_clear(rj);

            map_reserve(map0, grow3+grow4);

            if (grow3) {
                args.map0    = map2;
                args.map1    = map0;
                args.index0  = rj+rbits;
                args.index1  = rj+cbits;
                args.rindex0 = j+bits;
                args.rindex1 = j+cbits;
                process_asym(direction, args, thread_data);
            }

            if (j + bits <= rj+rbits) {
                assert(j+bits < rj+rbits);
                entry_clear(rj+rbits);
                entry_transfer(thread_data,  map2, j+bits,  pos, false,  down_neighbour & direction);
            } else {
                entry_clear(j+bits);
                entry_transfer(thread_data, map2, rj+rbits, pos,  down_neighbour & direction, false);
            }

            args.map0    = map1;
            args.map1    = map0;
            args.index0  = j+rbits;
            args.index1  = j+cbits;
            args.rindex0 = rj+bits;
            args.rindex1 = rj+cbits;
            process_asym(direction, args, thread_data);

            entry_transfer(thread_data,  map1, j+rbits,      pos,  up_neighbour & direction, false);
            // The rj+bits map can never get filled since j+rbits is smaller
            entry_clear(rj+bits);
            // The rj+cbits map can never get filled since j+cbits is smaller
            entry_clear(rj+cbits);
        }
        entry_transfer(thread_data, map0, j+cbits, pos, false, false);
    }
    if (args.index0 == rbits && entries_[nr_classes()].size()) {
        args.map0    = map0;
        args.map1    = map1;
        args.index0  = 0;
        args.index1  = bits;
        args.rindex0 = 0;
        args.rindex1 = rbits;

        inject(direction, args, thread_data, rbits, bits & direction);
    }
    // std::cout << "end" << std::endl;
}

void CountLiberties::call_up(int pos, ThreadData& thread_data) {
    _call_asym(-1, pos, thread_data);
}

void CountLiberties::call_asym_final(int pos, ThreadData& thread_data) {
    _call_asym(0, pos, thread_data);
}

void CountLiberties::reserve_thread_maps(size_t max) {
    if (max) {
        --max;
        max_map_ = max * map_load_multiplier_;
        // We must have at least 1 empty to prevent find() from looping
        if (max_map_ < max) max_map_ = max;
        assert(max_map_ > 0);
        // Set all bits after the first one
        max_map_ = (static_cast<size_t>(0) - 1) >> clz(max_map_);
        ++max_map_;

        max_backbone_ = max * backbone_load_multiplier_;
        // We must have at least 1 empty to prevent find() from looping
        if (max_backbone_ < max) max_backbone_ = max;
        assert(max_backbone_ > 0);
        // Set all bits after the first one
        max_backbone_ = (static_cast<size_t>(0) - 1) >> clz(max_backbone_);
        ++max_backbone_;
        ++max;
    } else {
        max_map_      = 0;
        max_backbone_ = 0;
    }
    size_t size_map = ((max_map_+1) * sizeof(EntrySet::value_type) + _CACHE_LINE -1) / _CACHE_LINE * _CACHE_LINE;
    size_t size_backbone = ((max_backbone_+1) * sizeof(EntrySet::value_type) + _CACHE_LINE -1) / _CACHE_LINE * _CACHE_LINE;
    size_t needed = (3*size_map + size_backbone) * threads_.nr_threads();
    // std::cout << "max=" << max << " (3*" << size_map << " + " << size_backbone << ") * " << threads_.nr_threads() << " = " << needed << "\n";

    // Check if the currently assigned areas are big enough
    if (max_map_      <= threads_arena_map_ &&
        max_backbone_ <= threads_arena_backbone_) return;

    // std::cout << "max_map_=" << max_map_ << ",max_backbone_=" << max_backbone_ << "\n";
    if (needed > threads_arena_allocated_) {
        if (ARENA_MALLOC) {
            free(threads_arena_);
        } else
            delete[] threads_arena_;
        threads_arena_ = nullptr;
        if (false) {
            // Go a factor 2 over to avoid many small increases
            max_map_      *= 2;
            max_backbone_ *= 2;
            size_t size_map = ((max_map_+1) * sizeof(EntrySet::value_type) + _CACHE_LINE -1) / _CACHE_LINE * _CACHE_LINE;
            size_t size_backbone = ((max_backbone_+1) * sizeof(EntrySet::value_type) + _CACHE_LINE -1) / _CACHE_LINE * _CACHE_LINE;
            needed = (3*size_map + size_backbone) * threads_.nr_threads();
        }
        struct alignas(_CACHE_LINE) Dummy {
            EntrySet::value_type dummy[_CACHE_LINE / sizeof(EntrySet::value_type)];
        };
        // std::cout << "(3*" << size_map << " + " << size_backbone << ") * " << threads_.nr_threads() << " = " << needed << "\n";
        if (ARENA_MALLOC) {
            threads_arena_allocated_ = (needed + ARENA_ALIGNMENT - 1) / ARENA_ALIGNMENT * ARENA_ALIGNMENT;
            threads_arena_ = reinterpret_cast<EntrySet::value_type *>(aligned_alloc(ARENA_ALIGNMENT, threads_arena_allocated_));
            if (!threads_arena_) throw std::bad_alloc();
        } else {
            threads_arena_allocated_ = needed;
            threads_arena_ = reinterpret_cast<EntrySet::value_type *>(new Dummy[threads_arena_allocated_ / sizeof(Dummy)]);
        }
        // std::cout << "allocated " << threads_arena_ << ", size " << threads_arena_allocated_ << " bytes\n";
    }

    auto ptr = threads_arena_;

    for (auto& thread: threads_)
        thread.alloc_arenas(ptr, max_map_, max_backbone_);
    std::memset(threads_arena_, -1, (ptr - threads_arena_) * sizeof(EntrySet::value_type));

    threads_arena_map_      = max_map_;
    threads_arena_backbone_ = max_backbone_;
    // std::cout << "Used " << ptr - threads_arena_  << " bytes\n";
}

int CountLiberties::run_round(int x, int pos) {
    int filter = x < target_width() ? filter_[x].at(pos) :  0;
    int record = x < target_width() ? record_map_[x].at(pos) : -1;

    for (auto& thread_data: threads_) {
        thread_data.max_entry = Entry::invalid();
        thread_data.real_max = new_real_max_;
        thread_data.new_max  = new_max_;
        thread_data.filter   = filter;
        thread_data.record   = record;
        thread_data.new_min  = new_min_;
        thread_data.max_entries = 0;
    }

    auto* sizes    = &sizes_[0];
    auto* indices0 = &indices0_[0];

    //for (int j=0; j<nr_classes(); ++j) {
    //    std::cout << "From keys(" << j << ")=" << nr_keys(j) << "\n";
    //}

    reversed_ = false;

    bool const final = pos == height()-1;
    int i = pos >> 1;
    int pos_left, bits, rbits;
    size_t max = 0;
    int limit = max_entries_;
    if ((pos & 1) == 0 && !final) {
        // even, work from top down
        pos_left = i;

        threads_.call_down(pos_left);

        bits  = 1 << pos_left;
        rbits = -1;

        if (limit & bits) limit = (limit & ~bits) | (bits-1);
        for (int j=0; j<=limit; ++j) {
            if (j & bits) continue;
            auto size = nr_keys(j) + nr_keys(j + bits);
            if (size) {
                sizes->index = j;
                sizes->size  = size;
                ++sizes;
                if (size > max) {
                    max = size;
                    if (max >= indices0_.size()) {
                        indices0_.resize(2*max);
                        indices0 = &indices0_[0];
                    }
                }
                ++indices0[size];
            }
        }
        limit = (nr_classes() - 1) & ~bits;
    } else {
        // odd, work from bottom up
        pos_left = height()-1-i;

        bits  = 1 << pos_left;
        rbits = 1 << i;
        int cbits = bits | rbits;

        if (final) {
            if (bits == rbits)
                threads_.call_sym_final(pos_left);
            else {
                assert(bits > rbits);
                threads_.call_asym_final(pos_left);
            }
        } else {
            assert(bits > rbits);
            threads_.call_up(pos_left);
        }

        auto const* reverse_bits = &reverse_bits_[0];
        if (limit & bits)  limit = (limit & ~ bits) |  (bits-1);
        if (limit & rbits) limit = (limit & ~rbits) | (rbits-1);
        for (int j=0; j<=limit; ++j) {
            if (j & cbits) continue;
            int rj = reverse_bits[j];
            if (j > rj) continue;
            size_t size = nr_keys(j) + nr_keys(j+bits);
            if (bits != rbits)
                size += nr_keys(j+rbits) + nr_keys(j+cbits);
            if (j != rj) {
                size += nr_keys(rj) + nr_keys(rj+bits);
                if (bits != rbits)
                    size += nr_keys(rj+rbits) + nr_keys(rj+cbits);
            }
            if (size) {
                sizes->index = j;
                sizes->size  = size;
                ++sizes;
                if (size > max) {
                    max = size;
                    if (max >= indices0_.size()) {
                        indices0_.resize(2*max);
                        indices0 = &indices0_[0];
                    }
                }
                ++indices0[size];
            }
        }
        limit = (nr_classes() - 1) & ~cbits;
    }

    // Turn off injection for column positions >= 3
    // (counting from 1, x counts from 0, so the test is x >= 2)
    // It's easy to prove that if there is a solution where the first stone is
    // in column 3, there is at least as good a solution with a stone in column
    // 2, so after this point we don't need to inject empty columns anymore
    // Easier way to see it: A full column on column 2 after an empty on column
    // 1 already has height() liberties. Empty up to column 2 can at most equal
    // that
    if (entries_[nr_classes()].size()) {
        if (x >= 2) entries_[nr_classes()].clear();
        else {
            if (sizes > sizes_&& sizes_[0].index == 0) {
                // Move index 0 to the start of the sizes array
                if (sizes_[0].size >= max) {
                    max = sizes_[0].size + 1;
                    if (max >= indices0_.size()) {
                        indices0_.resize(2*max);
                        indices0 = &indices0_[0];
                    }
                }
                --indices0[sizes_[0].size];
                sizes_[0].size = 0;
            } else {
                // There is no index 0 yet. Create one
                sizes->index = 0;
                sizes->size  = 0;
                ++sizes;
                // No need to resize indices0, it starts at size 100
                if (max <= 0) max = 1;
            }
            ++indices0[0];
        }
    }

    int ttop = sizes - &sizes_[0];
    if (ttop == 0) fatal("No work");

    if (false) {
        std::cout <<
            "Unsorted Width=" << nr_classes() <<
            ", ttop=" << ttop <<
            ", bits=" << bits << ", rbits=" << rbits <<
            ", full=" << limit << ",x=" << x << "\n";
        for (auto s = &sizes_[0]; s < sizes; ++s)
            std::cout << "    index " << s->index << ": size " << s->size << "\n";
    }

    // We could tighten max_map_ and max_backbone_ by a factor of 2 or so for
    // the common case of call_asym j!=rj
    // Even though inject is already accounted for in max in the actual code
    // we always grow the maps that CAN be injected by 1, so we need a +1
    reserve_thread_maps(max+1);

    size_t vertex = x * height() + pos+1;
    current_full_liberties_ = full_liberties_[vertex-1];
    if (ttop && sizes[-1].index == limit) {
        if (current_full_liberties_) {
            if (current_full_liberties_ > offset_ + UINT64_C(1))
                current_full_liberties_ = current_full_liberties_ - (offset_ +1);
            else
                current_full_liberties_ = 0;
        }
        // Execute the full column outside any threads
        // This entry can never be very big
        // (trivially <= 32, in reality probably <= 8)
        --ttop;
        --sizes;
        --indices0[sizes->size];
        indices_[0] = limit;
        threads_.save_execute(this);

        // Find the number of liberties of the fully connected colum (if any)
        // current_full_liberties_ = 0;
        // In principle we should set current_full_liberties_ 0 in case
        // there is no full column. But the PRUNE_SIDES option will quite often
        // prune a bumpy full column. We can however safely keep the current
        // value as a cutoff since liberties can only go up and any column that
        // loses enough EMPTYs to hit the cutoff did not actually reach its
        // potential and should have been pruned (though we didn't know it at
        // the time)
        // A few columns that could have been pruned in the just executed group
        // might slip through since we processed the group with the old
        // current_full_liberties. They never seem to grow to a big set so
        // it's not worth killing them
        current_full_liberties_ = full_liberties_[vertex-1];
        size_t full_index = nr_classes()-1;
        for (auto const& entry: entries_[full_index]) {
            // If there are bumps the column can be disconnected
            if (equal(entry, full_entry_)) {
                // +1 makes sure full_liberties > 0 even if real liberties == 0
                // so we can distinguish it from when no full entry exists
                current_full_liberties_ = entry.liberties() + (offset_+1);
                break;
            }
        }
    }
    if (vertex >= full_liberties_.size()) {
        full_liberties_.resize(vertex);
        full_liberties_.emplace_back(current_full_liberties_);
    } else if (current_full_liberties_ < full_liberties_[vertex])
        current_full_liberties_ = full_liberties_[vertex];
    // std::cout << "Set full to " << current_full_liberties_-1 << " liberties" << ", offset=" << offset_ << std::endl;
    if (current_full_liberties_) {
        if (current_full_liberties_ > offset_ + UINT64_C(1))
            current_full_liberties_ = current_full_liberties_ - (offset_ +1);
        else
            current_full_liberties_ = 0;
    }

    // Process counting results to get a sorted list
    // I suspect that at a big enough problem size max will start growing
    // with an exponent above 2 and counting sort will start losing to a plain
    // sort on sizes_. However for any problem sizes we can realistically handle
    // on current computers max is pretty restricted (e.g. it is only 14814 for
    // a 19x19 board and the growth factor is still below 2).
    ++max;
    if (max > max_size_) {
        //    std::cout << "max_max=" << max_max << std::endl;
        max_size_ = max;
    }
    // std::cout << "ttop=" << ttop << ", max=" << max << "\n";

    uint accu = 0;
    for (EntryVector::size_type i=0; i < max; ++i) {
        auto tmp = indices0[i];
        indices0[i] = accu;
        accu += tmp;
    }

    auto* indices  = &indices_[0];
    for (auto s = &sizes_[0]; s < sizes; ++s)
        indices[indices0[s->size]++] = s->index;
    std::memset(indices0, 0, max*sizeof(indices0[0]));

    if (false) {
        std::cout <<
            "Sorted Width=" << nr_classes() <<
            ", ttop=" << ttop <<
            ", bits=" << bits << ", rbits=" << rbits <<
            ", full=" << limit << ", x=" << x << "\n";
        auto i = ttop;
        while (--i >= 0)
            std::cout << "    index " << indices[i] << "\n";
    }

    uint threads = ttop ? threads_.execute(this, ttop) : 1;

    max_entries_ = 0;
    uint t_max = threads;
    for (uint t=0; t<threads; ++t) {
        if (threads_[t].new_max > new_max_) {
            new_max_ = threads_[t].new_max;
            t_max = t;
        }

        if (threads_[t].real_max > new_real_max_) {
            new_real_max_ = threads_[t].real_max;
            if (new_real_max_ > max_real_max_) max_real_max_ = new_real_max_;
        }

        if (threads_[t].new_min < new_min_)
            new_min_ = threads_[t].new_min;

        if (threads_[t].max_entries > max_entries_)
            max_entries_ = threads_[t].max_entries;
    }
    if (t_max < threads) {
        max_index_ = threads_[t_max].max_index;
        max_entry_ = threads_[t_max].max_entry;
    }

    if (DEBUG_FLOW) {
        std::cout << "   Final maximum " << new_max_ + offset_ << " for '" << column_string(max_entry_, max_index_) << " (" << max_entry_.history_bitstring() << ")\n";
    }

    if (DEBUG_FLOW || DEBUG_STORE || DEBUG_FETCH || DEBUG_THREAD)
        std::cout << std::flush;

    new_round();

    return pos_left;
}

// Memory usage by the current process in bytes
auto CountLiberties::get_memory() -> size_t {
    // This is linux specific,
    // but on non-linux it shouldn't hurt, just won't get any result
    std::ifstream statm;
    // statm.exceptions( std::ios::failbit );
    statm.open("/proc/self/statm");
    size_t mem = 0;
    statm >> mem;
    if (mem) return mem * PAGE_SIZE;

#ifdef JEMALLOC
    // If we're not on linux try to get the jemalloc idea of memory
    // Will be a reasonable but too low value.
    // E.g. 555M might be reported as 450M (why? diff seems more than code size)
    // Also notice that this query is pretty slow. It noticably slows down the
    // whole program!
    size_t len = sizeof(mem);
    mallctl("stats.mapped", &mem, &len, nullptr, 0);
    uint64_t epoch;
    len = sizeof(epoch);
    mallctl("epoch", nullptr, nullptr, &epoch, len);
#else  /* JEMALLOC */
# ifdef TCMALLOC
    // If we're not on linux try to get the tcmalloc idea of memory
    // Will be a reasonable but too low value.
    // E.g. 583M might be reported as 477M (why? diff seems more than code size)
    MallocExtension::instance()->GetNumericProperty("generic.heap_size", &mem);
# endif /* TCMALLOC */
#endif /* JEMALLOC */
    // If neither linux nor compiled with jemalloc or tcmalloc we have no idea and return 0
    return mem;
}

char CountLiberties::to_string(char* result, Column const& column) {
    return column.to_string(result, height());
}

std::string CountLiberties::to_string(Column const& column) {
    char buffer[EXPANDED_SIZE+1];
    column.to_string(buffer, height());
    return std::string(buffer, height());
}

char CountLiberties::column_string(char* result, CompressedColumn const& compressed, int from) {
    Column column;
    expand(column, compressed, from);
    return to_string(result, column);
}

std::string CountLiberties::column_string(CompressedColumn const& compressed, int from) {
    char buffer[EXPANDED_SIZE+1];
    column_string(buffer, compressed, from);
    return std::string(buffer, height());
}

void CountLiberties::new_round() {
    // No max is only a problem on the last round, so check that external
    // if (new_max_ == 0)
    //    fatal("No maximum");

    if (new_min_ == MAX_LIBERTIES)
        fatal("minimum is still maxed out. Probably means no entries");
    if (new_min_ <= 0)
        fatal("Some entry has 0 liberty offset. However, all entries should have an offset of at least 1");
    old_min_ = new_min_-1;
    offset_ += old_min_;
    // std::cout << "offset=" << offset_ << ", libs in [" << static_cast<uint>(old_min_+1) << ", " << static_cast<uint>(new_real_max_) << "]\n";
    new_min_ = MAX_LIBERTIES;

    // An extra -1 could be needed here because we use -1 as UNSET in EntrySet
    // so we could get to such a value if the number of liberties gets to 255
    // Possible if the colum is completely filled with a combination EMPTY and
    // BLACK_UP_DOWN. If any BLACK_UP_DOWN there must be at least one BLACK_UP
    // and BLACK_DOWN, so that cannot happen. If pure EMPTY then the last set
    // must have been an EMPTY and there was actually no growth. So not
    // subtracting an extra 1 is safe (a debug build checks this on insert())
    if (new_real_max_ - old_min_ >= MAX_LIBERTIES - MAX_GROWTH)
        throw std::logic_error("Liberty range is growing too large (" + std::to_string(new_real_max_ - old_min_) + "). Unable to renormalize");

    old_max_  = new_max_;
    new_max_  = 0;
    old_real_max_ = new_real_max_;
    new_real_max_ = 0;
}

void CountLiberties::clear() {
    max_size_ = 0;
    for (auto& entry: entries_)
        entry.clear();
    record_.clear();

    // Notice we do NOT clear the filter since we probably want to run again
    // using the new filter bits. To clear the filter call clear_filter()

    // We also do NOT clear the full_column_ vector since with the extra filter
    // the full column can have less liberties than the real full column without
    // filtering. This could then start pruning real solutions

    offset_   = -1;
    new_max_  =  0;
    old_min_  =  0;
    new_real_max_ = 0;
    max_real_max_ = 0;
    new_min_ = MAX_LIBERTIES;

    current_full_liberties_ = 0;

    Entry entry;
    entry.clear(-offset_);
    entries_[nr_classes()].reserve(1);
    entries_[nr_classes()].emplace_back(entry);

    if (INITIAL_INSERT) {
        // Initial insert. Conceptually not needed since inject() will put
        // initial stones. But without this run_round will see all entries as
        // empty and not even get going
        threads_[0].new_min  = new_min_;
        threads_[0].new_max  = new_max_;
        threads_[0].real_max = 0;
        auto map0 = &threads_[0][0];
        insert(threads_[0], map0, entry);
        new_min_   = threads_[0].new_min;
        new_max_   = threads_[0].new_max;
        max_index_ = threads_[0].max_index;
        max_entry_ = threads_[0].max_entry;
        entry_transfer(threads_[0], map0, 0, 0, false, false);

        // no need to initialize most old_ variables. new_round() will set them
        new_round();
    } else {
        old_max_  = 0;
        old_real_max_ = 0;
    }
}

void CountLiberties::clear_filter() {
    for (auto& row: filter_)
        for (auto& elem: row)
            elem = 0;

    for (auto& row: record_map_)
        for (auto& elem: row)
            elem = -1;

    filter_need_ = height() * target_width();
}

CountLiberties::CountLiberties(int height, uint nr_threads, bool save_thread) :
    height_{height},
    nr_classes_{1 << height_},
    threads_{nr_threads, save_thread},
    map_load_multiplier_{1. / MAP_LOAD_FACTOR},
    backbone_load_multiplier_{1. / BACKBONE_LOAD_FACTOR}
{
    // std::cout << "height=" << height_ << "\n";
    if (height > EXPANDED_SIZE)
        throw std::out_of_range
            ("Height " + std::to_string(height) +
             " is bigger than " + std::to_string(EXPANDED_SIZE));
    if (height < 0)
        throw std::out_of_range
            ("Height " + std::to_string(height) + " is below 0");

    reverse_bits_ = new int[nr_classes()];
    index_masks_  = new CompressedColumn::Mask[nr_classes()];
    for (int i = 0; i < nr_classes(); ++i) {
        int bits = i;
        int reverse = 0;
        for (int j=0; j<height_;++j) {
            reverse = 2 * reverse + (bits & 1);
            bits /= 2;
        }
        reverse_bits_[i] = reverse;
        index_masks_[i]  = CompressedColumn::Mask::backbone_mask(i);
    }
    target_width(height_);
    full_entry_ = Entry::full(index_masks_[nr_classes()-1]);

    entry00_.reserve(1);
    // One extra to hold the empty column injector
    entries_.resize(nr_classes()+1);
    sizes_   = new Size[nr_classes()];
    indices_ = new int [nr_classes()];
    // 100 is an arbitrary starting point to the exponential resizes
    // Avoid the need of many small initial steps before serious progress
    indices0_.resize(100);
    full_liberties_.emplace_back(0);

    clear();

    // reserve_thread_maps(28106);
    threads_.start(this);
}

CountLiberties::~CountLiberties() {
    if (ARENA_MALLOC) {
        free(threads_arena_);
    } else
        delete[] threads_arena_;
    delete[] indices_;
    delete[] sizes_;
    delete[] index_masks_;
    delete[] reverse_bits_;
}

class je_malloc_stats {
  public:
    uint length;
    char buffer[65536];
};

void je_malloc_stats_cb(void *closure, const char *out) {
    je_malloc_stats* result = static_cast<je_malloc_stats*>(closure);
    size_t len = strlen(out);
    if (len > sizeof(result->buffer) - result->length)
        len = sizeof(result->buffer) - result->length;
    std::memcpy(result->buffer+result->length, out, len);
    result->length += len;
}

/* ========================================================================= */

/* Duplicate from perl source (since it's not exported unfortunately) */
STATIC bool my_isa_lookup(pTHX_ HV *stash, const char *name, HV* name_stash,
                          int len, int level) COLD;
STATIC bool my_isa_lookup(pTHX_ HV *stash, const char *name, HV* name_stash,
                          int len, int level) {
    AV* av;
    GV* gv;
    GV** gvp;
    HV* hv = Nullhv;
    SV* subgen = Nullsv;

    /* A stash/class can go by many names (ie. User == main::User), so
       we compare the stash itself just in case */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnonnull"
    if ((name_stash && stash == name_stash) ||
        strEQ(HvENAME(stash), name) ||
        strEQ(name, "UNIVERSAL")) return TRUE;
#pragma GCC diagnostic pop

    if (level > 100) croak("Recursive inheritance detected in package '%s'",
                           HvENAME(stash));

    gvp = (GV**)hv_fetch(stash, "::ISA::CACHE::", 14, FALSE);

    if (gvp && (gv = *gvp) != (GV*)&PL_sv_undef && (subgen = GvSV(gv)) &&
        (hv = GvHV(gv))) {
        if (SvIV(subgen) == (IV)PL_sub_generation) {
            SV* sv;
            SV** svp = (SV**)hv_fetch(hv, name, len, FALSE);
            if (svp && (sv = *svp) != (SV*)&PL_sv_undef) {
                DEBUG_o( Perl_deb(aTHX_ "Using cached ISA %s for package %s\n",
                                  name, HvENAME(stash)) );
                return sv == &PL_sv_yes;
            }
        } else {
            DEBUG_o( Perl_deb(aTHX_ "ISA Cache in package %s is stale\n",
                              HvENAME(stash)) );
            hv_clear(hv);
            sv_setiv(subgen, PL_sub_generation);
        }
    }

    gvp = (GV**)hv_fetch(stash,"ISA",3,FALSE);

    if (gvp && (gv = *gvp) != (GV*)&PL_sv_undef && (av = GvAV(gv))) {
	if (!hv || !subgen) {
	    gvp = (GV**)hv_fetch(stash, "::ISA::CACHE::", 14, TRUE);

	    gv = *gvp;

	    if (SvTYPE(gv) != SVt_PVGV)
		gv_init(gv, stash, "::ISA::CACHE::", 14, TRUE);

	    if (!hv)
		hv = GvHVn(gv);
	    if (!subgen) {
		subgen = newSViv(PL_sub_generation);
		GvSV(gv) = subgen;
	    }
	}
	if (hv) {
	    SV** svp = AvARRAY(av);
	    /* NOTE: No support for tied ISA */
	    I32 items = AvFILLp(av) + 1;
	    while (items--) {
		SV* sv = *svp++;
		HV* basestash = gv_stashsv(sv, FALSE);
		if (!basestash) {
		    if (ckWARN(WARN_MISC))
			Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
                                    "Can't locate package %" SVf " for @%s::ISA",
                                    sv, HvENAME(stash));
		    continue;
		}
		if (my_isa_lookup(aTHX_ basestash, name, name_stash,
                                  len, level + 1)) {
		    (void)hv_store(hv,name,len,&PL_sv_yes,0);
		    return TRUE;
		}
	    }
	    (void)hv_store(hv,name,len,&PL_sv_no,0);
	}
    }
    return FALSE;
}

#define C_COUNT_LIBERTIES(object, context) c_CountLiberties(aTHX_ object, context)

STATIC CountLiberties* c_CountLiberties(pTHX_ SV *object, const char *context);
STATIC CountLiberties* c_CountLiberties(pTHX_ SV *object, const char *context) {
    SV *sv;
    HV *stash, *class_stash;
    IV address;

    if (MAGIC) SvGETMAGIC(object);
    if (!SvROK(object)) {
        if (SvOK(object)) croak("%s is not a reference", context);
        croak("%s is undefined", context);
    }
    sv = SvRV(object);
    if (!SvOBJECT(sv)) croak("%s is not an object reference", context);
    stash = SvSTASH(sv);
    /* Is the next even possible ? */
    if (!stash) croak("%s is not a typed reference", context);
    class_stash = gv_stashpv("Go::CountLiberties", FALSE);
    if (!my_isa_lookup(aTHX_ stash, "Go::CountLiberties", class_stash, 18, 0))
        croak("%s is not a Go::CountLiberties reference", context);
    address = SvIV(sv);
    if (!address)
        croak("Go::CountLiberties object %s has a nullptr pointer", context);
    return INT2PTR(CountLiberties*, address);
}

MODULE = Go::CountLiberties		PACKAGE = Go::CountLiberties
PROTOTYPES: DISABLE

SV *
new(char *class_name, unsigned int height, unsigned int nr_threads = 1, bool save_thread = true)
  PREINIT:
    CountLiberties* counter;
  CODE:
    RETVAL = newSV(0);
    try {
        counter = new CountLiberties(height, nr_threads, save_thread);
    } catch(std::exception &e) {
        croak("new: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }
    sv_setref_pv(RETVAL, class_name, counter);
  OUTPUT:
    RETVAL

void
CountLiberties::DESTROY()

void
CountLiberties::clear()

void
CountLiberties::clear_filter()

unsigned int
CountLiberties::height()

int
CountLiberties::nr_classes()

int
CountLiberties::nr_classes_non_empty()

UV
CountLiberties::nr_keys()

UV
CountLiberties::nr_keys_min()

UV
CountLiberties::nr_keys_max()

void
CountLiberties::keys()
  PPCODE:
    uint nr = THIS->nr_keys();
    if (GIMME_V == G_ARRAY) {
        uint height = THIS->height();
        EXTEND(SP, nr);
        for (auto const& entries: *THIS) {
            int index = THIS->index(entries);
            for (auto const& entry: entries) {
                SV* pv = newSV(height);
                sv_2mortal(pv);
                SvUPGRADE(pv, SVt_PV);
                SvPOK_on(pv);
                THIS->column_string(SvPVX(pv), entry, index);
                SvCUR(pv) = height;
                PUSHs(pv);
            }
        }
    } else if (GIMME_V == G_SCALAR) {
        dXSTARG;
        PUSHu(nr);
    }

void
CountLiberties::values()
  PPCODE:
    try {
        uint nr = THIS->nr_keys();
        if (GIMME_V == G_ARRAY) {
            uint offset = THIS->_offset();
            EXTEND(SP, nr);
            for (auto const& entries: *THIS) {
                for (auto const& entry: entries) {
                    mPUSHu(entry.liberties(offset));
                }
            }
        } else if (GIMME_V == G_SCALAR) {
            dXSTARG;
            PUSHu(nr);
        }
    } catch(std::exception &e) {
        std::cout << std::flush;
        croak("entries: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }

void
CountLiberties::entries(bool extended = 0)
  PPCODE:
    try {
        uint nr = THIS->nr_keys();
        if (GIMME_V == G_ARRAY) {
            uint height = THIS->height();
            uint offset = THIS->_offset();
            EXTEND(SP, nr);

            for (auto const& entries: *THIS) {
                int index = THIS->index(entries);
                for (auto const& entry: entries) {
                    AV* av = newAV();
                    SV* rv = newRV_noinc((SV *) av);
                    sv_2mortal(rv);
                    PUSHs(rv);

                    av_extend(av, extended ? 2 : 1);

                    SV* pv = newSV(height);
                    av_push(av, pv);
                    SvUPGRADE(pv, SVt_PV);
                    SvPOK_on(pv);
                    uint8_t unused = THIS->column_string(SvPVX(pv), entry, index);
                    SvCUR(pv) = height;

                    av_push(av, newSVuv(entry.liberties(offset)));

                    if (extended) {
                        SV* pv = newSV(1);
                        av_push(av, pv);
                        SvUPGRADE(pv, SVt_PV);
                        SvPOK_on(pv);
                        SvPVX(pv)[0] = unused;
                        SvCUR(pv) = 1;
                    }
                }
            }
        } else if (GIMME_V == G_SCALAR) {
            dXSTARG;
            PUSHu(nr);
        }
    } catch(std::exception &e) {
        std::cout << std::flush;
        croak("entries: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }

void
CountLiberties::key_values()
  PPCODE:
    try {
        uint nr = THIS->nr_keys();
        if (GIMME_V == G_ARRAY) {
            uint height = THIS->height();
            uint offset = THIS->_offset();
            EXTEND(SP, nr*2);

            for (auto const& entries: *THIS) {
                int index = THIS->index(entries);
                for (auto const& entry: entries) {
                    SV* pv = newSV(height);
                    sv_2mortal(pv);
                    SvUPGRADE(pv, SVt_PV);
                    SvPOK_on(pv);
                    THIS->column_string(SvPVX(pv), entry, index);
                    SvCUR(pv) = height;

                    PUSHs(pv);
                    mPUSHu(entry.liberties(offset));
                }
            }
        } else if (GIMME_V == G_SCALAR) {
            dXSTARG;
            PUSHu(nr);
        }
    } catch(std::exception &e) {
        std::cout << std::flush;
        croak("key_values: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }

UV
CountLiberties::signature()

IV
CountLiberties::target_width(int target_width = 0)
  CODE:
    try {
        RETVAL = THIS->target_width();
        if (items >= 2) THIS->target_width(target_width);
    } catch(std::exception &e) {
        croak("target_width: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }
  OUTPUT:
    RETVAL


IV
CountLiberties::filter(int x, int y, int filter = 0)
  CODE:
    try {
        RETVAL = THIS->filter(x, y);
        if (items >= 4) THIS->filter(x, y, filter);
    } catch(std::exception &e) {
        croak("filter: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }
  OUTPUT:
    RETVAL

UV
CountLiberties::record_need()

int
CountLiberties::record_map(int x, int y)
  CODE:
    try {
        RETVAL = THIS->record_map(x, y);
    } catch(std::exception &e) {
        croak("record_map: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }
  OUTPUT:
    RETVAL

void
CountLiberties::record(int x, int y)
  PPCODE:
    try {
        THIS->record(x, y);
    } catch(std::exception &e) {
        croak("record: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }

void
CountLiberties::record_by_cost()
  PPCODE:
    try {
        THIS->record_by_cost();
    } catch(std::exception &e) {
        croak("record_by_cost: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }

void
CountLiberties::records()
  PPCODE:
    uint nr = THIS->record_size();
    if (GIMME_V == G_ARRAY) {
        EXTEND(SP, nr);
        int pos = 0;
        for (auto const& coord: THIS->records()) {
            AV* av = newAV();
            SV* rv = newRV_noinc((SV *) av);
            sv_2mortal(rv);
            PUSHs(rv);

            av_extend(av, 2);
            av_push(av, newSVuv(coord.x()));
            av_push(av, newSVuv(coord.y()));
            av_push(av, newSViv(THIS->maximum_history(pos) ? 1 : -1));

            ++pos;
        }
    } else if (GIMME_V == G_SCALAR) {
        dXSTARG;
        PUSHu(nr);
    }

void
CountLiberties::record_sort()

void
CountLiberties::record_flush()

UV
CountLiberties::record_size()

void
CountLiberties::record_last_column(int index)
  PPCODE:
    int y0[CountLiberties::EXPANDED_SIZE];
    uint nr = THIS->record_last_column(index, y0);
    if (GIMME_V == G_ARRAY) {
        EXTEND(SP, nr);
        for (uint i=0; i<nr; ++i)
            mPUSHi(y0[i]);
    } else if (GIMME_V == G_SCALAR) {
        dXSTARG;
        PUSHu(nr);
    }

NV
CountLiberties::cost(int pos, double cost = 0)
  CODE:
    try {
        RETVAL = THIS->cost(pos);
        if (items >= 3) THIS->cost(pos, cost);
    } catch(std::exception &e) {
        croak("cost: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }
  OUTPUT:
    RETVAL

int
CountLiberties::run_round(int x, int y)
  CODE:
    try {
        RETVAL = THIS->run_round(x, y);
    } catch(std::exception &e) {
        croak("run_round: %s", e.what());
    } catch(...) {
        croak("Unknown exception");
    }
  OUTPUT:
    RETVAL

UV
CountLiberties::max_real_max()

UV
CountLiberties::real_max()

UV
CountLiberties::real_min()

UV
CountLiberties::max_size()

int
CountLiberties::_offset()

int
CountLiberties::maximum()

bool
CountLiberties::no_solution()

bool
CountLiberties::reversed()

int
CountLiberties::maximum_history(int bit)

UV
CountLiberties::maximum_column()

static UV
CountLiberties::entry_set_size()
  CODE:
    PERL_UNUSED_VAR(CLASS);
    RETVAL = sizeof(CountLiberties::EntrySet);
  OUTPUT:
    RETVAL

static UV
CountLiberties::countliberties_size()
  CODE:
    PERL_UNUSED_VAR(CLASS);
    RETVAL = sizeof(CountLiberties);
  OUTPUT:
    RETVAL

static UV
CountLiberties::entry_size()
  CODE:
    PERL_UNUSED_VAR(CLASS);
    RETVAL = sizeof(CountLiberties::Entry);
  OUTPUT:
    RETVAL

static UV
CountLiberties::thread_data_size()
  CODE:
    PERL_UNUSED_VAR(CLASS);
    RETVAL = sizeof(CountLiberties::ThreadData);
  OUTPUT:
    RETVAL

static UV
CountLiberties::vector_size()
  CODE:
    PERL_UNUSED_VAR(CLASS);
    std::vector<int> dummy;
    RETVAL = sizeof(dummy);
  OUTPUT:
    RETVAL

static UV
CountLiberties::max_height()
  CODE:
    PERL_UNUSED_VAR(CLASS);
    RETVAL = CountLiberties::max_height();
  OUTPUT:
    RETVAL

static void
CountLiberties::malloc_stats(const char* opts = nullptr)
  PPCODE:
    PERL_UNUSED_VAR(CLASS);
#ifdef JEMALLOC
    je_malloc_stats result;
    result.length = 0;
    malloc_stats_print(je_malloc_stats_cb, &result, opts);
    SV* pv = newSVpvn(result.buffer, result.length);
    sv_2mortal(pv);
    PUSHs(pv);
#else  /* JEMALLOC */
    PERL_UNUSED_VAR(opts);
# ifdef TCMALLOC
    char buffer[4096];
    MallocExtension::instance()->GetStats(buffer, sizeof(buffer));
    SV* pv = newSVpv(buffer, 0);
    sv_2mortal(pv);
    PUSHs(pv);
# else  /* TCMALLOC */
    croak("jemalloc/tcmalloc not compiled in");
# endif /* TCMALLOC */
#endif /* JEMALLOC */

UV
get_memory(...)
  CODE:
    PERL_UNUSED_VAR(items);
    RETVAL = CountLiberties::get_memory();
  OUTPUT:
    RETVAL

UV
nr_threads_default(...)
  CODE:
    PERL_UNUSED_VAR(items);
    RETVAL = std::thread::hardware_concurrency();
  OUTPUT:
    RETVAL

UV nr_cpus(...)
  PREINIT:
    cpu_set_t cs;
  CODE:
    PERL_UNUSED_VAR(items);
    if (sched_getaffinity(0, sizeof(cs), &cs))
        croak("Could not determine number of CPUs: %s", strerror(errno));
    RETVAL = CPU_COUNT(&cs);
  OUTPUT:
    RETVAL

static void
CountLiberties::malloc_property(const char* property, UV new_value=0)
  PPCODE:
    PERL_UNUSED_VAR(CLASS);
#ifdef JEMALLOC
    dXSTARG;
    size_t len;
    int rc;
    SV *pv;
    // size_t len = sizeof(buffer);
    union {
        int int_value;
        uint uint_value;
        uint32_t uint32_value;
        uint64_t uint64_value;
        size_t size_value;
        ssize_t ssize_value;
        bool bool_value;
        char const* char_value;
    };
    switch(new_value) {
        case 0:
          len = sizeof(uint_value);
          rc = mallctl(property, &uint_value, &len, nullptr, 0);
          if (rc) croak("mallcltl error %d", rc);
          PUSHu(uint_value);
          break;
        case 1:
          len = sizeof(uint32_value);
          rc = mallctl(property, &uint32_value, &len, nullptr, 0);
          if (rc) croak("mallcltl error %d", rc);
          PUSHu(uint32_value);
          break;
        case 2:
          len = sizeof(uint64_value);
          rc = mallctl(property, &uint64_value, &len, nullptr, 0);
          if (rc) croak("mallcltl error %d", rc);
          PUSHu(uint64_value);
          break;
        case 3:
          len = sizeof(bool_value);
          rc = mallctl(property, &bool_value, &len, nullptr, 0);
          if (rc) croak("mallcltl error %d", rc);
          PUSHs(bool_value ? &PL_sv_yes : &PL_sv_no);
          break;
        case 4:
          len = sizeof(size_value);
          rc = mallctl(property, &size_value, &len, nullptr, 0);
          if (rc) croak("mallcltl error %d", rc);
          PUSHu(size_value);
          break;
        case 5:
          len = sizeof(ssize_value);
          rc = mallctl(property, &ssize_value, &len, nullptr, 0);
          if (rc) croak("mallcltl error %d", rc);
          PUSHi(ssize_value);
          break;
        case 6:
          len = sizeof(int_value);
          rc = mallctl(property, &int_value, &len, nullptr, 0);
          if (rc) croak("mallcltl error %d", rc);
          PUSHi(int_value);
          break;
        case 7:
          len = sizeof(char_value);
          rc = mallctl(property, &char_value, &len, nullptr, 0);
          if (rc) croak("mallcltl error %d", rc);
          pv = newSVpvn(char_value, 0);
          sv_2mortal(pv);
          PUSHs(pv);
          break;
        default:
          croak("Unknown property type");
    }
#else  /* JEMALLOC */
# ifdef TCMALLOC
    size_t value;
    if (items >= 3) {
        value = new_value;
        MallocExtension::instance()->SetNumericProperty(property, value);
    }
    MallocExtension::instance()->GetNumericProperty(property, &value);
    dXSTARG;
    PUSHu(value);
# else  /* TCMALLOC */
    croak("tcmalloc not compiled in");
# endif /* TCMALLOC */
#endif /* JEMALLOC */

UV
history_bits(...)
  CODE:
    PERL_UNUSED_VAR(items);
    RETVAL = CountLiberties::HISTORY_BITS;
  OUTPUT:
    RETVAL

void
CountLiberties::map_load_factor(float factor)

void
CountLiberties::backbone_load_factor(float factor)

void
revision_system(...)
  PPCODE:
    PERL_UNUSED_VAR(items);
    mPUSHp(revision_system, strlen(revision_system));

void
parent_revision(...)
  PPCODE:
    PERL_UNUSED_VAR(items);
    mPUSHp(parent_revision, strlen(parent_revision));

void
current_revision(...)
  PPCODE:
    PERL_UNUSED_VAR(items);
    mPUSHp(current_revision, strlen(current_revision));

void
compile_date(...)
  PPCODE:
    PERL_UNUSED_VAR(items);
    mPUSHp(compile_date, strlen(compile_date));

void
compile_time(...)
  PPCODE:
    PERL_UNUSED_VAR(items);
    mPUSHp(compile_time, strlen(compile_time));
