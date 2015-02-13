// Emacs: -*- mode: C++;-*-

// Activate assertions
// #undef NDEBUG

// Use tcmalloc, part of Google Performance Tools:
//   http://goog-perftools.sourceforge.net/
// This is optional. Standard system malloc works fine but is slower and bloats
#define TCMALLOC

// Use jemalloc
//   http://www.canonware.com/jemalloc/
// This is optional. Standard system malloc works fine but is slower and bloats
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

// For reporting memory usage
size_t const PAGE_SIZE = 4096;

void fatal(std::string const message) NORETURN COLD;

NOINLINE void fatal(std::string message) {
    if (true) {
        std::cerr << message << std::endl;
        abort();
    } else {
        throw std::logic_error(message);
    }
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
        // BLACK_UP_DOWN. And only these two don't change under reverse
        // We chose LIBERTY = 4 so that the initial "all liberties" has value 0
        LIBERTY		= 4,
        EMPTY		= 7,
        STATES		= 8,
        BITS_PER_VERTEX = 2,
        STONE_MASK	= (1 << BITS_PER_VERTEX) -1,		// 0x03,

        // MAX_SIZE can be increased up to 24, but 21 or above leave only
        // 8 HISTORY_BITS, so finding an actual solution will be slow
        MAX_SIZE	= constants::MAX_SIZE_Y,		// 19
        // MAX_SIZE	= 24,		// 24
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

    enum Operation {
        CALL_DOWN,
        CALL_UP,
        CALL_ASYM_FINAL,
        CALL_SYM_FINAL,
        SIGNATURE,
        FINISH,
        WAITING,
    };

    // Repeated 01 bit pattern in the upper COMPRESSED_SIZE bytes
    static uint64_t const FILL_MULTIPLIER = UINT64_C(-1) / 3 << (sizeof(uint64_t) - COMPRESSED_SIZE) * 8;
    static uint64_t const BLACK_MASK         = FILL_MULTIPLIER * BLACK;
    static uint64_t const BLACK_UP_MASK      = FILL_MULTIPLIER * BLACK_UP;
    static uint64_t const BLACK_DOWN_MASK    = FILL_MULTIPLIER * BLACK_DOWN;
    static uint64_t const BLACK_UP_DOWN_MASK = FILL_MULTIPLIER * BLACK_UP_DOWN;

    static_assert(HISTORY_BYTES >= 0,
                  "We won't be able to fit an Entry in an uint64_t");

    class Column {
      public:
        Column() {};
        Column(char const* from, int height);
        auto      & operator[](int i)       { return column_[i]; }
        auto const& operator[](int i) const { return column_[i]; }
        std::string to_raw_string(size_t height) const;
        char to_string(char* result, size_t height) const;
        auto to_string(const size_t height) const {
            char buffer[EXPANDED_SIZE+1];
            to_string(buffer, height);
            return std::string(buffer, height);
        }

      private:
        std::array<State, EXPANDED_SIZE> column_;
    };

    class CompressedColumn {
      public:
        constexpr size_t length() { return COMPRESSED_SIZE; }
        uint64_t column() const {
            return _column() >> shift64;
        }
        void column(uint64_t column) {
            value_ = (value_ & ~column_mask64) | column << shift64;
        }
        auto fast_hash(uint shift = shift64) const -> uint64_t {
            // return hash();
            // return murmur_mix(column());
            // return column();	// Very bad collisions
            // Terrible hash, but seems to perform really well for our case
            return column() * lcm_multiplier >> shift;
        }
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
        static uint popcount32(uint v) {
            return _mm_popcnt_u32(v);
        }
        static uint popcount64(uint64_t v) {
            return _mm_popcnt_u64(v);
        }
        static uint half_popcount64(uint64_t v) {
            return _mm_popcnt_u64(v) / 2;
        }
#else /* __POPCNT__ */
        static uint popcount32(uint v) {
            v -= (v >> 1) & 0x55555555;
            v  = (v & 0x33333333) + ((v >> 2) & 0x33333333);
            v  = (v + (v >> 4)) & 0xF0F0F0F;
            return v * 0x1010101 >> 24;
        }
        static uint popcount64(uint64_t v) {
            v -= (v >> 1) & UINT64_C(0x5555555555555555);
            v  = (v & UINT64_C(0x3333333333333333)) + ((v >> 2) & UINT64_C(0x3333333333333333));
            v  = (v + (v >> 4)) & UINT64_C(0x0f0f0f0f0f0f0f0f);
            return v * UINT64_C(0x0101010101010101) >> 56;
        }
        static uint half_popcount64(uint64_t v) {
            v  = v & UINT64_C(0x5555555555555555);
            v  = (v & UINT64_C(0x3333333333333333)) + ((v >> 2) & UINT64_C(0x3333333333333333));
            v  = (v + (v >> 4)) & UINT64_C(0x0f0f0f0f0f0f0f0f);
            return v * UINT64_C(0x0101010101010101) >> 56;
        }
#endif /* __POPCNT__ */
        void clear(Liberties liberties = 0) {
            value_ = liberties;
        }
        auto nr_empty(uint index) const -> uint;
        auto nr_empty(CompressedColumn backbone) const -> uint;
        bool multichain(uint64_t mask) const;
        bool multichain(int from, int height) const;
        void expand(Column& column, int from, int height) const;

        auto test_vertex(uint64_t mask) const {
            return  _column() &  mask;
        }
        auto _get_vertex(uint shift) const {
            // Take care! do not use if shift can be >= sizeof(uint64_t)
            return  _column() >> shift & STONE_MASK;
        }
        auto get_vertex(uint shift) const {
            // Take care! do not use if final shift can be >= sizeof(uint64_t)
            _get_vertex(shift * BITS_PER_VERTEX);
        }
        void set_liberty(uint64_t mask) {
            clear_stone_mask(mask);
        }
        void set_empty(uint64_t mask) {
            set_stone_mask(mask);
        }
        void set_black(uint64_t mask) {
            clear_stone_mask(mask);
        }
        void add_direction(uint64_t mask) {
            _column(_column() |  mask);
        }
        static uint stone_shift(uint pos) {
            return pos * BITS_PER_VERTEX + shift64;
        }
        static uint64_t _stone_mask(uint pos, uint64_t mask= STONE_MASK) {
            // Take care! do not use if pos can be >= sizeof(uint64_t)
            return mask << pos;
        }
        static uint64_t stone_mask(uint pos) {
            // Take care! do not use if shift can be >= sizeof(uint64_t)
            return _stone_mask(stone_shift(pos));
        }

        std::string raw_column_string() const;

        // Remove the down pointer to the current group looking up
        void _terminate_up(uint64_t down_mask, uint64_t value);
        // Remove the up pointer to the current group looking down
        void _terminate_down(uint64_t up_mask, uint64_t value);
        // Go to the top of the current group and make it point up
        void _join_up(uint64_t stone_mask, uint64_t value);
        // Go to the bottom of the current group and make it point down
        void _join_down(uint64_t stone_mask, uint64_t value);

      protected:
        static uint const top_bit = 1 << (sizeof(uint)*8-1);
        static uint const shift64 = 8*(sizeof(uint64_t) - COMPRESSED_SIZE);
        static uint const shift8  = 8*(sizeof(uint64_t) - 1);
        static uint64_t const column_mask64	= UINT64_C(-1) << shift64;
        static uint64_t const murmur_seed       = UINT64_C(0xc70f6907);
        static uint64_t const lcm_multiplier    = UINT64_C(6364136223846793005);
        static uint64_t const murmur_multiplier = UINT64_C(0xc6a4a7935bd1e995);

        uint64_t _column() const {
            return value_;
        }
        void _column(uint64_t value) {
            value_ = value;
        }
        static uint64_t murmur_mix(uint64_t v) {
            v *= murmur_multiplier;
            return v ^ (v >> 47);
        }
        void set_stone_mask(uint64_t mask) {
            _column(_column() |  mask);
        }
        void clear_stone_mask(uint64_t mask) {
            _column(_column() & ~mask);
        }

      private:
        static uint const popcount_table_[256];
        static uint const empty_mask_table_[256];

        // The actual column data is in the COMPRESSED_SIZE MSBs
        // For Entry: liberties will be in the LSB
        // For Entry: history bits will be inbetween,
        //            byte [1, 8-COMPRESSED_SIZE-1]
        uint64_t value_;
    };

    class EntrySet;
    class Entry: public CompressedColumn {
        friend EntrySet;
      public:
        auto _liberties() const { return _column() << shift8; }
        auto liberties() const { return _column() & liberty_mask; }
        uint liberties(int offset) const {
            return static_cast<int>(liberties()) + offset;
        }
        void liberties_add(uint64_t add) {
            // Caller should make sure this doesn't overflow
            _column(_column() + add);
        };
        void liberties_subtract(uint64_t sub) {
            // Caller should make sure this doesn't underflow
            _column(_column() - sub);
        };

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

        void history_clear() { _column(_column() & ~history_mask); }
        void record0(int record) {
            if (record >= 0)
                _column(_column() & ~(1 << (record + 8)));
        }
        void record1(int record) {
            if (record >= 0)
                _column(_column() |  (1 << (record + 8)));
        }
        auto history(int bit) const { return _column() >> (bit+8) & 1; }
        std::string history_bitstring() const;

        Entry backbone(uint64_t mask) const {
            Entry temp;
            temp._column(_column() & mask);
            return temp;
        }
        static uint64_t backbone_mask(uint index) {
            uint64_t mask{0};
            uint64_t add{static_cast<uint64_t>(STONE_MASK) << shift64};
            while (index) {
                if (index & 1) mask += add;
                index >>= 1;
                add <<= BITS_PER_VERTEX;
            }
            return mask + MAX_LIBERTIES;
        }
        static Entry invalid(uint64_t base = BLACK_UP) {
            Entry temp;
            temp._column(base << shift64);
            return temp;
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
    // Knows how an invalid Entry looks
    class EntrySet {
      public:
        typedef size_t size_type;
        class value_type {
          public:
            bool unset()      const { return entry._column() == UNSET; }
            bool terminator() const { return entry._column() == TERMINATOR; }
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

        EntrySet():
            used_{0},
            mask_{0},
            size_{0},
            max_load_multiplier_{2.}
            {
                value_type terminator;
                terminator.entry._column(TERMINATOR);
                arena_.resize(1, terminator);
            }
        ~EntrySet() {};

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
        iterator	end()		{ return &arena_[used_]; }
        const_iterator	end() const	{ return &arena_[used_]; }
        void dump() const {
            std::cout << "dump " << (void*) this << " =";
            for (size_type i=0; i < used_; ++i)
                std::cout << "\t" << arena_[i].entry._column();
            std::cout << "\n";
        }
        void clear() {
            // std::cout << "Clear " << (void*) this << "\n";
            // repeated UNSET8 leads to UNSET
            if (size_) {
                std::memset(&arena_[0], UNSET8, used_ * sizeof(arena_[0]));
                size_ = 0;
            }
        }
        auto size()  const { return size_; }
        auto empty() const { return size_ == 0; }
        static int clz(auto x) {
#ifdef __GNUC__
            if (sizeof(size_type) == sizeof(unsigned int))
                return __builtin_clz(x);
            if (sizeof(size_type) == sizeof(unsigned long))
                return __builtin_clzl(x);
            if (sizeof(size_type) == sizeof(unsigned long long))
                return __builtin_clzll(x);
#endif /* __GNUC__ */
            // Here we assume x < 2**32 which is ok for realistic EntrySets
            int r = sizeof(x) * 8 - 1;
            if (x & 0xFFFF0000) {
                x >>= 16;
                r -= 16;
            }
            if (x & 0xFF00) {
                x >>= 8;
                r -= 8;
            }
            if (x & 0xF0) {
                x >>= 4;
                r -= 4;
            }
            if (x & 0xC) {
                x >>= 2;
                r -= 2;
            }
            if (x & 0x2) {
                // x >>= 1;
                r -= 1;
            }
            return r;
        }
        void reserve(size_type elements) {
            // Most reserves are for size 0
            // Ignoring size 0, most reserves end up at 1 << (height()+1)/2
            // std::cout << "Reserve " << (void*) this << ": " << elements << "\n";
            if (size_) fatal("reserve only supported on empty EntrySets");
            size_type target = elements * max_load_multiplier_;
            if (target) {
                --target;
                // We must have at least 1 empty to prevent find() from looping
                if (target < elements) target = elements;
                assert(target > 0);
                shift_ = clz(target);
                // Set all bits after the first one
                target = (static_cast<size_type>(0) - 1) >> shift_;
                shift_ += (sizeof(uint64_t) - sizeof(size_type)) * 8;
            }
            ++target;
            // std::cout << "Really Reserve " << target << "\n";
            if (target == used_) return;
            arena_[used_].entry._column(UNSET);
            if (target >= arena_.size()) arena_.resize(target+1, arena_[used_]);
            arena_[target].entry._column(TERMINATOR);
            used_ = target;
            mask_ = target - 1;
        }
        // Normally insert returns pair of iterator and bool
        // We return nullptr if the insert worked or the address on failure
        bool insert(Entry entry, value_type*& where) {
            assert(entry._column() != UNSET);
            assert(entry._column() != TERMINATOR);
            // if (size_ >= used_ * max_load_factor_) fatal("size " + std::to_string(size_) + ", used=" + std::to_string(used_));
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
        // Normally find returns an iterator to the position or end()
        // Instead we return a direct pointer to the position or nullptr
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
        float max_load_factor() const { return 1. / max_load_multiplier_; }
        void max_load_factor(float max_load_factor) { max_load_multiplier_ = 1. / max_load_factor; }
      private:
        static constexpr uint8_t  UNSET8     = -1;
        static constexpr uint64_t UNSET      = -1;
        static constexpr uint64_t TERMINATOR =  0;

        std::vector<value_type> arena_;
        size_type used_;
        size_type mask_;
        size_type size_;
        int shift_;
        float max_load_multiplier_;
    };

    typedef EntrySet::size_type size_type;
    typedef EntrySet::iterator iterator;
    typedef EntrySet::const_iterator const_iterator;

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
      public:
        Entry max_entry CACHE_ALIGNED;
        int  max_index;
        uint real_max;
        uint new_max;
        uint new_min;
      private:
        std::array<EntrySet, 3> maps_;
      public:
        EntrySet backbone_set;
        uint64_t result;
        uint old_min;
        int filter, record;
#ifdef CONDITION_VARIABLE
        std::condition_variable work_condition_;
#endif /* CONDITION_VARIABLE */
        std::mutex work_mutex_;
        int operation_;
    };

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
        void work_prepare() {
            std::unique_lock<std::mutex> lock{left_mutex_};
            left_waiting_ = 0;
        }
        void work_wait(ThreadData* thread_data) {
            std::unique_lock<std::mutex> lock{thread_data->work_mutex_};
            while (thread_data->operation_ == WAITING)
                thread_data->work_condition_.wait(lock);
        }
        void work_done_wait() {
            std::unique_lock<std::mutex> lock{left_mutex_};
            while (!left_waiting_)
                left_condition_.wait(lock);
        }
        void work_start(ThreadData& thread_data) {
            {
                std::unique_lock<std::mutex> lock{thread_data.work_mutex_};
                thread_data.operation_ = operation_;
            }
            thread_data.work_condition_.notify_one();
        }
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
        void waiting() {
            operation_ = WAITING;
        }

        void thread_loop(CountLiberties* count_liberties, ThreadData* thread_data);
        int get_work() {
	    return --atop_;
	    // return atop_--;
	    // return atop_.fetch_sub(1, std::memory_order_relaxed);
	}
        void start(CountLiberties* count_liberties);
        void do_work(CountLiberties* countliberties, ThreadData& threads_data);
        void signature() {
            operation_ = SIGNATURE;
        }
        void call_down(uint pos) {
            pos_ = pos;
            operation_ = CALL_DOWN;
        }
        void call_up(uint pos) {
            pos_ = pos;
            operation_ = CALL_UP;
        }
        void call_asym_final(uint pos) {
            pos_ = pos;
            operation_ = CALL_ASYM_FINAL;
        }
        void call_sym_final(uint pos) {
            pos_ = pos;
            operation_ = CALL_SYM_FINAL;
        }
        void finish() {
            operation_ = FINISH;
        }
        uint execute(CountLiberties* count_liberties, uint ttop) {
            if (ttop == 0) fatal("No work to start");
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
        void map_load_factor(float factor);
        void backbone_load_factor(float factor);

      private:
        class finish_exception: public std::exception {
        };

        void _do_work(CountLiberties* countliberties, ThreadData& threads_data);

        std::atomic_int atop_;
        std::atomic_int threads_left_;
        std::vector<ThreadData> threads_data_;
        // Notice that threads_ can have one element less than threads_data_
        std::vector<std::thread> threads_;
        std::exception_ptr eptr_;
#ifdef CONDITION_VARIABLE
        int left_waiting_;
        std::condition_variable left_condition_;
#endif /* CONDITION_VARIABLE */
        std::mutex left_mutex_;
        std::mutex eptr_mutex_;
        int operation_;
        uint pos_;
        uint save_thread_;
        // This boolean exists because checking eptr_ directly is slow
        bool has_eptr_;
    };

    struct Args {
        uint pos;
        uint index0, rindex0;
        uint index1, rindex1;
        int filter, record;
        uint64_t old_min;
    };

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

    static size_t get_memory() HOT;

    CountLiberties(int height, uint nr_threads = 1, bool save_thread = true);
    void clear();
    void clear_filter();
    auto reversed() const { return reversed_; }
    auto height() const { return height_; }
    auto target_width() const { return target_width_; }
    void target_width(int target_width);
    auto max_real_max() const { return max_real_max_; }
    auto real_max() const { return old_real_max_; }
    auto real_min() const { return old_min_+1; }

    void sym_compress(CompressedColumn& compressed, int index, int rindex) const HOT;

    void expand(Column& column, CompressedColumn const& compressed, int from) const;
    bool multichain(CompressedColumn const& compressed, uint64_t mask) const {
        return compressed.multichain(mask);
    }
    bool multichain(CompressedColumn const& compressed, int from) const {
        return compressed.multichain(from, height());
    }

    void insert(ThreadData& thread_data, EntrySet* map, Entry const entry) HOT;
    uint64_t signature() HOT;
    auto nr_classes() const { return nr_classes_; }
    auto nr_keys(int i) const {
        // std::cout << "    nr_keys(" << i << ")=" << old_[i].size() << "\n";
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

    void inject(int direction, Args args, ThreadData& thread_data) HOT;
    void process(int direction, Args const args, ThreadData& thread_data) HOT;
    void process_down(Args const args, ThreadData& thread_data) HOT;
    void process_up(Args const args, ThreadData& thread_data) HOT;
    void process_final(Args const args, ThreadData& thread_data) HOT;
    void process_asym(int direction, Args const args, ThreadData& thread_data) HOT;

    void map_load_factor(float factor) { threads_.map_load_factor(factor); }
    void backbone_load_factor(float factor) { threads_.backbone_load_factor(factor); }
  private:
    void _call_asym(int direction, int pos, ThreadData& thread_data) HOT;

    void _process(bool inject, int direction, Args const args,
                  uint from, bool left_black, ThreadData& thread_data) HOT;
    void entry_clear(int index) HOT {
        entry_clear(entries_[index]);
    }
    static void entry_clear(EntryVector& entries) HOT {
        entries.clear();
        entries.reserve(0);
        entries.shrink_to_fit();
    }
    void entry_transfer(ThreadData& thread_data, int index, uint pos, int up, int down) HOT;
    void cost_propagate(int x, int y) { cost_propagate(x * height() + y); }
    void cost_propagate(int pos);

    static uint8_t const bit_reverse_table_[256];
    static uint const start_table_[STATES];
    static Liberties const MAX_LIBERTIES = -1;
    static double const cost_divider;
    static double const cost_multiplier;

    // Stuff shared among threads
    std::vector<EntrySet*> maps_ CACHE_ALIGNED;
    int const height_;
    int const nr_classes_;
    std::vector<EntryVector> entries_;
    std::vector<int> reverse_bits_;
    std::vector<uint64_t> index_masks_;
    EntryVector entry00_;

    // Stuff not accessed from within a thread or constant
    Threads threads_;
    std::vector<std::vector<int>> filter_;
    std::vector<std::vector<int>> record_map_;
    std::vector<double> cost_;
    std::vector<Coordinate> record_;
    Entry max_entry_;
    int offset_;		// Current Liberty renormalization
    int max_index_;
    uint old_real_max_;		// Use to detect risk of Liberty overflow
    uint new_real_max_;
    uint max_real_max_;
    uint old_max_;
    uint new_max_;
    // Used to renormalize the liberty counts so we don't overflow Liberty
    uint old_min_;
    uint new_min_;
    uint filter_need_;
    int target_width_;
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
    // sizes_ should in principle be a size_t, but the data is spread over
    // the maps_ vector and bin size remains modest even for the largest board
    // size this program can hope to handle. Therefore an uint will be ok.
    // (saves some memory and makes the program about 1% faster)
    class Size {
      public:
        int  index = 0;
        uint size  = 0;
    };
    mutable std::vector<Size> sizes_;
    mutable std::vector<int> indices_;
    std::vector<int> indices0_;
};

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

inline __attribute__((always_inline))
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
uint const CountLiberties::CompressedColumn::popcount_table_[256] = {
#	define B2(n) n,     n+1,     n+1,     n+2
#	define B4(n) B2(n), B2(n+1), B2(n+1), B2(n+2)
#	define B6(n) B4(n), B4(n+1), B4(n+1), B4(n+2)
    B6(0), B6(1), B6(1), B6(2)
};
uint const CountLiberties::CompressedColumn::empty_mask_table_[256] = {
#	define E2(n) 0+n, 0+n, 0+n, 1+n
#	define E4(n) E2(0+n), E2(0+n), E2(0+n), E2(2+n)
#	define E6(n) E4(0+n), E4(0+n), E4(0+n), E4(4+n)
    E6(0),
    E6(0),
    E6(0),
    E6(8),
};

auto CountLiberties::CompressedColumn::nr_empty(uint index) const -> uint {
    if (0) {
        index = ~index;
        auto value = column();
        uint nr_empty = 0;
        for (uint i=0; i<length(); ++i) {
            nr_empty += popcount_table_[empty_mask_table_[value & 0xff] & index];
            index  >>= 4;
            value  >>= 8;
        }
        return nr_empty;
    } else if (1) {
        index = ~index;
        auto value = column();
        uint mask = 0;
        uint shift= 8 * length();
        while (shift) {
            shift -= 8;
            mask = mask * 16 + empty_mask_table_[value >> shift & 0xff];
        }
        return popcount32(mask & index);
    } else {
        return half_popcount64((_column() & ~index) >> shift64);
    }
}

auto CountLiberties::CompressedColumn::nr_empty(CompressedColumn backbone) const -> uint {
#ifdef __POPCNT__
        auto value = (_column() & ~backbone._column()) >> shift64;
        return half_popcount64(value);
#else  /* __POPCNT__ */
        auto value = _column() & ~backbone._column();
        value &= UINT64_C(0x5555555555555555) << shift64;
        // Special implementation of popcount32 for our use case
        uint32_t v = value + (value >> 32);
        v  = (v & 0x33333333) + ((v >> 2) & 0x33333333);
        v  = (v + (v >> 4)) & 0xF0F0F0F;
        return v * 0x1010101 >> 24;
#endif  /* __POPCNT__ */
}

// Remove the down pointer to the current group looking up
void CountLiberties::CompressedColumn::_terminate_up(uint64_t down_mask, uint64_t value) {
    int depth = 0;
    auto value2 = value << 1;
    while (true) {
        down_mask >>= BITS_PER_VERTEX;
        if (value & down_mask) {
            // BLACK_DOWN or BLACK_UP_DOWN
            if (depth <= 0) {
                value_ &= ~down_mask;
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
void CountLiberties::CompressedColumn::_terminate_down(uint64_t up_mask, uint64_t value) {
    int depth = 0;
    auto value2 = value >> 1;
    while (true) {
        up_mask <<= BITS_PER_VERTEX;
        if (value & up_mask) {
            // BLACK_UP or BLACK_UP_DOWN
            if (depth <= 0) {
                value_ &= ~up_mask;
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
void CountLiberties::CompressedColumn::_join_up(uint64_t stone_mask, uint64_t value) {
    int depth = 0;
    while (true) {
        stone_mask >>= BITS_PER_VERTEX;
        auto vertex = value & stone_mask;
        if (vertex & BLACK_DOWN_MASK) {
            if (!(vertex & BLACK_UP_MASK)) {
                // BLACK_DOWN
                if (depth-- == 0) {
                    // set to BLACK_UP_DOWN
                    value_ |= stone_mask;
                    return;
                }
            }
        } else if (vertex & BLACK_UP_MASK)
            // BLACK_UP
            ++depth;
    }
}

// Go to the bottom of the current group and make it point down
void CountLiberties::CompressedColumn::_join_down(uint64_t stone_mask, uint64_t value) {
    int depth = 0;
    while (true) {
        stone_mask <<= BITS_PER_VERTEX;
        auto vertex = value & stone_mask;
        if (vertex & BLACK_UP_MASK) {
            // BLACK_UP or BLACK_UP_DOWN
            if (!(vertex & BLACK_DOWN_MASK)) {
                // BLACK_UP
                if (depth-- == 0) {
                    // set to BLACK_UP_DOWN
                    value_ |= stone_mask;
                    return;
                }
            }
        } else if (vertex  & BLACK_DOWN_MASK)
            // BLACK_DOWN
            ++depth;
    }
}

// Strangely enough this turns out to be slower than the loop based one below
inline bool CountLiberties::CompressedColumn::multichain(uint64_t mask) const {
    // ~ column changes BLACK and BLACK_UP to 11 and 10 respectively
    // & 0xAAAAAAAAAAAAAAAA changes them both to 10 (and other blacks are 00)
    // & column_mask64 gets rid of history and liberties
    // & mask gets rid of EMPTY/LIBERTY
    uint64_t bits = (~_column() & (UINT64_C(0xAAAAAAAAAAAAAAAA) & column_mask64) & mask);
    // bits now contains as many 1 bits as there are chains
    // reverse power of 2 check (0 is considered a power of 2 which is wanted)
    return (bits & (bits - 1)) != 0;
}

inline bool CountLiberties::CompressedColumn::multichain(int from, int height) const {
    int from_mask = ~from << 2;
    height *= 8 / BITS_PER_VERTEX;
    auto value = column();
    uint chains = 0;

    for (int i = 0; i < height; ++i) {
        auto vertex = (from_mask & 0x4) | (value & STONE_MASK);
        chains += start_table_[vertex];
        if (chains > 1) return true;
        from_mask >>= 1;
        value >>= BITS_PER_VERTEX;
    }

    return false;
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

inline __attribute__((always_inline))
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

inline __attribute__((always_inline))
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

void CountLiberties::Threads::map_load_factor(float factor) {
    for (auto& thread_data: threads_data_)
        for (auto& map: thread_data)
            map.max_load_factor(factor);
}

void CountLiberties::Threads::backbone_load_factor(float factor) {
    for (auto& thread_data: threads_data_)
        thread_data.backbone_set.max_load_factor(factor);
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

inline __attribute__((always_inline))
void CountLiberties::expand(Column& column,
                            CompressedColumn const& compressed, int from) const {
    compressed.expand(column, from, height());
}

inline __attribute__((always_inline))
void CountLiberties::CompressedColumn::expand(Column& expanded,
                                              int from, int height) const {
    int from_mask = ~from << 2;
    auto value = column();
    height *= 8 / BITS_PER_VERTEX;
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

inline __attribute__((always_inline))
void CountLiberties::insert(ThreadData& thread_data, EntrySet* map, Entry const entry) {
    // std::cout << "         Insert count " << (uint) entry.liberties() << "\n";
    // std::cout << "         Out: " << column_string(entry, index) << " -> " << (uint) entry.liberties() << "\n";

    EntrySet::value_type* result;
    if (map->insert(entry, result)) {
        // Already existed
        // std::cout << "           Already exists with count " << (uint) result->liberties() << "\n";
        if (entry.liberties() <= result->entry.liberties()) return;
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

    uint max = nr_keys(0);
    auto* sizes = &sizes_[0];
    for (int i=0; i<nr_classes(); ++i) {
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

    // Process counting results to get a sorted list
    ++max;
    // if (max > max_max) max_max = max;
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

    uint64_t signature = 0;
    for (auto const& thread_data: threads_)
        signature += thread_data.result;

    // std::cout << "-> Sig " << signature << "\n";
    return signature;
}

void CountLiberties::entry_transfer(ThreadData& thread_data, int index, uint pos, int up, int down) {
    if (DEBUG_STORE)
        std::cout << "   Write entryset " << index << "\n";
    auto& map		= *maps_[index];
    auto& entries	= entries_[index];
    if (map.empty()) {
        entry_clear(entries);
        if (DEBUG_STORE)
            std::cout << "   Close entryset " << index << "\n";
        return;
    }

    auto& backbone_set	= thread_data.backbone_set;
    uint64_t mask	= index_masks_[index];

    entries.clear();
    entries.reserve(map.size());
    backbone_set.reserve(map.size());
    auto   up_mask = Entry::stone_mask(pos);
    auto down_mask = Entry::stone_mask(height() - 1 - pos);
    // std::cout << "map size=" << map.size() << "\n";

    for (auto const& element: map) {
        auto entry = element.entry;
        uint64_t liberties{entry.liberties()};
        if (up) {
            if (entry.test_vertex(up_mask)) {
                // EMPTY
                Entry probe{entry};
                probe.set_liberty(up_mask);	// Set LIBERTY
                auto found = map.find(probe);
                if (found)
                    if (liberties < found->entry.liberties())
                        continue;
            } else {
                // LIBERTY
                Entry probe{entry};
                probe.set_empty(up_mask);	// Set EMPTY
                auto found = map.find(probe);
                if (found)
                    if (liberties <= found->entry.liberties())
                        continue;
            }
        }
        if (down) {
            if (entry.test_vertex(down_mask)) {
                // EMPTY
                Entry probe{entry};
                probe.set_liberty(down_mask);	// Set LIBERTY
                auto found = map.find(probe);
                if (found)
                    if (liberties < found->entry.liberties()) continue;
            } else {
                // LIBERTY
                Entry probe{entry};
                probe.set_empty(down_mask);	// Set EMPTY
                auto found = map.find(probe);
                if (found)
                    if (liberties <= found->entry.liberties()) continue;
            }
        }

        Entry backbone{entry.backbone(mask)};
        if (0)
            std::cout <<
                "I Entry: "     << column_string(entry,    index) <<
                ", Bacbone: " << column_string(backbone, index) <<
                " (raw liberties=" << backbone.liberties() << ")\n";
        EntrySet::value_type* result;
        if (backbone_set.insert(backbone, result)) {
            // Already existed
            int64_t gain = liberties - result->entry.liberties();
            if (gain > 0)
                // Effectively set found liberties to entry.liberties()
                // However using the add makes sure the internals of Entry
                // don't have to do any bit fiddling
                result->entry.liberties_add(gain);
        }

        entries.emplace_back(entry);
    }
    // std::cout << "backbone size=" << backbone_set.size() << "\n";

    // The program logic ensures that entries is already cleared and shrunk
    uint64_t new_min = thread_data.new_min;
    uint64_t new_max = thread_data.new_max;
    if (backbone_set.size() == entries.size()) {
        // If the sizes are equal no pruning can happen
        for (auto const entry: entries) {
            uint64_t liberties{entry.liberties()};

            if (liberties < new_min) new_min = liberties;
            // std::cout << "           New\n";
            if (liberties > new_max) {
                if (liberties > thread_data.real_max)
                    thread_data.real_max = liberties;
                // if (!multichain(entry, mask)) {
                if (!multichain(entry, index)) {
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
        }
    } else {
        size_t nr_entries = 0;
        for (auto const entry: entries) {
            uint64_t liberties{entry.liberties()};
            Entry backbone{entry.backbone(mask)};
            if (0)
                std::cout <<
                    "O Entry: "     << column_string(entry,    index) <<
                    ", Bacbone: " << column_string(backbone, index) <<
                    " (raw liberties=" << backbone.liberties() << ")\n";

            auto found = backbone_set.find(backbone);
            if (!found) fatal("Did not find entry backbone");
            uint64_t max_liberties = found->entry.liberties();
            if (liberties < max_liberties) {
                uint64_t nr_empty = entry.nr_empty(backbone);
                // std::cout << "nr_empty=" << nr_empty << "\n";
                // if (liberties + nr_empty <= max_liberties) continue;
                if (liberties + nr_empty <= max_liberties) {
                    if (liberties + nr_empty < max_liberties || nr_empty)
                        continue;
                }
            }

            if (liberties < new_min) new_min = liberties;
            // std::cout << "           New\n";
            if (liberties > new_max) {
                if (liberties > thread_data.real_max)
                    thread_data.real_max = liberties;
                // if (!multichain(entry, mask)) {
                if (!multichain(entry, index)) {
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
        entries.resize(nr_entries);
    }
    entries.shrink_to_fit();
    thread_data.new_min = new_min;
    // std::cout << "entries size=" << entries.size() << "\n";

    map.clear();
    backbone_set.clear();
    // This is a slight speedup. Possibly because it does not delay fixing
    // up the TERMINATOR position (it's still in cache now)
    backbone_set.reserve(0);
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
                            ThreadData& thread_data) {
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
}

inline __attribute__((always_inline))
void CountLiberties::process(int direction, Args const args,
                             ThreadData& thread_data) {
    _process(false, direction, args, args.index0, false, thread_data);
    _process(false, direction, args, args.index1, true,  thread_data);
}

// Inline because it only has 1 call site
inline __attribute__((always_inline))
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

inline __attribute__((always_inline))
void CountLiberties::process_asym(int direction, Args const args, ThreadData& thread_data) {
    if (direction)
        process_up(args, thread_data);
    else
        process_final(args, thread_data);
}

// This is the core logic of the whole program.
// Add a bump in the given direction (0 combines two bumps to a flat column)
// Apply symmetry at the end (except when going down, direction > 0)
inline __attribute__((always_inline))
void CountLiberties::_process(bool inject, int direction, Args const args,
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

    uint up_black, down_black, up_or_down_black;
    if (direction >= 0) {
        // Make sure that args.pos == 0 works and results in up_black == 0
        // Should really be based on from, but args.index0 has the same bits
        // at position args.pos-1
        // up_black = (2*from >> args.pos) & 1;
        up_black = (2*args.index0 >> args.pos) & 1;
    }
    if (direction <= 0) {
        // Should really be based on from, but args.index0 has the same bits
        // at position args.pos+1
        // down_black = (from >> args.pos) & 2;
        down_black = (args.index0 >> args.pos) & 2;
    }
    if (direction == 0) up_or_down_black = up_black | down_black;

    bool sym0 = direction <= 0 && args.index0 >= args.rindex0;
    EntrySet* map0 = maps_[sym0 ? args.rindex0 : args.index0];
    bool sym1 = direction <= 0 && args.index1 >= args.rindex1;
    EntrySet* map1 = maps_[sym1 ? args.rindex1 : args.index1];

    uint pos2 = Entry::stone_shift(args.pos);
    auto const stone_mask	= Entry::_stone_mask(pos2);
    auto const black_up		= stone_mask & BLACK_UP_MASK;
    auto const black_down	= stone_mask & BLACK_DOWN_MASK;
    auto const black_up_down	= stone_mask; // stone_mask & BLACK_UP_DOWN_MASK

    uint64_t const up_mask	= up_black ?
        Entry::_stone_mask(pos2 - BITS_PER_VERTEX, BLACK_DOWN) :
        args.pos ? Entry::_stone_mask(pos2 - BITS_PER_VERTEX) : 0;
    // auto const up_black_down	= up_mask & BLACK_DOWN_MASK;

    // We need to test args.pos. Normally you would think that if too big the
    // stone mask will shift out. However with the barrel shifter of modern CPUs
    // it will actually not shift at all
    auto const down_mask	= down_black ?
        Entry::_stone_mask(pos2 + BITS_PER_VERTEX, BLACK_UP) :
        args.pos < EXPANDED_SIZE-1 ?
                   Entry::_stone_mask(pos2 + BITS_PER_VERTEX) : 0;
    // auto const down_black_up	= down_mask & BLACK_UP_MASK;

    // index_mask should be based on from, but except at the current position
    // args.index0 has the same bits and we will never look at the there
    // uint64_t index_mask	= index_masks_[from];
    uint64_t index_mask	= index_masks_[args.index0];

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
            // The history map is initialized with zeroes so record0 is a noop
            // result.record0(args.record);
            if (DEBUG_FLOW) {
                std::cout << "         Empty: '" <<
                    column_string(result, sym0 ? args.rindex0 : args.index0) <<
                    "' -> " << result.liberties(offset_) << " (" <<
                    result.history_bitstring() << ") set=" << (sym0 ? args.rindex0 : args.index0) << "\n";
            }
            if (inject) entry00_.emplace_back(result);
            else insert(thread_data, map0, result);
        }

      BLACK_STONE:
        // Set black

        // Avoid complete disconnect. Notice that we used to also test
        // for libs + offset_ here so we would keep empty columns were no
        // stone was ever placed. These however get added back by inject()
        if (inject || from) {
            if (args.filter < 0) continue;

            Entry& result = entry;

            if (!left_black) {
                if (left != (LIBERTY & STONE_MASK)) {
                    // EMPTY
                    // Set to LIBERTY,except that we will immediately set BLACK
                    // result.set_liberty(stone_mask);
                    result.set_black(stone_mask);	// sets BLACK
                    result.liberties_add(1);
                } else {
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
                            // We were already connected. Do nothing
                            // We could prune the loop because it is never
                            // optimal, but the program will soon discover this
                            // for itself
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
                            // We were already connected. Do nothing
                            // We could prune the loop because it is never
                            // optimal, but the program will soon discover this
                            // for itself
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
                    }
                }
            }

            if (sym1) sym_compress(result, args.index1, args.rindex1);

            result.record1(args.record);

            if (DEBUG_FLOW) {
                std::cout << "         Black: '" <<
                    column_string(result, sym1 ? args.rindex1 : args.index1) <<
                    "' -> " << result.liberties(offset_) << " (" <<
                    result.history_bitstring() << ") set=" << (sym1 ? args.rindex1 : args.index1) << "\n";
            }
            insert(thread_data, map1, result);
        }
    }
    // This would be the place to clear the entry vector
    // But we delay that to entry_transfer so the reuse there will be faster
    // if (!inject) entry_clear(from);
}

void CountLiberties::call_down(int pos, ThreadData& thread_data) {
    int bits  = 1 << pos;

    Args args;
    args.filter = thread_data.filter;
    args.record = thread_data.record;
    args.old_min = thread_data.old_min;
    args.pos     = pos;

    auto const* indices = &indices_[0];
    while (true) {
        int i = threads_.get_work();
        if (i < 0) break;
        int j = indices[i];

        // std::cout << "call_down\n";
        EntrySet* map0 = &thread_data[0];
        EntrySet* map1 = &thread_data[1];
        maps_[j]      = map0;
        maps_[j+bits] = map1;

        args.index0  = j;
        args.index1  = j+bits;
        args.rindex0 = j;
        args.rindex1 = j+bits;

        size_t grow = entries_[j].size() + entries_[j+bits].size();
        map0->reserve(grow);
        map1->reserve(grow+1);

        process_down(args, thread_data);
        if (j == 0) inject(1, args, thread_data);

        // If neighbour the given position is a guaranteed LIBERTY
        // Consider only the up direction since the bump is not down yet
        int neighbour = ~(j << 1) & bits;
        entry_transfer(thread_data, j,      pos, neighbour, false);
        entry_transfer(thread_data, j+bits, pos, false,     false);
    }
    // std::cout << "end" << std::endl;
}

void CountLiberties::call_sym_final(int pos, ThreadData& thread_data) {
    int bits  = 1 << pos;

    auto const* indices = &indices_[0];
    auto const* reverse_bits = &reverse_bits_[0];

    Args args;
    args.filter  = thread_data.filter;
    args.record  = thread_data.record;
    args.old_min = thread_data.old_min;
    args.pos     = pos;

    while (true) {
        int i = threads_.get_work();
        if (i < 0) break;
        int j  = indices[i];
        int rj = reverse_bits[j];

        // std::cout << "call_sym_final\n";
        assert(j <= rj);
        // Normally we would need to distinguish between j == rj or not
        // in the j != rj case we would have to worry about reversing
        // However sym_final is only ever called from a symmetrized position
        // so the extra case simply does not happen. The assert checks this
        assert(j == rj || entries_[rj].size() == 0);
        assert(j == rj || entries_[rj+bits].size() == 0);

        EntrySet* map0 = &thread_data[0];
        EntrySet* map1 = &thread_data[1];
        maps_[j]      = map0;
        maps_[j+bits] = map1;

        args.index0  = j;
        args.index1  = j+bits;
        args.rindex0 = rj;
        args.rindex1 = rj+bits;

        size_t grow = entries_[j].size() + entries_[j+bits].size();
        map0->reserve(grow);
        map1->reserve(grow+1);

        process_final(args, thread_data);
        if (j == 0) inject(0, args, thread_data);

        // Consider both directions since the bump is getting straightened here.
        int neighbours = ~(j << 1 | j >> 1) & bits;
        entry_transfer(thread_data, j,       pos, neighbours, false);
        entry_transfer(thread_data, j+bits,  pos, false,      false);
        // rj and rj + bits are already empty (for j != rj) so no clear needed
        // if (j != rj) {
        //     entry_clear(rj);
        //     entry_clear(rj+bits);
        // }
    }
    // std::cout << "end" << std::endl;
}

inline __attribute__((always_inline))
void CountLiberties::_call_asym(int direction, int pos, ThreadData& thread_data) {
    int  bits = 1 << pos;
    int rbits = 1 << (height() - 1 - pos);
    int cbits = bits + rbits;

    assert(bits > rbits);

    auto const* indices = &indices_[0];
    auto const* reverse_bits = &reverse_bits_[0];

    Args args;
    args.filter  = thread_data.filter;
    args.record  = thread_data.record;
    args.old_min = thread_data.old_min;
    args.pos     = pos;

    while (true) {
        int i = threads_.get_work();
        if (i < 0) break;
        int j  = indices[i];
        int rj = reverse_bits[j];

        maps_[j]	= &thread_data[0];
        maps_[j+rbits]	= &thread_data[1];
#ifndef NDEBUG
        // The rj+bits map can never get filled since j+rbits is smaller
        // maps_[rj+bits]       = &thread_data[5];
        maps_[rj+bits]	= nullptr;
#endif /* NDEBUG */

        if (j == rj) {
            // std::cout << "call_asym j==rj, direction=" << direction << "\n";
            // The j + bits map can never get filled since rj + bits is smaller
            // maps_[j+bits]       = &thread_data[2];

            size_t grow1 = entries_[j].size() + entries_[j+bits].size();
            size_t grow2 = entries_[j+rbits].size() + entries_[j+cbits].size();
            maps_[j+rbits]->reserve(grow1 + grow2 + 1);
            maps_[j]->reserve(grow1+1);

            args.index0  = j;
            args.index1  = j+bits;
            args.rindex0 = rj;
            args.rindex1 = rj+rbits;

            process_asym(direction, args, thread_data);
            if (j == 0) inject(direction, args, thread_data);

            int neighbour = ~(j >> 1) & bits;
            entry_transfer(thread_data, j, pos, neighbour, neighbour);

            maps_[j+cbits] = &thread_data[0];
            maps_[j+cbits]->reserve(grow2);

            args.index0  = j+rbits;
            args.index1  = j+cbits;
            args.rindex0 = rj+bits;
            args.rindex1 = rj+cbits;
            process_asym(direction, args, thread_data);

            // The j + bits map can never get filled since rj + bits is smaller
            // entry_transfer(thread_data, j+bits,  pos, false,  true);
            entry_clear(j+bits);
            entry_transfer(thread_data, j+rbits, pos,  neighbour & direction, false);
        } else {
            // std::cout << "call_asym j!=rj, direction=" << direction << "\n";
            assert(j < rj);
            // These two can never have gotten filled during the previous
            // call_down round (call_down only changes bits at the top half
            // of a column, that is the low bits)
            assert(entries_[rj+bits].size() == 0);
            assert(entries_[rj+cbits].size() == 0);

            // One of j+bits and rj+rbits will be unused
            // maps_[j+bits]	= &thread_data[2];
            // maps_[rj+rbits]	= &thread_data[3];
            auto mj = std::min(j+bits, rj+rbits);
            maps_[mj]		= &thread_data[2];
#ifndef NDEBUG
            // The rj map can never get filled since j is smaller
            // maps_[rj]            = &thread_data[4];
            maps_[rj]		= nullptr;
            // The rj+cbits map can never get filled since j+cbits is smaller
            // maps_[rj+cbits] = &thread_data[6];
            maps_[rj+cbits]	= nullptr;
#endif /* NDEBUG */

            // size_t grow3 = entries_[rj+rbits].size() + entries_[rj+cbits].size();
            size_t grow3 = entries_[rj+rbits].size();
            size_t grow4 = entries_[j+rbits].size() + entries_[j+cbits].size();
            // size_t grow2 = entries_[rj].size() + entries_[rj+bits].size();
            size_t grow2 = entries_[rj].size();
            maps_[j+rbits]->reserve(grow2+grow4);
            size_t grow1 = entries_[j].size() + entries_[j+bits].size();
            maps_[mj]->reserve(grow1+grow3);
            maps_[j]->reserve(grow1+grow2);

            args.index0  = j;
            args.index1  = j+bits;
            args.rindex0 = rj;
            args.rindex1 = rj+rbits;
            process_asym(direction, args, thread_data);

            if (grow2) {
                args.index0  = rj;
                args.index1  = rj+bits;
                args.rindex0 = j;
                args.rindex1 = j+rbits;
                process_asym(direction, args, thread_data);
            }

            int   up_neighbour = ~(j >> 1) &  bits;
            int down_neighbour = ~(j << 1) & rbits;
            entry_transfer(thread_data,  j, pos,  up_neighbour,  down_neighbour);
            // The rj map can never get filled since j is smaller
            // entry_transfer(thread_data, rj, pos,  true,  true);
            entry_clear(rj);

            maps_[j+cbits]	= &thread_data[0];
            maps_[j+cbits]->reserve(grow3+grow4);

            if (grow3) {
                args.index0  = rj+rbits;
                args.index1  = rj+cbits;
                args.rindex0 = j+bits;
                args.rindex1 = j+cbits;
                process_asym(direction, args, thread_data);
            }

            if (j + bits <= rj+rbits) {
                assert(j+bits < rj+rbits);
                entry_clear(rj+rbits);
                entry_transfer(thread_data,  j+bits,  pos, false,  down_neighbour & direction);
            } else {
                entry_clear(j+bits);
                entry_transfer(thread_data, rj+rbits, pos,  down_neighbour & direction, false);
            }

            args.index0  = j+rbits;
            args.index1  = j+cbits;
            args.rindex0 = rj+bits;
            args.rindex1 = rj+cbits;
            process_asym(direction, args, thread_data);

            entry_transfer(thread_data,  j+rbits,      pos,  up_neighbour & direction, false);
            // The rj+bits map can never get filled since j+rbits is smaller
            // entry_transfer(thread_data, rj+bits,       pos, false,  true);
            entry_clear(rj+bits);
            // The rj+cbits map can never get filled since j+cbits is smaller
            // entry_transfer(thread_data, rj+cbits, pos, false, false);
            entry_clear(rj+cbits);
        }
        entry_transfer(thread_data, j+cbits, pos, false, false);
    }
    // std::cout << "end" << std::endl;
}

void CountLiberties::call_up(int pos, ThreadData& thread_data) {
    _call_asym(-1, pos, thread_data);
}

void CountLiberties::call_asym_final(int pos, ThreadData& thread_data) {
    _call_asym(0, pos, thread_data);
}

int CountLiberties::run_round(int x, int pos) {
    int filter = x < target_width() ? filter_[x].at(pos) :  0;
    int record = x < target_width() ? record_map_[x].at(pos) : -1;

    // Turn off injection for column positions >= 3
    // (counting from 1, x counts from 0, so the test is x >= 2)
    // It's easy to prove that if there is a solution where the first stone is
    // in column 3, there is at least as good a solution with a stone in column
    // 2, so after this point we don't need to inject empty columns anymore
    if (x >= 2) entries_[nr_classes()].clear();

    for (auto& thread_data: threads_) {
        thread_data.max_entry = Entry::invalid();
        thread_data.real_max = new_real_max_;
        thread_data.new_max  = new_max_;
        thread_data.filter   = filter;
        thread_data.record   = record;
        thread_data.old_min  = old_min_;
        thread_data.new_min  = new_min_;
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
    uint max = 0;
    if ((pos & 1) == 0 && !final) {
        // even, work from top down
        pos_left = i;

        threads_.call_down(pos_left);

        bits  = 1 << pos_left;
        rbits = -1;

        {
            int j = 0;
            auto size = nr_keys(j) + nr_keys(j + bits);
            size += entries_[nr_classes()].size();
            if (size) {
                sizes->index = j;
                sizes->size  = size;
                ++sizes;
                max = size;
                if (max >= indices0_.size()) {
                    indices0_.resize(2*max);
                    indices0 = &indices0_[0];
                }
                // if (size >= indices0_.size()) fatal("out of bounds");
                ++indices0[size];
            }
        }
        int limit = nr_classes() - 1;
        limit &= ~bits;
        for (int j=1; j<=limit; ++j) {
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
    } else {
        // odd, work from bottom up
        pos_left = height()-1-i;

        bits  = 1 << pos_left;
        rbits = 1 << i;
        int cbits = bits | rbits;

        if (final) {
            if (bits == rbits)
                threads_.call_sym_final(pos_left);
            else
                threads_.call_asym_final(pos_left);
        } else {
            threads_.call_up(pos_left);
        }

        auto const* reverse_bits = &reverse_bits_[0];
        {
            int j = 0;
            size_t size = nr_keys(j) + nr_keys(j+bits);
            size += entries_[nr_classes()].size();
            if (bits != rbits)
                size += nr_keys(j+rbits) + nr_keys(j+cbits);
            if (size) {
                sizes->index = j;
                sizes->size  = size;
                ++sizes;
                max = size;
                if (max >= indices0_.size()) {
                    indices0_.resize(2*max);
                    indices0 = &indices0_[0];
                }
                // if (size >= indices0_.size()) fatal("out of bounds");
                ++indices0[size];
            }
        }
        int limit = nr_classes() - 1;
        limit &= ~cbits;
        for (int j=1; j<=limit; ++j) {
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
    }

    if (0) {
        std::cout <<
            "Unsorted Width=" << nr_classes() <<
            ", ttop=" << sizes - &sizes_[0] <<
            ", bits=" << bits << ", rbits=" << rbits << "\n";
        for (auto s = &sizes_[0]; s < sizes; ++s)
            std::cout << "    index " << s->index << ": size " << s->size << "\n";
    }

    // Process counting results to get a sorted list
    // I suspect that at a big enough problem size max will start growing
    // with an exponent above 2 and counting sort will start losing to a plain
    // sort on sizes_. However for any problem sizes we can realistically handle
    // on current computers max is pretty restricted (e.g. it is only 38662 for
    // a 19x19 board and the growth factor is still below 2).
    ++max;
    // if (max > max_max) max_max = max;
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
    uint threads = threads_.execute(this, ttop);

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
    // If neither linux nor compiled with tcmalloc we have no idea and return 0
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
    // if (max_max) std::cout << height() << ": max_max=" << max_max << std::endl;
    // max_max = 0;
    for (auto& entry: entries_)
        entry.clear();
    record_.clear();

    // Notice we do NOT clear the filter since we probably want to run again
    // using the new filter bits. To clear the filter call clear_filter()

    offset_   = -1;
    new_max_  =  0;
    old_min_  =  0;
    new_real_max_ = 0;
    max_real_max_ = 0;
    new_min_ = MAX_LIBERTIES;

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
        maps_[0] = &threads_[0][0];
        insert(threads_[0], maps_[0], entry);
        new_min_   = threads_[0].new_min;
        new_max_   = threads_[0].new_max;
        max_index_ = threads_[0].max_index;
        max_entry_ = threads_[0].max_entry;
        entry_transfer(threads_[0] , 0, 0, false, false);

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
    threads_{nr_threads, save_thread}
{
    // std::cout << "height=" << height_ << "\n";
    if (height > EXPANDED_SIZE)
        throw std::out_of_range
            ("Height " + std::to_string(height) +
             " is bigger than " + std::to_string(EXPANDED_SIZE));
    if (height < 0)
        throw std::out_of_range
            ("Height " + std::to_string(height) + " is below 0");

    reverse_bits_.resize(nr_classes());
    index_masks_.resize(nr_classes());
    for (int i = 0; i < nr_classes(); ++i) {
        int bits = i;
        int reverse = 0;
        for (int j=0; j<height_;++j) {
            reverse = 2 * reverse + (bits & 1);
            bits /= 2;
        }
        reverse_bits_[i] = reverse;
        index_masks_[i]  = Entry::backbone_mask(i);
    }
    target_width(height_);

    entry00_.reserve(1);
    // One extra to hold the empty column injector
    entries_.resize(nr_classes()+1);
    maps_.resize(nr_classes());
    sizes_  .resize(nr_classes());
    indices_.resize(nr_classes());
    // indices0 size needed
    //  1:     3
    //  2:     7
    //  3:     8
    //  4:    14
    //  5:    16
    //  6:    37
    //  7:    43
    //  8:    97
    //  9:   107
    // 10:   277
    // 11:   357
    // 12:   851
    // 13:  1060
    // 14:  2726
    // 15:  3480
    // 16:  8998
    // 17: 11569
    // 18: 30502
    // 19: 38662
    indices0_.resize(100);
    clear();

    threads_.start(this);
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

XS_EXTERNAL(boot_Go__CountLiberties__Shape);

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

BOOT:
  PUSHMARK(SP);
  boot_Go__CountLiberties__Shape(aTHX_ cv);
