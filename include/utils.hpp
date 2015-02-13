/* $Version: 1.000 $ */

#ifndef utils_hpp
# define utils_hpp 1

#include <cstring>	// for memcpy
#include <string>
#include <iostream>
#include <vector>
#include <iterator>
#include <system_error>
#include <utility>

#include "constants.hpp"

class Chain;
class Mover;
class Color;
class VertexId;
class Vertex;
class EmptyVertex;
class ColoredVertex;
class VertexSelector;
class BoardEmptySet;
class BoardMemory;
class Board;
class Move;
class Game;
class Position;
class Tree;

/* ========================================================================= */
#define DEBUG_WHERE()	do { std::cerr << "File " << __FILE__ << " line " << __LINE__ << "\n"; } while (false)
#define ASSERTION(x)	throw Assertion((x),   __LINE__, __FILE__)
#define SYS_ERROR(x)	throw SystemError((x), __LINE__, __FILE__)

/* ========================================================================= */
#define ALLOC_ALIGNED(alignment, size)	_alloc_aligned(alignment, size, __LINE__, __FILE__)
#define ALLOC_CACHE_ALIGNED(size)	_alloc_aligned(CACHE_LINE, size, __LINE__, __FILE__)
#define ALLOC_VECTOR_ALIGNED(size)	_alloc_aligned(VECTOR_ALIGNMENT, size, __LINE__, __FILE__)
#define FREE_ALIGNED(ptr, size)	_free_aligned(ptr, size, __LINE__, __FILE__)

/* ========================================================================= */
// A group can have a size up to board area (-1)
typedef uint16_t	Stones;
typedef uint16_t	BranchNr;
// All stones can become prisoners, but they can be for either party, so signed
typedef int16_t		Prisoners;
// A chain can have at most 241 liberties on a 19x19 board,
// so we can use uint8_t
// However for alive caching we need to be able to overestimate liberties
// by a lot and currently we would overflow on uint8_t, so use uint16_t instead
typedef uint16_t	Liberties;
typedef int16_t		MoveNr;
typedef uint64_t	Zobrist;
typedef float		Gamma;

/* ========================================================================= */
class Assertion : public std::logic_error {
  public:
    explicit Assertion(std::string const& what) COLD;
    explicit Assertion(std::string const& what, int line, char const* file) COLD;
};

/* ========================================================================= */
class SystemError : public std::system_error {
  public:
    explicit SystemError(std::string const& what) COLD;
    explicit SystemError(std::string const& what, int line, char const* file) COLD;
};

/* ========================================================================= */
class Elapsed {
  public:
    Elapsed();			// Create timer without starting
    Elapsed(int dummy);		// Create timer and immediately start it
    void start();		// start the timer
    void measure();		// later queries will return from this time
    // wall time in seconds   (truncated to int64_t)
    int64_t get_wall() const PURE;
    // user time in seconds   (truncated to int64_t)
    int64_t get_user() const PURE;
    // system time in seconds (truncated to int64_t)
    int64_t get_system() const PURE;
    // wall time in microseconds
    int64_t get_wall_micro() const PURE;
    // user time in microseconds
    int64_t get_user_micro() const PURE;
    // system time in microseconds
    int64_t get_system_micro() const PURE;
    // wall time in seconds
    double get_wall_double() const PURE;
    // user time in seconds
    double get_user_double() const PURE;
    // system time in seconds
    double get_system_double() const PURE;

  private:
    class Period {
      public:
        Period(int dummy) COLD;
        Period();

        int64_t	wall_;
        int64_t	user_;
        int64_t	system_;
        void measure_period(Period &from);

        // 1000 seconds is the biggest NTP step
        int const MAX_WARP = 1000*1000000;

        static int64_t ticks_per_sec_;
        static clock_t init_clock_;
        static struct tms init_times_;
        static int initialized_;
        static Period start_time_;
    };

    Period start_;
    Period period_;
};

/* ========================================================================= */
class Alloc {
  public:
    Alloc();
    ~Alloc();
    void *_alloc_aligned(size_t alignment, size_t size, int line, char const* file)
	__attribute__((malloc))
	__attribute__((alloc_size(3)));
    void _free_aligned(void *ptr, size_t size, int line, char const* file);
    size_t get_alloced() const COLD PURE;
    size_t get_allocs() const COLD PURE;
    size_t get_max_alloced() const COLD PURE;
    size_t get_max_allocs() const COLD PURE;

    enum {
        /*
          MALLOC:
          0: Don't ever use malloc for aligned requests
          1: Use malloc only for non-slab (gt) requests
          2: Always use malloc, whatever the code asks

          Notice that your frees must match your allocs
        */
        MALLOC			= 2,
        VECTOR_ALIGNMENT	= constants::VECTOR_ALIGNMENT,
        CACHE_LINE		= constants::CACHE_LINE,
    };
  private:
    size_t alloced_;
    size_t allocs_;
    size_t max_alloced_;
    size_t max_allocs_;
};

/* ========================================================================= */
inline Elapsed::Period::Period() {}
/* ========================================================================= */
inline Elapsed::Elapsed() {}

inline Elapsed::Elapsed(int dummy) { start(); }

inline void Elapsed::measure() { period_.measure_period(start_); }

inline void Elapsed::start() { start_.measure_period(Period::start_time_); }

inline int64_t Elapsed::get_wall_micro()   const { return period_.wall_; }

inline int64_t Elapsed::get_user_micro()   const { return period_.user_; }

inline int64_t Elapsed::get_system_micro() const { return period_.system_; }

inline int64_t Elapsed::get_wall() const { return period_.wall_ / 1000000; }

inline int64_t Elapsed::get_user() const { return period_.user_ / 1000000; }

inline int64_t Elapsed::get_system() const { return period_.system_ / 1000000; }

inline double Elapsed::get_wall_double() const {
    return static_cast<double>(period_.wall_) / 1000000.;
}

inline double Elapsed::get_user_double() const {
    return static_cast<double>(period_.user_) / 1000000.;
}

inline double Elapsed::get_system_double() const {
    return static_cast<double>(period_.system_) / 1000000.;
}

/* ========================================================================= */
inline Alloc::Alloc() :
    alloced_{0},
    allocs_{0},
    max_alloced_{0},
    max_allocs_{0}
{}

inline size_t Alloc::get_alloced()	const { return alloced_; }

inline size_t Alloc::get_allocs()	const { return allocs_; }

inline size_t Alloc::get_max_alloced()	const { return max_alloced_; }

inline size_t Alloc::get_max_allocs()	const { return max_allocs_; }

/* ========================================================================= */
// Completely unused but a convenient marker in assembly
#if DEBUG_VOLATILE
# define CHECK_VOLATILE(x) do { debug_volatile = x; } while (0)
#else /* DEBUG_VOLATILE */
# define CHECK_VOLATILE(x) do {} while (0)
#endif /* DEBUG_VOLATILE */

extern volatile int debug_volatile;
/* ========================================================================= */


#endif /* utils_hpp */
