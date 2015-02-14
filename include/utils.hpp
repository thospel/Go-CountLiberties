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

/* ========================================================================= */
#define ASSERTION(x)	throw Assertion((x),   __LINE__, __FILE__)
#define SYS_ERROR(x)	throw SystemError((x), __LINE__, __FILE__)

/* ========================================================================= */
#define ALLOC_ALIGNED(alignment, size)	_alloc_aligned(alignment, size, __LINE__, __FILE__)
#define ALLOC_CACHE_ALIGNED(size)	_alloc_aligned(CACHE_LINE, size, __LINE__, __FILE__)
#define ALLOC_VECTOR_ALIGNED(size)	_alloc_aligned(VECTOR_ALIGNMENT, size, __LINE__, __FILE__)
#define FREE_ALIGNED(ptr, size)	_free_aligned(ptr, size, __LINE__, __FILE__)

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

/* ========================================================================= */
inline Alloc::Alloc() :
    alloced_{0},
    allocs_{0},
    max_alloced_{0},
    max_allocs_{0}
{}

#endif /* utils_hpp */
