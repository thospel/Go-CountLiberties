/* $Version: 1.000 $ */

#ifndef constants_hpp
# define constants_hpp 1

// typedef unsigned int uint;

#include <cstddef>
#include <cstdint>

#define MAGIC	1	/* Support magic */

#define STATIC		static
#define HOT		__attribute__((__hot__))
#define COLD		__attribute__((__cold__))
// pure means does not modify any (non const) global memory.
#define PURE		__attribute__((__pure__))
// const means does not read/modify any (non const) global memory.
#define FUNCTIONAL	__attribute__((__const__))
#define NORETURN	__attribute__((__noreturn__))
#define NOINLINE	__attribute__((__noinline__))
#define INLINE		static inline
#define ALWAYS_INLINE	inline __attribute__((always_inline))
#define RESTRICT	__restrict__
#define UNREACHABLE	__builtin_unreachable()
#define ALIGNED(x)	__attribute__((aligned (x)))
// #define ALIGNED(x)	alignas(x)
#define CACHE_ALIGNED	ALIGNED(_CACHE_LINE)
#define VECTOR_ALIGNED	ALIGNED(_VECTOR_ALIGNMENT)

#define _CACHE_LINE		64
#define _VECTOR_ALIGNMENT	16

/* We already get these from perl.h, but I don't like them.
   These versions allow LIKELY(ptr) etc. */
#undef LIKELY
#undef UNLIKELY
/* Having LIKELY/UNLIKELY gives about 1% speedup on an intel Core Duo CPU */
#define LIKELY(x)	__builtin_expect(!!(x),1)
#define UNLIKELY(x)	__builtin_expect(!!(x),0)
// #define LIKELY(x)	(x)
// #define UNLIKELY(x)	(x)

#ifdef NULL
# undef NULL
# define NULL nullptr
#endif /* NULL */

#ifdef LITTLE_ENDIAN
static_assert(LITTLE_ENDIAN == 1234,
              "LITTLE_ENDIAN from perl differs from LITTLE_ENDIAN from compiler");
# undef LITTLE_ENDIAN
#endif

#ifdef BIG_ENDIAN
static_assert(BIG_ENDIAN == 4321,
              "BIG_ENDIAN from perl differs from BIG_ENDIAN from compiler");
# undef BIG_ENDIAN
#endif

#ifdef BYTE_ORDER
static_assert(BYTE_ORDER == 1234,
              "BYTE_ORDER from perl differs from BYTE_ORDER from compiler");
# undef BYTE_ORDER
#endif

enum {
    LITTLE_ENDIAN = 1234,
    BIG_ENDIAN    = 4321,
    BYTE_ORDER    = 1234,
};

// Variables surrounded by $$ get their values from
// lib/Go/CountLiberties/Constants.pm
// Same with %%, but these are substituted in hex
namespace constants {
    enum {
        /* Current x86 processors have 64 byte cache lines, e.g.:
           # x86info -c
           Cache info
           L1 Instruction cache: 32KB, 8-way associative. 64 byte line size.
           L1 Data cache: 32KB, 8-way associative. 64 byte line size.
           L2 cache: 3MB, 12-way associative. 64 byte line size. Unified on-die.
        */
        CACHE_LINE		= _CACHE_LINE,
        VECTOR_ALIGNMENT	= _VECTOR_ALIGNMENT,

        LITTLE_ENDIAN		= 1234,
        BIG_ENDIAN		= 4321,
        BYTE_ORDER		= 1234,

        DEFAULT_SIZE		= 19,

        MAX_SIZE_X		= 19,
        MAX_SIZE_Y		= 19,
    };
};
#endif /* constants_hpp */
