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

#endif /* constants_hpp */
