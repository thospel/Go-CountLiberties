/* $Version: 1.000 $ */

#include "utils.hpp"

#include <cerrno>

#include <unistd.h>
#include <sys/times.h>

volatile int debug_volatile;

Assertion::Assertion(std::string const& what) :
    std::logic_error{"Assertion: " + what}
{}

Assertion::Assertion(std::string const& what, int line, char const* file) :
    std::logic_error{std::string("File ") + file + ":" + std::to_string(line) +
        ": Assertion: " + what}
{}

SystemError::SystemError(std::string const& what) :
    std::system_error{errno, std::system_category(), what}
{}

SystemError::SystemError(std::string const& what, int line, char const* file) :
    std::system_error{errno, std::system_category(),
        std::string("File ") + file + ":" + std::to_string(line) + ": " + what}
{}

int Elapsed::Period::initialized_{0};
clock_t Elapsed::Period::init_clock_;
struct tms Elapsed::Period::init_times_;
int64_t Elapsed::Period::ticks_per_sec_;

Elapsed::Period Elapsed::Period::start_time_{0};

Elapsed::Period::Period(int dummy): wall_{0}, user_{0}, system_{0} {
    if (initialized_) ASSERTION("Multiple start_time initialization");

    long speed = sysconf(_SC_CLK_TCK);
    if (speed < 0) {
        if (speed == -1L)
            SYS_ERROR("Could not determine _SC_CLK_TCK");
        ASSERTION("sysconf(_SC_CLK_TCK) returned a negative value");
    }
    ticks_per_sec_ = speed;

    init_clock_ = -1;
    if (init_clock_ > 0)
        ASSERTION("clock_t is an not a signed type");
    init_clock_ = times(&init_times_);
    if (init_clock_ == (clock_t) -1)
        SYS_ERROR("Error in times(2)");

    initialized_ = 1;
}

void Elapsed::Period::measure_period(Elapsed::Period &from) {
    struct tms end_time;

    clock_t end = times(&end_time);
    if (end == (clock_t) -1)
        SYS_ERROR("Error in times(2)");
    wall_ = (int64_t) (end - init_clock_) * 1000000 / ticks_per_sec_ - from.wall_;
    if (wall_ < 0) {
        if (wall_ < -MAX_WARP) ASSERTION("clock_t overflow");
        /* Assume time just ran backward */
        std::cerr << "Time warp. Clock is probably wrong" << std::endl;
        init_clock_ = end;
        from.wall_ -= wall_;
        wall_ = 0;
    }
    user_ = (int64_t) (end_time.tms_utime - init_times_.tms_utime) * 1000000 / ticks_per_sec_ - from.user_;
    if (user_ < 0) {
        if (user_ < -MAX_WARP) ASSERTION("clock_t overflow");
        /* Assume time just ran backward */
        std::cerr << "Time warp. Clock is probably wrong" << std::endl;
        init_times_ = end_time;
        from.user_ -= user_;
        user_ = 0;
    }
    system_ = (int64_t) (end_time.tms_stime - init_times_.tms_stime) * 1000000 / ticks_per_sec_ - from.system_;
    if (system_ < 0) {
        if (system_ < -MAX_WARP) ASSERTION("clock_t overflow");
        /* Assume time just ran backward */
        std::cerr << "Time warp. Clock is probably wrong" << std::endl;
        init_times_   = end_time;
        from.system_ -= system_;
        system_ = 0;
        from.user_ -= user_;
        user_ = 0;
    }
}

/* ========================================================================= */
Alloc::~Alloc() {
    if (alloced_ || allocs_)
        std::cerr << "Memory leak: " << alloced_ << " bytes in " << allocs_ << " blocks\n";
}

void* Alloc::_alloc_aligned(size_t alignment, size_t size,
                           int line, char const* file) {
    void *ptr;

    /* warn("%.200s:%d allocates %lu bytes\n", file, line, (unsigned long) size); */
    if (MALLOC == 2) {
        ptr = std::malloc(size);
        if (!ptr) {
            throw std::bad_alloc();
            // char buf[80];
            // if (strerror_r(errno, buf, sizeof(buf)))
            //     strcpy(buf, "Unknown errno");
            // croak("malloc error (%.200s:%d): %.100s", file, line, buf);
        }
    } else {
        if (posix_memalign(&ptr, alignment, size)) {
            throw std::bad_alloc();
            // char buf[80];
            // if (strerror_r(errno, buf, sizeof(buf)))
            //     strcpy(buf, "Unknown errno");
            // croak("posix_memalign error (%.200s:%d): %.100s", file, line, buf);
        }
    }
    alloced_ += size;
    ++allocs_;
    if (alloced_ > max_alloced_) max_alloced_ = alloced_;
    if (allocs_ > max_allocs_)   max_allocs_  = allocs_;
    return ptr;
}

void Alloc::_free_aligned(void *ptr, size_t size, int line, const char *file) {
    if (allocs_ == 0)
        throw Assertion("freed more blocks than allocated", line, file);
    if (alloced_ < size)
        throw Assertion("freed more bytes than allocated", line, file);
    --allocs_;
    alloced_ -= size;
    std::free(ptr);
}

/* ========================================================================= */
