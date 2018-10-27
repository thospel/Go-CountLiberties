# NAME

count\_liberties - Calculate the maximum number of liberties one group on a go board can have

# SYNOPSIS

    count_liberties --blib <other_options>
    count_liberties [--solution] [--quiet] [[<min_n>] <maxn>]
    count_liberties [--save_thread] [-t <threads>] [--threads <threads>] <other_options>
    count_liberties [--tcmalloc] [--jemalloc] [--llmalloc] [--libcmalloc] [--hugepages [mount_point]] <other_options>
    count_liberties [map_load_factor <float>] [topology_load_factor <float>]  <other_options>
    count_liberties [--generalize] <other_options>
    count_liberties [--html <file>] <other_options>
    count_liberties [--stats] [--dump] <other_options>
    count_liberties [--sizes] [--debug_max_size] [--debug_history] <other_options>
    count_liberties [--history_bits <n>] <other_options>
    count_liberties [--profile [<file>]] <other_options>
    count_liberties [--nice [<level>]] <other_options>
    count_liberties [--version] [-h] [--help]

# DESCRIPTION

This program calculates the maximum number of liberties a single group can have on a go board of given height. It will output a table and rule which answers this question for any board width. Optionally also gives a sample solution.

If no arguments are given it runs for all board heights from 1 to the maximum supported size. With one argument it only runs for that board size. With two arguments it runs for all sizes in the range defined by these arguments.

# OPTIONS

- --blib

    Add this option to load libraries from the current compilation directory instead of from the installed libraries. You must give this option if you want to run the current development version before it is installed. This option is not processed normally and must be given before any other options.

- --solution

    After working out the maximum number of liberties for a given board height also prints sample groups that reach this number of liberties for all board widths not greater than the height.

    This is done by following back a trail of breadcrumbs from the final column to the initital one. However during the forward calculation the old states repeatedly get dropped to save memory. So instead the forward run is repeated again and again using more and more constraints from previous runs to zoom in on an actual solution. Once a solution for a given width is found it is cut on each column and checked if that happens to give a valid solution for a smaller board width. If so that is also printed as an "extra" solution. The program then doesn't have to bother to work out a solution for that board width since we already got one for free. The whole process is repeated until for all widths not larger than the height an example has been generated.

    Which solution will be found is not deterministic if there is more than one thread.

- --quiet

    Don't print progress information, only final results

- --threads &lt;threads>, -t &lt;threads>

    Run the program with `threads` processing threads. Defaults to the number of logical CPUs.

- --save\_thread

    If given start up one less new thread and instead also use the main thread as a computation thread.

    Defaults to true.

- --nice \[level\]

    Run program under [nice(1)](http://man.he.net/man1/nice). Level defaults to `10`.

- --tcmalloc

    Use [tcmalloc](http://goog-perftools.sourceforge.net/doc/tcmalloc.html) as memory allocator.

- --jemalloc

    Use [jemalloc](http://jemalloc.net/) as memory allocator.

- --llmalloc

    Use [llmalloc](https://locklessinc.com/install_linux.shtml) as memory allocator.

- --libcmalloc

    Use standard libc malloc as memory allocator. If this option is not given the program tries to find any of the previously mentioned mallocs and use that. Only if none of them is found does it fall back to standfard libc malloc.

- --hugepages \[mount\_point\]

    Use [hugepages](https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt) from hugetlbfs `mount_point`. If `mount_point` is not given the program tries to autodetect it.

    This option implies [--tcmalloc](#tcmalloc)

- --map\_load\_factor &lt;float>

    The load at which an internal datastructure resizes

- --topology\_load\_factor &lt;float>

    The load at which an internal datastructure resizes

- --generalize

    Once all possibilities for a column repeat the program knows it has found the general ruie for any board width and the pattern will repeat from the first occurance of a column. But even before that column it can happen that the rule turns out to be still valid even if the possibilities for an earlier column where not the same. If this option is true (the default) the rule is presented as starting on the earliest such column.

- --html &lt;file>

    Also write output to the given file as HTML.

- --stats

    Development option

    Print out some information about each inbetween state while a column is moved to the right

- --dump

    Development option

    Print the possible columns with their liberties

- --sizes

    Development option

    Print out the sizes of several C++ objects. Used during development to make sure these objects are laid out as expected.

- --debug\_max\_size

    Development option

    Internally at some point a datastructure needs to be sorted which is done using a counting sort. This prints some helper information to make sure this is an efficient way of sorting.

- --debug\_history

    Development option

    Show some information about the breadcrumbs that are left which will allow recovery of a [solution](#solution).

- --history\_bits &lt;n>

    Development option

    Use some other number of bits to remember breadcrumbs than the builtin value (which will normally be `16` or `8`). The value must be between `1` and the builtin and is used to check that the history/breadcrumbs algorithm can handle other bit values.

- --profile \[&lt;file>\]

    Run the program under the [google perftools profiler](http://goog-perftools.sourceforge.net/doc/cpu_profiler.html) with `file` as filename to dump the profile to. Defaults to `count_liberties.pprof`.

- --version

    Print version information

- --help, -h

    Print this help

# BUGS

None known

# SEE ALSO

The inspiration for this code is from John Tromp's [Number of legal Go positions](https://tromp.github.io/go/legal.html)
