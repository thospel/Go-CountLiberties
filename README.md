# Go::CountLiberties

1. [Overview](#overview)
2. [Compilation](#compilation)
3. [Invocation](#invocation)
4. [Output](#output)
5. [Results](#results)
6. [Hypothesis](#hypothesis)

## Overview

This program calculates the maximum number of liberties a single group can have on a go board of given height. It will output a table and rule which answers this question for any board width.

The inspiration for this code is from John Tromp's [Number of legal Go positions](https://tromp.github.io/go/legal.html)

## Compilation

The program was developed on linux using the gcc C++ compiler. While I tried to remain portable I never tried it on another environment so most likely it will only work with g++

The program needs

* [perl][perl]

  On [debian linux][debian] you probably already have it, but if not `apt install perl`

* [gnu C++ compiler][gcc]

  On [debian linux][debian] install with `apt install g++ make` or (pulls in more) `apt install build-essential`

* [google perftools][gperftools]

  On [debian linux][debian] install with `apt install libgoogle-perftools-dev`

* [jemalloc][jemalloc]

  On [debian linux][debian] install with `apt install libjemalloc-dev`

* Optionally [lock less malloc][lockless]

  There is no debian package, instead follow [these instructions][llmalloc]

The default version of the program supports board heights up to 20 (so it can easily work out the answer for a 19 x 19 board). The compile time variable `MAX_SIZE` in *CountLiberties.xs* can be increased up to 24 to answer the question for heights up to 24. The drawback is that in that case only 8 instead of 16 bits are available to record how a solution got constructed, so recovering an actual solution (the `--solution` option) will take a lot longer.

The algorithm in principle also works for boards higher than 24 rows but the current code would have to changed. Memory usage and speed also become quite painful at this point. The general rule of the answer also seems quite clear by this point, so I currently have no plans to implement larger boards.

Then do:

    perl Makefile.PL -y         # Prepare setup
    make                        # Actual compilation
    make test                   # Placeholder. No interesting tests are done
    make install                # Works, but why pollute you system ?
                                # You can use the --blib option to run from the
                                # compilation directory instead

A code contributor might also do (this is **not** needed to compile or run the program, this may need extra installs):

    make ppport                 # Update ppport.h for newer code or a new version of perl
    make invocation.md          # Make a new invocation markdown file
                                # (in case you added and documented new functionality)

## Invocation

See [invocation](invocation.md)

## Output

The basic output of a program run like:

    bin/count_liberties --blib 7

Looks something like this:

    git revision 5e5ad24200c0bee39c5922550587206d9b90f631, parent 3c531ec8ce9651ee6168f206b77582a8be408f39
    Height =  7, 7x7 will be in [29, 33], 4 threads
     width  1:   2 (   0.0 s,        19 columns, 08F54132D63BF9E9 sig,    61896 KiB)
     width  2:   7 (   0.0 s,        40 columns, 78A7D38C13098F50 sig,    61896 KiB)
     width  3:  14 (   0.0 s,        73 columns, 910993FBC5419212 sig,    61896 KiB)
     width  4:  16 (   0.0 s,        76 columns, 240C14546B2331CB sig,    61896 KiB)
     width  5:  20 (   0.0 s,        88 columns, 51B0941288AB0A2D sig,    61896 KiB)
     width  6:  26 (   0.0 s,        96 columns, 861C09B2AD04E73A sig,    61896 KiB)
     width  7:  29 (   0.0 s,        72 columns, 9224B0DCB00FD42D sig,    61896 KiB)
     width  8:  33 (   0.0 s,        93 columns, 677D79B13A168222 sig,    61896 KiB)
     width  9:  39 (   0.0 s,        95 columns, 58EF84EFDC2F8616 sig,    61896 KiB)
     width 10:  42 (   0.0 s,        72 columns, 9224B0DCB00FD42D sig,    61896 KiB)
      General rule for width >= 4: 16 [+4 +6 +3] (4 * 1.2s user, 1.5s real)

In the top line

    git revision 5e5ad24200c0bee39c5922550587206d9b90f631, parent 3c531ec8ce9651ee6168f206b77582a8be408f39

*parent* `3c531ec8ce9651ee6168f206b77582a8be408f39` is the git revision id of the last commit that was done before the code was compiled. The compilation may however not have been done on an unchanged checkout. The *revision* `5e5ad24200c0bee39c5922550587206d9b90f631` is a SHA1 hash over the state of the files known to git at the moment the compilation was done. It may look a like a git revision id, but it isn't. It is however deterministic, so it can be used to make sure that two outputs were generated using a program based on the exact same source files.

The next line

    Height =  7, 7x7 will be in [29, 33], 4 threads

indicates that the program will now look for the maximum number of liberties a group can have for boards of height 7 and widths from 1 to infinity. In this example it will be using *4* threads for this which usually gives a good speedup since the code parallelizes extremely well.

It will also do a prediction about in what range the answer will fall for a square board (7 x 7). The maximum is based on:

1. A straight string stones:

        ....
       .XXXX.        liberties = 2 * stones + 2
        ....

   A more complicated group will have corners and branches which will cause some liberties to overlap, so in general:

       liberties ≤ 2 * stones + 2

2. Any stone cannot be a liberty, so

       liberties ≤ area - stones

Working out the number of stones where these two constraints cross and substituting that back into the constraints gives the best we can hope for:

    liberties ≤ 2(area+1)/3

So on a 7 x 7 board we have

    liberties ≤ 2*(49+1)/3 = 100/3 = 33 + 1/3

Since liberties must be an integer that gives

    liberties ≤ 33

The lower bound of `29` is based on the group patterns given later in this text. The hypothesis is that these values are optimal, but even if not they at least give a group that realizes that number of of liberties.

Next you get a number of lines like:

     width  5:  20 (   1.2 s,        88 columns, 51B0941288AB0A2D sig,    61896 KiB)

This indicates that on the 5 x 7 board a group has at most 20 liberties. It took the program 1.2 seconds to reach that conclusion (starting from the information it had already collected about the 4 x 7 board). At the end of this calculation the program was using [`61896 KiB`][kibibyte] of memory (though this may include memory it isn't really using but has not been given back to the operating system). The last column on this 5 x 7 board can be in `88` different states (a state is a sequence of filled or empty intersection combined with how they are topologically connected and which empty intersections are liberties taking into account top/down symmetry). The `51B0941288AB0A2D` is a hash over all these possible columns. The word `sig` stands for *signature*.

These output lines will be repeated until the first time the final board column has the same hash as some earlier final board column. In this example that happens for the 10 x 7 board which has the same hash as the 7 x 7 board. This means we can stop now since from now on the pattern for columns 7 to 9 will repeat. Possible columns for 7 x 7 will be the same as for 10 x 7, 13 x 7, 16 x 7 etc. Possible columns for 8 x 7 will be the same as for 11 x 7, 14 x 7 etc. And the number of liberties will keep growing, but in the same repeated pattern. So from width 7 to 8 the number of liberties increases by `+4 = 33-29`, from width 8 to 9 it increases by `+6 = 39-33` and from width 9 to 10 it increases by `+3 = 42-39`. And these increases will repeat in the same pattern, so width 10 to 11 will be +4 again (so `45=42+3` liberties), width 11 to 12 will be +6 (so `51=45+6` liberties) and width 12 to 13 will be +3 (so `54=51+3` liberties). This pattern of `+4,+6,+3` will repeat ad infinitum.

Looking back to before column 7 we can see that the pattern of increases was already well established even though the set of possible column states had not yet stabilized. The pattern starts on the 4 x 7 board with `16` liberties and the increases from there are `+4, +6, +3, +4, +6, +3` etc. The program does this looking back step only if the `--generalize` option is true (which is the default).

So the final line

      General rule for width >= 4: 16 [+4 +6 +3] (4 * 1.2s user, 1.5s real)

tells us this general rule and also that the total runtime was 1.5 seconds (wallclock time) and that each thread worked on average for 1.2 seconds for the user program. So assuming 4 logical CPUs we kept our processor busy on our program for 1.2/1.5 = 80% of the time.

The first repeated column set can happen before after or exactly when the width is equal to the height. If repetition already happens before the square board and you have given the `--solution` option it will continue calculating columns and giving output lines until the square board is reached because it needs to build up history to be able to give examples of all board widths up to the square board. Notice that this first repeated column must happen at some point (since there are only a finite number of possible column states) but there is no directly obvious reason why this shouldn't take very long. The program however only supports board heights up to 24 and the worst case is needing to go up to column 28 (for height 24).

Strictly speaking the program doesn't detect that the set of column states repeats, only that the hash of the set of column states repeats, so the detection of repeated patterns could be based on false positives (hash collision). This could be fixed by dumping and comparing the full column state sets but this would take a lot of diskspace for large boards (about 40Gib for a 24 x 24 board). This is rather theoretical, but I may add an option for this at some later point.

If the `--solution` option is given a phase starts where the program will show a sample group that realizes the maximum number of liberties for all board widths ≤ board height. It mostly tries to work backwards from the square board to the width 1 board. Unfortunately by the time the program printed the information about the last column, it has (mostly) forgotten anything it knew about the previous columns. However the internal datastructures have some unused bits in which the program records a little bit of information about how for a given column the highest amount of liberties can be reached. These bits record whether some positions are filled with a stone or not. For a m x n board it will need `m*n` bits to fully know the group layout. So the program will now repeatedly do the forward deduction from column 1 to the target width using the bits (stone or empty) it already knows to work out the value of some unknown bits. Since less and less columns will be allowed by the already known bits the sets of possible columns will get smaller and smaller and the runs will become faster and faster.

For the first repeat run the program decides to work out a solution for the 7 x 7 board and (though it doesn't say so) could already deduce 21 bits and still needs 28 bits (`21+28=49=7*7`). The output for the first new forward run is:

     width  1:   2 (   0.0 s,        19 columns, 08F54132D63BF9E9 sig,    61896 KiB)
     width  2:   7 (   0.0 s,        10 columns, 76B9BCD7D6991697 sig,    61896 KiB)
     width  3:  13 (   0.0 s,         9 columns, 90EFE2F2BBA46AF6 sig,    61896 KiB)
     width  4:  16 (   0.0 s,        24 columns, DD5B96BC7A36D611 sig,    61896 KiB)
     width  5:  20 (   0.0 s,        21 columns, ECBDAFE6E8A02572 sig,    61896 KiB)
     width  6:  24 (   0.0 s,        20 columns, 118BED7753629E27 sig,    61896 KiB)
     width  7:  29 (   0.0 s,         6 columns, 6B28D71943FAE766 sig,    61896 KiB)
       7x 7: 29 (4 * 0.0s user, 0.0s real), still need 12 bits

The lines basically mean the same thing as during the initial run. You can however see that due to the constraint of the already known bits the possible number of columns at each board width may be lower. The highest number of liberties found for intermediate board width can be wrong now. For example the 6 x 7 board can have at most `24` liberties instead of the correct answer of `26`. This is because the program is applying the constraints for the solution on a 7 x 7 board and none of the optimal solutions for a 6 x 7 board are compatible with that. Compatible layouts can do no better than `24` liberties on a 6 x 7 board.

After this there are still 12 unknown bits, meaning there are 12 intersections where the program still doesn't know it should put a stone or not. Since this version of the code will gain at least 16 bits per run this means the next run will be the last one. The output is:

       7x 7: 29 (4 * 0.0s user, 0.0s real), still need 12 bits
     width  1:   2 (   0.0 s,         4 columns, 506F284BFE0E5A1C sig,    61896 KiB)
     width  2:   7 (   0.0 s,         2 columns, 5F6A778404824A0E sig,    61896 KiB)
     width  3:  12 (   0.0 s,         2 columns, 89AAEE9DB212A1CB sig,    61896 KiB)
     width  4:  16 (   0.0 s,         4 columns, 89290FD0030A5864 sig,    61896 KiB)
     width  5:  20 (   0.0 s,         6 columns, E6C67DE53E29CB81 sig,    61896 KiB)
     width  6:  24 (   0.0 s,         5 columns, FBE896754284809A sig,    61896 KiB)
     width  7:  29 (   0.0 s,         1 columns, 7A5FCB6CEF879B9C sig,    61896 KiB)
       7x 7: 29 (4 * 0.0s user, 0.0s real)

At this point the program knows an optimal layout for the 7 x 7 board. It will however check to see that if it cuts this solution at columns 1, 2, 3 etc if that also happens to give one of the optimal solutions for that board width. It didn't really optimize for this, but quite often the program gets lucky. That is the case here and it got the 2 x 7, 4 x 7 and 5 x 7 boards for free. These are printed first:

    Extra solution 2x7: 7 liberties
    ****
    *.O*
    *.O*
    *.O*
    *.O*
    *.O*
    *.O*
    *.O*
    ****

    Extra solution 4x7: 16 liberties
    ******
    *.O..*
    *.OOO*
    *.O..*
    *.O..*
    *.O.O*
    *.OOO*
    *.O..*
    ******

    Extra solution 5x7: 20 liberties
    *******
    *.O...*
    *.OOOO*
    *.O...*
    *.O...*
    *.O.OO*
    *.OOO.*
    *.O...*
    *******

where `O` is a stone, `.` is an empty point and `*` is the board edge.

Only then does it print the actual 7 x 7 solution and how long this solution took:

    Solution 7x7: 29 liberties (4 * 0.0s user, 0.0s real)
    *********
    *.O.....*
    *.OOOOOO*
    *.O....O*
    *.O.....*
    *.O.OOO.*
    *.OOO.O.*
    *.O...O.*
    *********

Notice that the extra solutions are indeed derived from this layout by slicing off columns from the board.

The widest board narrower than a square it hasn't worked out yet is 6 x 7, so the program now tries to reconstruct a solution for that size. This again needs 2 runs:

    Going for 6x7, already know 20 bits, still need 22 bits
     width  1:   2 (   0.0 s,        19 columns, 08F54132D63BF9E9 sig,    61896 KiB)
     width  2:   7 (   0.0 s,        10 columns, 76B9BCD7D6991697 sig,    61896 KiB)
     width  3:  14 (   0.0 s,         9 columns, EA9DF7C5E8DE96D8 sig,    61896 KiB)
     width  4:  16 (   0.0 s,        22 columns, 160613C9D56AE5FB sig,    61896 KiB)
     width  5:  20 (   0.0 s,        17 columns, FF4608EE1C621AA1 sig,    61896 KiB)
     width  6:  26 (   0.0 s,         1 columns, 65E15D69D86F4850 sig,    61896 KiB)
       6x 7: 26 (4 * 0.0s user, 0.0s real), still need 6 bits
     width  1:   2 (   0.0 s,         4 columns, 506F284BFE0E5A1C sig,    61896 KiB)
     width  2:   7 (   0.0 s,         2 columns, 5F6A778404824A0E sig,    61896 KiB)
     width  3:  13 (   0.0 s,         2 columns, D83C1611B489B73C sig,    61896 KiB)
     width  4:  15 (   0.0 s,         3 columns, CAA1D65F6F5B83CC sig,    61896 KiB)
     width  5:  20 (   0.0 s,         2 columns, 8C827CC1FF4E62B1 sig,    61896 KiB)
     width  6:  26 (   0.0 s,         1 columns, 669B547BBC0D2DAF sig,    61896 KiB)
       6x 7: 26 (4 * 0.0s user, 0.0s real)

Notice that this time the number of liberties for the 6 x 7 is optimal again since now it is using the proper constraints for that size, but narrower boards can be wrong again.

This time there happen to be no free solutions by cutting so we only get the targeted 6 x 7 solution:

    Solution 6x7: 26 liberties (4 * 0.0s user, 0.0s real)
    ********
    *.O..O.*
    *.O..O.*
    *.O..O.*
    *.O..O.*
    *.O..O.*
    *.OOOO.*
    *.O....*
    ********

Board sizes 5 x 7 and 4 x 7 were already found, so the next missing board size is 3 x 7. Most bits are already known so this needs only 1 run:

    Going for 3x7, already know 18 bits, still need 3 bits
     width  1:   2 (   0.0 s,         4 columns, 506F284BFE0E5A1C sig,    61896 KiB)
     width  2:   7 (   0.0 s,         2 columns, 5F6A778404824A0E sig,    61896 KiB)
     width  3:  14 (   0.0 s,         1 columns, AF95D5F739438F2D sig,    61896 KiB)
       3x 7: 14 (4 * 0.0s user, 0.0s real)

Again no lucky free solutions so only:

    Solution 3x7: 14 liberties (4 * 0.0s user, 0.0s real)
    *****
    *.O.*
    *.O.*
    *.O.*
    *.O.*
    *.O.*
    *.O.*
    *.O.*
    *****

We already had a solution for the 2 x 7 board so the only one left is the 1 x 7 board. And previous runs allowed the program to already know 7 bits so actually the whole board layout is known without any calculations. So it immediately prints:

    Solution 1x7: 2 liberties (4 * 0.0s user, 0.0s real)
    ***
    *.*
    *.*
    *.*
    *.*
    *O*
    *.*
    *.*
    ***

And finally it prints how long the whole run took:

     Generated all boards with height  7: (4 * 0.0s user, 0.0s real)

## Results

A table for all board sizes up to 24 x 24. Since it also has the general rule you can in fact work out the solution for any width board with a height of 24 or less

| | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | | from | rule | repeat | |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- | ---: | :--- | ---: | ---: |
| **1** | _`0`_ | 1 | **2** | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | 2 | | 3 | +0 | 4 | **1** |
| **2** | 1 | _`2`_ | 4 | **4** | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | | 4 | +1 | 5 | **2** |
| **3** | **2** | 4 | _`6`_ | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 | 34 | 36 | 38 | 40 | 42 | 44 | 46 | 48 | | 1 | +2 | 4 | **3** |
| **4** | 2 | 4 | 8 | **_`9`_** | **11** | **14** | 16 | 18 | 21 | 23 | 25 | 28 | 30 | 32 | 35 | 37 | 39 | 42 | 44 | 46 | 49 | 51 | 53 | 56 | | 4 | +2,+3,+2 | 5 | **4** |
| **5** | 2 | 5 | 10 | **11** | **_`14`_** | **18** | 20 | 23 | 27 | 29 | 32 | 36 | 38 | 41 | 45 | 47 | 50 | 54 | 56 | 59 | 63 | 65 | 68 | 72 | | 4 | +3,+4,+2 | 6 | **5** |
| **6** | 2 | 6 | 12 | **14** | 18 | _`22`_ | 26 | 30 | 34 | 38 | 42 | 46 | 50 | 54 | 58 | 62 | 66 | 70 | 74 | 78 | 82 | 86 | 90 | 94 | | 4 | +4 | 6 | **6** |
| **7** | 2 | 7 | 14 | **16** | **20** | **26** | _`29`_ | 33 | 39 | 42 | 46 | 52 | 55 | 59 | 65 | 68 | 72 | 78 | 81 | 85 | 91 | 94 | 98 | 104 | | 4 | +4,+6,+3 | 7 | **7** |
| **8** | 2 | 8 | 16 | **18** | **23** | **30** | 33 | _`38`_ | 45 | 48 | 53 | 60 | 63 | 68 | 75 | 78 | 83 | 90 | 93 | 98 | 105 | 108 | 113 | 120 | | 4 | +5,+7,+3 | 8 | **8** |
| **9** | 2 | 9 | 18 | 21 | 27 | 34 | **39** | 45 | _`51`_ | 57 | 63 | 69 | 75 | 81 | 87 | 93 | 99 | 105 | 111 | 117 | 123 | 129 | 135 | 141 | | 7 | +6 | 9 | **9** |
| **10** | 2 | 10 | 20 | **23** | **29** | **38** | 42 | 48 | 57 | _`61`_ | 67 | 76 | 80 | 86 | 95 | 99 | 105 | 114 | 118 | 124 | 133 | 137 | 143 | 152 | | 4 | +6,+9,+4 | 8 | **10** |
| **11** | 2 | 11 | 22 | **25** | **32** | **42** | 46 | 53 | 63 | 67 | _`74`_ | 84 | 88 | 95 | 105 | 109 | 116 | 126 | 130 | 137 | 147 | 151 | 158 | 168 | | 4 | +7,+10,+4 | 8 | **11** |
| **12** | 2 | 12 | 24 | 28 | 36 | 46 | 52 | 60 | 69 | **76** | 84 | _`92`_ | 100 | 108 | 116 | 124 | 132 | 140 | 148 | 156 | 164 | 172 | 180 | 188 | | 10 | +8 | 12 | **12** |
| **13** | 2 | 13 | 26 | **30** | **38** | **50** | 55 | 63 | 75 | 80 | 88 | 100 | _`105`_ | 113 | 125 | 130 | 138 | 150 | 155 | 163 | 175 | 180 | 188 | 200 | | 4 | +8,+12,+5 | 9 | **13** |
| **14** | 2 | 14 | 28 | **32** | **41** | **54** | 59 | 68 | 81 | 86 | 95 | 108 | 113 | _`122`_ | 135 | 140 | 149 | 162 | 167 | 176 | 189 | 194 | 203 | 216 | | 4 | +9,+13,+5 | 10 | **14** |
| **15** | 2 | 15 | 30 | 35 | 45 | 58 | 65 | 75 | 87 | 95 | 105 | 116 | **125** | 135 | _`145`_ | 155 | 165 | 175 | 185 | 195 | 205 | 215 | 225 | 235 | | 13 | +10 | 16 | **15** |
| **16** | 2 | 16 | 32 | **37** | **47** | **62** | 68 | 78 | 93 | 99 | 109 | 124 | 130 | 140 | 155 | _`161`_ | 171 | 186 | 192 | 202 | 217 | 223 | 233 | 248 | | 4 | +10,+15,+6 | 12 | **16** |
| **17** | 2 | 17 | 34 | **39** | **50** | **66** | 72 | 83 | 99 | 105 | 116 | 132 | 138 | 149 | 165 | 171 | _`182`_ | 198 | 204 | 215 | 231 | 237 | 248 | 264 | | 4 | +11,+16,+6 | 12 | **17** |
| **18** | 2 | 18 | 36 | 42 | 54 | 70 | 78 | 90 | 105 | 114 | 126 | 140 | 150 | 162 | 175 | **186** | 198 | _`210`_ | 222 | 234 | 246 | 258 | 270 | 282 | | 16 | +12 | 19 | **18** |
| **19** | 2 | 19 | 38 | **44** | **56** | **74** | 81 | 93 | 111 | 118 | 130 | 148 | 155 | 167 | 185 | 192 | 204 | 222 | _`229`_ | 241 | 259 | 266 | 278 | 296 | | 4 | +12,+18,+7 | 18 | **19** |
| **20** | 2 | 20 | 40 | **46** | **59** | **78** | 85 | 98 | 117 | 124 | 137 | 156 | 163 | 176 | 195 | 202 | 215 | 234 | 241 | _`254`_ | 273 | 280 | 293 | 312 | | 4 | +13,+19,+7 | 15 | **20** |
| **21** | 2 | 21 | 42 | 49 | 63 | 82 | 91 | 105 | 123 | 133 | 147 | 164 | 175 | 189 | 205 | 217 | 231 | 246 | **259** | 273 | _`287`_ | 301 | 315 | 329 | | 19 | +14 | 23 | **21** |
| **22** | 2 | 22 | 44 | **51** | **65** | **86** | 94 | 108 | 129 | 137 | 151 | 172 | 180 | 194 | 215 | 223 | 237 | 258 | 266 | 280 | 301 | _`309`_ | 323 | 344 | | 4 | +14,+21,+8 | 24 | **22** |
| **23** | 2 | 23 | 46 | **53** | **68** | **90** | 98 | 113 | 135 | 143 | 158 | 180 | 188 | 203 | 225 | 233 | 248 | 270 | 278 | 293 | 315 | 323 | _`338`_ | 360 | | 4 | +15,+22,+8 | 17 | **23** |
| **24** | 2 | 24 | 48 | 56 | 72 | 94 | 104 | 120 | 141 | 152 | 168 | 188 | 200 | 216 | 235 | 248 | 264 | 282 | 296 | 312 | 329 | **344** | 360 | _`376`_ | | 22 | +16 | 27 | **24** |
| | **1** | **2** | **3** | **4** | **5** | **6** | **7** | **8** | **9** | **10** | **11** | **12** | **13** | **14** | **15** | **16** | **17** | **18** | **19** | **20** | **21** | **22** | **23** | **24** | | from | rule | repeat | |

The start of the repeated pattern is indicated in **bold**. Square boards are indicated in *`italic`*

The table must obviously be symmetric and it's a nice check of the program that it indeed is.

The obvious question was of course what the answer is for the standard 19 x 19 board which from the table you can see is *229*.

![Sample solution][19x19]

Many people already had constructed such solutions by hand, strongly suspecting but not being really sure that they were optimal. For example https://senseis.xmp.net/?GroupWithMostLiberties

A lot of regularity is visible in the table, for example where the repeated patterns start and their values. This will easily allow you to predict values starting from column 4 except for the lines that are a multiple of 3. For the lines that are a multiple of 3 we can use the symmetry of the table and find all values except where the columns are a multiple of 3 too. Basically we seem to have boards where the smallest size is 3 or less as special cases and a pattern depending on the width and height modulo 3. By also observing the solutions for these cases a pattern becomes visible.

Consider m x n boards with `m ≤ n`. We then have as cases:

* `m=1` (proven)

  Sample solutions:

      ***   ***   ***   ***   ***
      *O*   *O*   *.*   *.*   *.*
      ***   *.*   *O*   *O*   *O*
            ***   *.*   *O*   *O*
                  ***   *.*   *O*
                        ***   *.*
                              ***

  This sequence generalizes well and gives:

  * `n=1`: `0`

    This is suicide and cannot be a final state in a real game. But all rule sets define suicide in terms of a group without liberties so they still acknowledge the concept.

  * `n=2`: `1`
  * `n≥3`: `2`

  Running the program for height 1 agrees

* `m=2` (proven)

  Sample solutions:

      ****   ****   ****   ****   ****
      *O.*   *O.*   *..*   *O.*   *O.*
      ****   *O.*   *OO*   *O.*   *O.*
             ****   *..*   *O.*   *O.*
                    ****   *O.*   *O.*
                           ****   *O.*
                                  ****

  This sequence generalizes well and gives:

  * `n=3`: `4` (the exceptional case)
  * `n≠3`: `n`

  Running the program for height 2 agrees

* `m=3` (proven)

  Sample solutions:

      *****   *****   *****   *****
      *.O.*   *.O.*   *.O.*   *.O.*
      *****   *.O.*   *.O.*   *.O.*
              *****   *.O.*   *.O.*
                      *****   *.O.*
                              *****

  This sequence generalizes well and gives:

  * `n`:  `2n`

  Running the program for height 3 agrees

* `m≥4` (proven for `m≤24`)

  Here we leave the exceptional sizes and have to establish the pattern

  * `m≡0 (mod 3)`

    Sample solutions:

        ***********   ***********   ***********   **************
        *.........*   *.........*   *.........*   *............*
        *.OOOOOOO.*   *.OOOOOOO.*   *.OOOOOOO.*   *.OOOOOOOOOO.*
        *.O..O..O.*   *.O..O..O.*   *.O..O..O.*   *.O..O..O..O.*
        *.O..O..O.*   *.O..O..O.*   *.O..O..O.*   *.O..O..O..O.*
        *.O..O..O.*   *.O..O..O.*   *.O..O..O.*   *.O..O..O..O.*
        *.O..O..O.*   *.O..O..O.*   *.O..O..O.*   *.O..O..O..O.*
        *.O..O..O.*   *.O..O..O.*   *.O..O..O.*   *.O..O..O..O.*
        *.O..O..O.*   *.O..O..O.*   *.O..O..O.*   *.O..O..O..O.*
        *.O..O..O.*   *.O..O..O.*   *.O..O..O.*   *.O..O..O..O.*
        *.O..O..O.*   *.O..O..O.*   *.O..O..O.*   *.O..O..O..O.*
        ***********   *.O..O..O.*   *.O..O..O.*   *.O..O..O..O.*
                      ***********   *.O..O..O.*   *.O..O..O..O.*
                                    ***********   **************

    So the preference is to make the connecting bar run along the short side of board if that short side has a width that is a multiples of 3. This sequence generalizes well and gives:

    * `n`: `m(2n-1)/3`

    Running the program for heights up to 24 agrees

  * `m≢0 (mod 3)` and `n≡0 (mod 3)`

    Sample solutions:

        ******   *******   *********   **********
        *....*   *.....*   *.......*   *........*
        *.OOO*   *.OOOO*   *.OOOOOO*   *.OOOOOOO*
        *.O..*   *.O...*   *.O.....*   *.O......*
        *.O..*   *.O...*   *.O.....*   *.O......*
        *.OOO*   *.OOOO*   *.OOOOOO*   *.OOOOOOO*
        *.O..*   *.O...*   *.O.....*   *.O......*
        *.O..*   *.O...*   *.O.....*   *.O......*
        *.OOO*   *.OOOO*   *.OOOOOO*   *.OOOOOOO*
        *.O..*   *.O...*   *.O.....*   *.O......*
        *.O..*   *.O...*   *.O.....*   *.O......*
        *.OOO*   *.OOOO*   *.OOOOOO*   *.OOOOOOO*
        *....*   *.....*   *.......*   *........*
        ******   *******   *********   **********

    So if the short side is not a multiple of 3 but the long side is then make the connection bar run along the long side. This sequence generalizes well and gives:

    * `n`: `n(2m-1)/3`

    Running the program for heights up to 24 agrees

  * `m≡1 (mod 3)` and `n≡1 (mod 3)`

    Sample solutions:

        ******   ******   ******   *********   ************
        *....*   *....*   *....*   *.......*   *..........*
        *.OOO*   *.OOO*   *.OOO*   *.OOOOOO*   *.OOOOOOOOO*
        *.O..*   *.O..*   *.O..*   *.O..O..*   *.O..O..O..*
        *.O..*   *.O..*   *.O..*   *.O..O..*   *.O..O..O..*
        *.OOO*   *.OOO*   *.OOO*   *.O..OOO*   *.O..O..OOO*
        *.O..*   *.O..*   *.O..*   *.O..O..*   *.O..O..O..*
        *.O..*   *.O..*   *.O..*   *.O..O..*   *.O..O..O..*
        ******   *.OOO*   *.OOO*   *.O..OOO*   *.O..O..OOO*
                 *.O..*   *.O..*   *.O..O..*   *.O..O..O..*
                 *.O..*   *.O..*   *.O..O..*   *.O..O..O..*
                 ******   *.OOO*   *********   ************
                          *.O..*
                          *.O..*
                          ******

    Here are some alternative extendable ways to implement this case, demonstrated for 19 x 19:

        *********************   *********************
        *...................*   *...................*
        *.OOOOOOOOOOOOOOOOOO*   *.OOOOOOOOOOOOOOOOO.*
        *.O.................*   *.O..O..O..O..O...O.*
        *.O.................*   *.O..O..O..O..O...O.*
        *.OOOOOOOOOOOOOOOOOO*   *.O..O..O..O..O.OOO.*
        *.O.................*   *.O..O..O..O..O...O.*
        *.O.................*   *.O..O..O..O..O...O.*
        *.OOOOOOOOOOOOOOOOOO*   *.O..O..O..O..O.OOO.*
        *.O..O.....O.....O..*   *.O..O..O..O..O...O.*
        *.O..O..O..O..O..O..*   *.O..O..O..O..O...O.*
        *.O.....O.....O.....*   *.O..O..O..O..O.OOO.*
        *.OOOOOOOOOOOOOOOOOO*   *.O..O..O..O..O...O.*
        *.O.................*   *.O..O..O..O..O...O.*
        *.O.................*   *.O..O..O..O..O.OOO.*
        *.OOOOOOOOOOOOOOOOOO*   *.O..O..O..O..O...O.*
        *.O.................*   *.O..O..O..O..O...O.*
        *.O.................*   *.O..O..O..O..O.OOO.*
        *.OOOOOOOOOOOOOOOOOO*   *.O..O..O..O..O...O.*
        *...................*   *.O..O..O..O..O...O.*
        *********************   *********************

    All these sequences generalizes well and give:

    * `n`: `(n(2m-1)-m+3)/3 = ((2m-1)(2n-1)+5)/6`

    Running the program for heights up to 24 agrees

  * `m≡1 (mod 3)` and `n≡2 (mod 3)`

    Sample solutions:

        ******   ******   ******   *********   ************
        *....*   *....*   *....*   *.......*   *..........*
        *.OOO*   *.OOO*   *.OOO*   *.OOOOOO*   *.OOOOOOOOO*
        *.O..*   *.O..*   *.O..*   *.O.....*   *.O........*
        *.OOO*   *.OOO*   *.OOO*   *.OOOOOO*   *.OOOOOOOOO*
        *.O..*   *.O..*   *.O..*   *.O.....*   *.O........*
        *.O..*   *.O..*   *.O..*   *.O.....*   *.O........*
        *.OOO*   *.OOO*   *.OOO*   *.OOOOOO*   *.OOOOOOOOO*
        *....*   *.O..*   *.O..*   *.O.....*   *.O........*
        ******   *.O..*   *.O..*   *.O.....*   *.O........*
                 *.OOO*   *.OOO*   *.OOOOOO*   *.OOOOOOOOO*
                 *....*   *.O..*   *.......*   *..........*
                 ******   *.O..*   *********   ************
                          *.OOO*
                          *....*
                          ******

    This sequence generalizes well and gives:

    * `n`: `((2m-1)(2n-1)+3)/6`

    Running the program for heights up to 24 agrees

  * `m≡2 (mod 3)` and `n≢0 (mod 3)`

    Sample solutions:

        *******   *******   *******   **********   *************
        *.....*   *.....*   *.....*   *........*   *...........*
        *.OOO.*   *.OOO.*   *.OOO.*   *.OOOOOO.*   *.OOOOOOOOO.*
        *.O.O.*   *.O.O.*   *.O.O.*   *.O..O.O.*   *.O..O..O.O.*
        *.O.O.*   *.O.O.*   *.O.O.*   *.O..O.O.*   *.O..O..O.O.*
        *.O.O.*   *.O.O.*   *.O.O.*   *.O..O.O.*   *.O..O..O.O.*
        *.O.O.*   *.O.O.*   *.O.O.*   *.O..O.O.*   *.O..O..O.O.*
        *.O.O.*   *.O.O.*   *.O.O.*   *.O..O.O.*   *.O..O..O.O.*
        *******   *.O.O.*   *.O.O.*   *.O..O.O.*   *.O..O..O.O.*
                  *******   *.O.O.*   *.O..O.O.*   *.O..O..O.O.*
                            *.O.O.*   *.O..O.O.*   *.O..O..O.O.*
                            *******   **********   *************

    This sequence generalizes well and again gives:

    * `n`: `((2m-1)(2n-1)+3)/6`

    Running the program for heights up to 24 agrees

    Notice that this combines with the previous case as:

    If all sides are larger than 3, any side is `2 (mod 3)` and the other side isn't a multiple of 3 then put a connecting bar along the `2 (mod 3)` side and have tines at distance 3 except for once at distance 2.

Since running the program for all heights up to 24 agrees with these formulas this actually **proves** them for `m ≤ 24` and `m ≤ n` (remember that the program **proves** its result for a given board height for **all** widths). Here I must again admit that the program only is a real proof once I replace the hash compare by a full set compare. But the chance that your computer is hit by cosmic rays and makes a mistake is larger than such a false positive (especially since I used several different hash algorithms at one time or another which would all need to give false positives).

## Hypothesis

My hypothesis is that these cases are optimal for all board sizes (since the patterns given above cover all board sizes, at least seem pretty optimal and indeed **are** optimal up to size 24).

A sample implementation in perl:

```perl
sub liberties {
    # Hypothesis: This program is valid for any board size
    my ($m, $n) = @_;
    ($m, $n) = ($n, $m) if $m > $n;
    # So now $m <= $n
    # This program is certain to be valid for all $m <= 24
    if ($m >= 4) {
        return $m*(2*$n-1)/3 if $m % 3 == 0;
        return $n*(2*$m-1)/3 if $n % 3 == 0;
        return ((2*$m-1)*(2*$n-1)+5)/6 if $m % 3 == 1 && $n % 3 == 1;
        return ((2*$m-1)*(2*$n-1)+3)/6; # if $m % 3 == 2 || $n % 3 == 2
    }
    return 2*$n if $m == 3;
    return $n == 3 ? 4 : $n if $m == 2;
    return $n >= 3 ? 2 : $n-1 if $m == 1;

    die "Bad call";
}
```

This code exactly duplicates the results table. In fact it's used in the *count_liberties* program to calculate the lower bound and the program will output a warning message if the actual generated value differs from this lower bound.

[perl]: https://www.perl.org/
[debian]: https://www.debian.org/
[gcc]: https://gcc.gnu.org/
[gperftools]: https://github.com/gperftools/gperftools
[jemalloc]: http://jemalloc.net/
[lockless]: https://locklessinc.com/
[llmalloc]: https://locklessinc.com/install_linux.shtml
[19x19]: http://i.imgur.com/m4YhgzI.png
[kibibyte]: https://en.wikiyy.com/wiki/KiB
