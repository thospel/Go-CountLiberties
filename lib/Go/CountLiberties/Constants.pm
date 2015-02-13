package Go::CountLiberties::Constants;
use 5.008001;
use strict;
use warnings;

our $VERSION = '1.000';	# $Revision: 5609 $

use Exporter::Tidy
    other =>
    [qw(OFFSET_X MAX_SIZE_X MAX_SIZE_Y BLACK_SHIFT WHITE_SHIFT UP DOWN LEFT
        RIGHT DOWN_LEFT LEFT_UP UP_RIGHT RIGHT_DOWN COLOR_SHIFT MAX_CHAINS
        EMPTY BLACK WHITE EDGE COLOR_MAX COLORS ALL_DIRECTIONS SAFE_STONES
        UP_SHIFT DOWN_SHIFT RIGHT_SHIFT LEFT_SHIFT
        ORTHOGONAL_SHIFT DIAGONAL_SHIFT
        UP_RIGHT_SHIFT DOWN_LEFT_SHIFT RIGHT_DOWN_SHIFT LEFT_UP_SHIFT
        PATTERN_DIRECTION_BITS
	BACKGROUNG_CHAR_VISIBLE
	BACKGROUND_CHAR EMPTY_CHAR BLACK_CHAR WHITE_CHAR EDGE_CHAR
        DEBUG_CHECK DEBUG_BOARD DEBUG_DEBUG_BOARD DEBUG_MOVES DEBUG_RAND_STATE
	DEBUG_COUNT3X3 DEBUG_SELF_ATARI

        FEATURE_CONTIGUOUS FEATURE_ATARI_CAPTURE
	FEATURE_ATARI_CAPTURE_SELF_ATARI FEATURE_ATARI_EXTENSION
        FEATURE_ATARI_EXTENSION_SELF_ATARI FEATURE_SOLVE_KO
        FEATURE_TWO_LIBERTIES
	NR_FEATURES

        LITTLE_ENDIAN BIG_ENDIAN BYTE_ORDER
    )];

sub naive {
    my ($m, $n) = @_;

    return int(($m+2)*($n+2)/5-4);
}

sub gamma {
    my ($m, $n) = @_;

    ($m, $n) = ($n, $m) if $m > $n;

    my $result;
    if ($m == 1) {
        $result = ($n+2) / 3;
    } elsif ($m ==  2) {
        $result = ($n+2)/2;
    } elsif ($m ==  3) {
        $result = (3*$n+4) / 4;
    } elsif ($m ==  4) {
        $result = $n;
        $result++ if $n == 5 || $n == 6 || $n == 9;
    } elsif ($m ==  5) {
        $result = (6*$n+8)/5;
        $result = 9 if $n == 7;
    } elsif ($m ==  6) {
        if ($n % 7 == 1) {
            $result = (10*$n+10)/7;
        } else {
            $result = (10*$n+12)/7;
        }
    } elsif ($m ==  7) {
        $result = (5*$n+3)/3;
    } elsif ($m ==  8) {
        $result = (15*$n+14)/8;
    } elsif ($m ==  9) {
        $result = (23*$n+20)/11;
    } elsif ($m == 10) {
        if ($n % 13 == 0 || $n % 13 == 3 and $n != 13 && $n != 16) {
            $result = (30*$n+37)/13;
        } else {
            $result = (30*$n+24)/13;
        }
    } elsif ($m == 11) {
        if ($n == 11 || $n == 18 || $n == 20 || $n == 22 || $n == 33) {
            $result = (38*$n+21)/15;
        } else {
            $result = (38*$n+36)/15;
        }
    } elsif ($m == 12) {
        $result = (80*$n+66)/29;
    } elsif ($m == 13) {
        if ($n % 33 == 14 || $n % 33 == 15 || $n % 33 == 17 || $n % 33 == 20) {
            $result = (98*$n+111)/33;
        } else {
            $result = (98*$n+78)/33;
        }
    } elsif ($m == 14) {
        if ($n % 22 == 18) {
            $result = (35*$n+40)/11;
        } else {
            $result = (35*$n+29)/11;
        }
    } elsif ($m == 15) {
        if ($n % 26 == 5) {
            $result = (44*$n+27)/13;
        } else {
            $result = (44*$n+40)/13;
        }
    } else {
        $result = naive($m, $n);
    }
    return int($result);
}

sub chains {
    my ($m, $n) = @_;

    return $m*$n - gamma($m, $n);
}

# Round up to the next power of two
sub pow2 {
    my $value = shift;
    my $n = log($value) / log(2);
    return $value if $n == int($n);
    return 2**int($n+1);
}

my $endian = unpack("L", pack "N", 0x04030201);
$endian == 0x04030201 || $endian == 0x01020304 ||
    die sprintf("Unhandled endianness %08x", $endian);

use constant {
    LITTLE_ENDIAN	=> 1234,
    BIG_ENDIAN		=> 4321,

    GUARD_X		=> 1,
    GUARD_Y		=> 1,

    # Don't just increase sizes above 19. There are several places assuming
    # certain limits on these values.
    #  - Converting board-coordinates to strings assumes MAX_SIZE_X <= 25
    #  - The Chain datastructure assumes the number of liberties fits in 16 bits
    MAX_SIZE_X		=> 19,
    MAX_SIZE_Y		=> 19,

    # Dividing this by the board area gives how many mcts descents we by
    # default do before checking the time. Given the playout speed this should
    # work around to (within an order of magnitude) one check every 0.010 s
    MCTS_DESCENTS_AREA	=> 50000,

    # Half for black and 0.5 mercy compensation, so 6.5 komi for white
    DEFAULT_IKOMI		=> -12,
    DEFAULT_NOMERCY		=> 0.3,
    DEFAULT_CLARITY		=> 0.057,
    DEFAULT_RAVE_BIAS		=> 1500.0,
    DEFAULT_EXPANSION		=> 12,
    DEFAULT_SCORE_BONUS		=> 0.1,
    DEFAULT_SCORE_SCALE		=> 1,

    MAX_PASSES		=> 2,	# Game ends after this many passes

    BLACK_SHIFT		=> 0,
    WHITE_SHIFT		=> 8,

    ORTHOGONAL_SHIFT	=> 0,
    DIAGONAL_SHIFT	=> 4,

    DEBUG_CHECK		=>  1,
    DEBUG_BOARD		=>  2,
    DEBUG_DEBUG_BOARD	=>  4,
    DEBUG_MOVES		=>  8,
    DEBUG_RAND_STATE	=> 16,
    DEBUG_COUNT3X3	=> 32,
    DEBUG_SELF_ATARI	=> 64,

    FEATURE_CONTIGUOUS			=> 0,
    FEATURE_ATARI_CAPTURE		=> 1,
    FEATURE_ATARI_CAPTURE_SELF_ATARI	=> 2,
    FEATURE_ATARI_EXTENSION		=> 3,
    FEATURE_ATARI_EXTENSION_SELF_ATARI	=> 4,
    FEATURE_SOLVE_KO			=> 5,
    FEATURE_TWO_LIBERTIES		=> 6,
    NR_FEATURES				=> 7,
};

use constant {
    BYTE_ORDER		=> unpack("L",pack"N",0x04030201) == 0x01020304 ? LITTLE_ENDIAN : BIG_ENDIAN,

    # OFFSET_X is the offset to the next row. Must be at least
    # MAX_SIZE_X+GUARD_X but is alllowed to be larger if you want some special
    # alignment for the next row
    OFFSET_X		=> MAX_SIZE_X+GUARD_X,
    # OFFSET_X		=> 32,

    MAX_AREA		=> MAX_SIZE_X * MAX_SIZE_Y,

    UP_SHIFT		=> ORTHOGONAL_SHIFT + 0,
    DOWN_SHIFT		=> ORTHOGONAL_SHIFT + 1,
    LEFT_SHIFT		=> ORTHOGONAL_SHIFT + 2,
    RIGHT_SHIFT		=> ORTHOGONAL_SHIFT + 3,

    # The diagonal shifts have been carefully chosen so that the resulting
    # neighbour_index table is as small as possible (doesn't gain much)
    RIGHT_DOWN_SHIFT	=> DIAGONAL_SHIFT + 0,
    DOWN_LEFT_SHIFT	=> DIAGONAL_SHIFT + 1,
    UP_RIGHT_SHIFT	=> DIAGONAL_SHIFT + 2,
    LEFT_UP_SHIFT	=> DIAGONAL_SHIFT + 3,

    SAFE_STONES		=> 6,

    PATTERN_DIRECTION_BITS	=>  3,

    # Graph characters
    BACKGROUND_CHAR		=> " ",
    BACKGROUND_CHAR_VISIBLE	=> ".",
    EMPTY_CHAR			=> ".",
    BLACK_CHAR			=> "#",
    WHITE_CHAR			=> "O",
    EDGE_CHAR			=> "*",
};

use constant {
    MAX_RANGE	=> (MAX_SIZE_Y-1) * OFFSET_X + MAX_SIZE_X,

    UP		=> (1 << UP_SHIFT),
    DOWN	=> (1 << DOWN_SHIFT),
    LEFT	=> (1 << LEFT_SHIFT),
    RIGHT	=> (1 << RIGHT_SHIFT),

    DOWN_LEFT	=> (1 << DOWN_LEFT_SHIFT),
    LEFT_UP	=> (1 << LEFT_UP_SHIFT),
    UP_RIGHT	=> (1 << UP_RIGHT_SHIFT),
    RIGHT_DOWN	=> (1 << RIGHT_DOWN_SHIFT),

    EMPTY	=> 0,
    BLACK	=> 1,
    WHITE	=> 2,
    EDGE	=> 3,
    COLOR_MAX	=> 3,
    COLORS	=> 4,

    COLOR_SHIFT => BLACK_SHIFT || WHITE_SHIFT,
    MAX_CHAINS	=> chains(MAX_SIZE_X, MAX_SIZE_Y),

    PATTERN_DIRECTION_MASK	=>  ((1 << PATTERN_DIRECTION_BITS) -1),
};

use constant {
    MAX_RANGE2		=> pow2(MAX_RANGE),

    ALL_DIRECTIONS	=> UP | DOWN | LEFT | RIGHT,

    BLACK_DOWN		=> (DOWN	 << BLACK_SHIFT),
    BLACK_LEFT		=> (LEFT	 << BLACK_SHIFT),
    BLACK_UP		=> (UP	 << BLACK_SHIFT),
    BLACK_RIGHT		=> (RIGHT << BLACK_SHIFT),

    BLACK_DOWN_LEFT	=> (DOWN_LEFT	<< BLACK_SHIFT),
    BLACK_LEFT_UP	=> (LEFT_UP	<< BLACK_SHIFT),
    BLACK_UP_RIGHT	=> (UP_RIGHT	<< BLACK_SHIFT),
    BLACK_RIGHT_DOWN	=> (RIGHT_DOWN	<< BLACK_SHIFT),

    WHITE_DOWN		=> (DOWN	 << WHITE_SHIFT),
    WHITE_LEFT		=> (LEFT	 << WHITE_SHIFT),
    WHITE_UP		=> (UP	 << WHITE_SHIFT),
    WHITE_RIGHT		=> (RIGHT << WHITE_SHIFT),

    WHITE_DOWN_LEFT	=> (DOWN_LEFT	<< WHITE_SHIFT),
    WHITE_LEFT_UP	=> (LEFT_UP	<< WHITE_SHIFT),
    WHITE_UP_RIGHT	=> (UP_RIGHT	<< WHITE_SHIFT),
    WHITE_RIGHT_DOWN	=> (RIGHT_DOWN	<< WHITE_SHIFT),
};

use constant {
    EDGE_DOWN		=> (BLACK_DOWN  | WHITE_DOWN),
    EDGE_LEFT		=> (BLACK_LEFT  | WHITE_LEFT),
    EDGE_UP		=> (BLACK_UP    | WHITE_UP),
    EDGE_RIGHT		=> (BLACK_RIGHT | WHITE_RIGHT),

    EDGE_DOWN_LEFT	=> (BLACK_DOWN_LEFT	| WHITE_DOWN_LEFT),
    EDGE_LEFT_UP	=> (BLACK_LEFT_UP	| WHITE_LEFT_UP),
    EDGE_UP_RIGHT	=> (BLACK_UP_RIGHT	| WHITE_UP_RIGHT),
    EDGE_RIGHT_DOWN	=> (BLACK_RIGHT_DOWN	| WHITE_RIGHT_DOWN),

    BLACK_MASK		=> ALL_DIRECTIONS << BLACK_SHIFT,
    WHITE_MASK		=> ALL_DIRECTIONS << WHITE_SHIFT,
};

use constant {
    ALL_EDGES		=> (EDGE_DOWN | EDGE_LEFT | EDGE_UP | EDGE_RIGHT),
};

1;
