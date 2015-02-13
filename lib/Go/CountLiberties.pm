package Go::CountLiberties;

use 5.008001;
use strict;
use warnings;

our $VERSION = '1.000';	# $Revision: 5609 $

require XSLoader;
XSLoader::load('Go::CountLiberties', $VERSION);

package Go::CountLiberties::Shape;
use Carp;

use Exporter::Tidy
    other => [qw(
	symmetry_matrix cache_file neighbour_cache_file comment
	set_name smash_shapes
        @orthogonals @diagonals @offsets %offsets
	%orthogonals %diagonals %direction_names
        EMPTY BLACK WHITE EDGE COLOR_MAX COLORS)];

use Go::CountLiberties::Constants qw(
	EMPTY BLACK WHITE EDGE COLOR_MAX COLORS
        UP_SHIFT DOWN_SHIFT LEFT_SHIFT RIGHT_SHIFT
	DOWN_LEFT_SHIFT UP_RIGHT_SHIFT RIGHT_DOWN_SHIFT LEFT_UP_SHIFT
	BACKGROUND_CHAR EMPTY_CHAR BLACK_CHAR WHITE_CHAR EDGE_CHAR);
use constant {
    INFINITY => 9**9**9,
};

our %orthogonals = (
    UP_SHIFT()		=> [ 0,  1],
    DOWN_SHIFT()	=> [ 0, -1],
    LEFT_SHIFT()	=> [-1,  0],
    RIGHT_SHIFT()	=> [ 1,  0]);
our @orthogonals = @orthogonals{sort {$a <=> $b} keys %orthogonals};
our %diagonals = (
    DOWN_LEFT_SHIFT()	=> [-1, -1],
    LEFT_UP_SHIFT()	=> [-1,  1],
    UP_RIGHT_SHIFT()	=> [ 1,  1],
    RIGHT_DOWN_SHIFT()	=> [ 1, -1]);
our @diagonals   = @diagonals{sort {$a <=> $b} keys %diagonals};
our @offsets     = (@orthogonals, @diagonals);
our %offsets	 = (%orthogonals, %diagonals);

our %direction_names;

$direction_names{UP_SHIFT()}         = "up";
$direction_names{DOWN_SHIFT()}       = "down";
$direction_names{LEFT_SHIFT()}       = "left";
$direction_names{RIGHT_SHIFT()}      = "right";

$direction_names{UP_RIGHT_SHIFT()}   = "up_right";
$direction_names{DOWN_LEFT_SHIFT()}  = "down_left";
$direction_names{LEFT_UP_SHIFT()}    = "left_up";
$direction_names{RIGHT_DOWN_SHIFT()} = "right_down";

sub symmetries {
    my ($shape) = @_;
    my $work;
    my @shapes = ($shape->clone);
    for my $i (1..3) {
        $work = $shapes[-1]->clone;
        $work->rotate_left;
        push @shapes, $work;
    }
    $work = $shape->clone;
    $work->mirror_x;
    push @shapes, $work;
    for my $i (1..3) {
        $work = $shapes[-1]->clone;
        $work->rotate_left;
        push @shapes, $work;
    }
    return @shapes;
}

sub symmetry_ids {
    my ($shape) = @_;
    my $work = $shape->clone;
    my @ids;
    for my $i (1..4) {
        push @ids, $work->id;
        $work->rotate_left;
    }
    $work->mirror_x;
    push @ids, $work->id;
    for my $i (1..3) {
        $work->rotate_left;
        push @ids, $work->id;
    }
    return @ids;
}

{
    my @symmetry_matrix;
    my $probe = Go::CountLiberties::Shape->new;
    $probe->set(1, 0, 0);
    $probe->set(0, 1, 1);
    for my $p ($probe->symmetries) {
        my $symmetry = [];
        my ($key1, $key2) = $p->keys;
        $symmetry->[pop @$key1] = $key1;
        $symmetry->[pop @$key2] = $key2;
        push @symmetry_matrix, $symmetry;
    }

    sub symmetry_matrix {
        return @{$symmetry_matrix[shift]};
    }
}

sub set_box {
    my ($shape, $x_min, $y_min, $x_max, $y_max, $color) = @_;

    $color = EDGE if !defined $color;

    $x_min = int $x_min;
    $y_min = int $y_min;
    $x_max = int $x_max;
    $y_max = int $y_max;

    $x_min <= $x_max || croak "x_min $x_min > x_max $x_max";
    $y_min <= $y_max || croak "y_min $y_min > y_max $y_max";

    if ($x_min == $x_max) {
        for my $y ($y_min..$y_max) {
            $shape->set($x_min, $y, $color);
        }
    } elsif ($y_min == $y_max) {
        for my $x ($x_min..$x_max) {
            $shape->set($x, $y_min, $color);
        }
    } else {
        for my $x ($x_min..$x_max) {
            $shape->set($x, $y_min, $color);
            $shape->set($x, $y_max, $color);
        }
        for my $y ($y_min+1..$y_max-1) {
            $shape->set($x_min, $y, $color);
            $shape->set($x_max, $y, $color);
        }
    }
    return $shape;
}

# Return a raw representation of a shape that you can give to from_id
# but that is NOT translated to make it a hash key for the shape
# Basicaly this is the shape + the translation state
sub _id {
    return ${shift()};
}

sub from_string {
    my ($class, $string) = @_;
    my $shape = $class->new;

    my $y = 1;
    my $x = 0;
    while ($string =~ /(.)/sg) {
        if ($1 eq BACKGROUND_CHAR) {
        } elsif ($1 eq BLACK_CHAR) {
            $shape->set($x, $y, BLACK);
        } elsif ($1 eq EMPTY_CHAR) {
            $shape->set($x, $y, EMPTY);
        } elsif ($1 eq WHITE_CHAR) {
            $shape->set($x, $y, WHITE);
        } elsif ($1 eq EDGE_CHAR) {
            $shape->set($x, $y, EDGE);
        } elsif ($1 eq "\n") {
            --$y;
            $x = -1;
        } else {
            croak "Unsupported shape character '$1'";
        }
        ++$x;
    }
    return $shape;
}

sub backbone {
    my ($shape, $color) = @_;

    $color = BLACK if !defined $color;
    my $backbone = Go::CountLiberties::Shape->new;
    for my $pos (@orthogonals) {
        my $c = $shape->get(@$pos);
        $backbone->set(@$pos, $color) if $color == (defined $c ? $c : EMPTY);
    }
    for my $pos (@diagonals) {
        my $c = $shape->get(@$pos);
        $backbone->set(@$pos, $color) if
            $color == (defined $c ? $c : EMPTY) &&
            (defined$backbone->get(0, $pos->[1]) ||
             defined$backbone->get($pos->[0], 0));
    }
    return $backbone;
}

sub backbone_id {
    my $shape = shift;
    my $backbone = $shape->backbone(@_);
    return (sort $backbone->symmetry_ids)[0];
}

sub component {
    my ($shape, @active) = @_;

    my %seen = ("@{$active[0]}" => 1);
    while (my $active = shift @active) {
        for my $ortho ($shape->neighbours(@$active)) {
            push @active, $ortho if !$seen{"@$ortho"}++;
        }
    }
    return \%seen;
}

sub components {
    my ($shape, $actives) = @_;
    my @components;
  ACTIVE:
    for my $active (@$actives) {
        for my $component (@components) {
            next ACTIVE if exists $component->{"@$active"};
        }
        push @components, component($shape, $active);
    }
    for my $i (0..$#components) {
        $_ = $i for values %{$components[$i]}
    }
    return \@components;
}

sub components_hash {
    my ($shape, $actives) = @_;
    my %components;
    my $i = 0;
    for my $active (@$actives) {
        next if exists $components{"@$active"};
        $components{$_} = $i for keys %{component($shape, $active)};
        ++$i;
    }
    return \%components;
}

sub comment {
    return indent("// ", shift);
}

sub set_name {
    my ($set) = @_;

    my @nr = map scalar @$_ ,@{$set->{orthogonal_type}};
    my $name = "EMPTY$nr[EMPTY]_JOIN$nr[BLACK]_ATTACK$nr[WHITE]_EDGE$nr[EDGE]";
    $name .= "_SHARE$set->{sub_joins}" if $set->{sub_joins};
    return $name;
}

sub smash_shapes {
    my $indent = shift;
    $indent = [$indent] if ref $indent ne "ARRAY";
    my @lines;
    for my $shape (@_) {
        my $i = 0;
        for my $line ($shape->as_string =~ /^(.*)$/mg) {
            if ($i < @lines) {
                $lines[$i] .= $indent->[$i % @$indent] . $line;
            } else {
                $lines[$i] = $line;
            }
            $i++;
        }
    }
    my $result = "";
    for my $line (@lines) {
        $line =~ s/\s*\z//;
        $result .= "// $line\n";
    }
    return $result;
}

my $cache_dir = "cache";
my $file_format = "%s/%schains_%s%d";
sub cache_file {
    my ($size, $max_m, $max_n, $split) = @_;

    my $prefix;
    if ($max_m < $size || $max_n < $size) {
        if ($max_m > $max_n) {
            $prefix = sprintf("%dx%d_", $max_n, $max_m);
        } else {
            $prefix = sprintf("%dx%d_", $max_m, $max_n);
        }
    } else {
        $prefix = "";
    }
    return sprintf($file_format,
                   $cache_dir, $split ? "split_" : "", $prefix, $size);
}

sub neighbour_cache_file {
    my ($dir) = @_;
    return "$cache_dir/$dir";
}

1;
