package TestDrive;
# $Id: TestDrive.pm 5611 2013-11-08 21:27:00Z hospelt $
## no critic (UselessNoCritic MagicNumbers)
use strict;
use warnings;

our $VERSION = "1.000";

use Carp;

use Exporter::Tidy
    other => [qw(
	spew compare diff)];

sub spew {
    my $file = shift;
    croak "filename is undefined" if !defined $file;
    defined $_ || croak "Undefined argument" for @_;
    open(my $fh, ">", $file) || croak "Could not create '$file': $!";
    eval {
        print($fh @_) || croak "Error writing to '$file': $!";
        close($fh)    || croak "Error closing '$file': $!";
    };
    if ($@) {
        undef $fh;
        unlink($file) || croak "Could not unlink '$file' after $@";
        croak $@;
    }
}

sub compare {
    my $got    = shift;
    my $expect = shift;
    for ($got, $expect) {
        defined && s/[^\S\n]+\n/\n/g;
    }
    splice(@_, 0, 2, $got, $expect);
    goto &Test::More::is;
}

sub diff {
    my $tmp_dir = ".";
    $tmp_dir || croak "No tmp_dir prepared";
    my $got    = shift;
    my $expect = shift;
    for ($got, $expect) {
        defined && s/[^\S\n]+\n/\n/g;
    }
    goto &Test::More::pass if
        defined $expect && defined $got && $expect eq $got ||
        !defined $expect && !defined $got;
    my $expect_file = "$tmp_dir/cache/expect";
    spew($expect_file, $expect);
    my $got_file    = "$tmp_dir/cache/got";
    spew($got_file, $got);
    open(my $fh, "-|", "diff", "-u", $expect_file, $got_file) ||
        die "Could not start diff: $!";
    local $_;
    while (<$fh>) {
        chomp;
        # s{(\s)}{sprintf('\x%02x', ord $1)}eg;
        Test::More::diag($_);
    }
    goto &Test::More::fail;
}

1;
