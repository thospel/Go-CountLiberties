#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 01_syntax.t'
#########################
# $Id: 01_syntax.t 5026 2012-02-02 22:33:26Z hospelt $

## no critic (UselessNoCritic MagicNumbers)
use strict;
use warnings;
BEGIN { $^W = 1 };

use Test::More tests => 1;
use FindBin qw($Bin);
use File::Temp qw(tempdir);
use Carp;
use Errno qw(ENOENT ESTALE);
use Sys::Hostname;

my $host = hostname();

my $tmp_dir = tempdir(CLEANUP => 1);

# Import a complete file and return the contents as a single string
sub slurp {
    my ($file, $may_not_exist) = @_;
    croak "filename is undefined" if !defined $file;
    open(my $fh, "<", $file) or
        $may_not_exist && ($! == ENOENT || $! == ESTALE) ?
	return undef : croak "Could not open '$file': $!";
    my $rc = read($fh, my $slurp, 1024 + -s $fh);
    croak "File '$file' is still growing" if
        $rc &&= read($fh, $slurp, 1024, length $slurp);
    croak "Error reading from '$file': $!" if !defined $rc;
    close($fh) || croak "Error while closing '$file': $!";
    return $slurp;
}

sub check {
    open(my $olderr, ">&", "STDERR") || die "Can't dup STDERR: $!";
    open(STDERR, ">", "$tmp_dir/stderr") ||
        die "Can't open $tmp_dir/stderr: $!";
    # diag("$^X -c @_");
    my $rc = system($^X, "-c", @_);
    open(STDERR, ">&", $olderr)        || die "Can't dup old STDERR: $!";
    my $errors = slurp("$tmp_dir/stderr");
    $errors =~ s/.* syntax OK\n//;
    if ($errors ne "") {
        diag($errors);
        return 1;
    }
    return $rc;
}

$Bin =~ s{/t/?\z}{} || die "No /t at end of $Bin";

for my $script (qw(goth)) {
    ok(!check(
            "-I", "$Bin/blib/lib", "-I", "$Bin/blib/arch",
            "-T",
            "bin/$script",
            #"--blib",
       ),
       "Can compile bin/$script");
}
