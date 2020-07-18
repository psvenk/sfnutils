#!/usr/bin/env perl
use strict;
use warnings;
use 5.032;

my $path = shift || '.';

opendir(my $dh, $path) or die "$0: $!\n";
my $fn_map = {};
my @files = ();
while (readdir $dh) {
	push @files, filename_make($_, $fn_map) unless /^..?$/;
}
closedir $dh;
printf("%-8s %-3s\n", $_->[0], $_->[1]) for sort filename_cmp @files;

sub filename_make {
	my ($name, $fn_map) = @_;

	# Index of separator between filename and extension
	my $sep_i = -1;
	# Whether or not the filename has been modified
	my $modified = 0;

	for my $i (reverse(0 .. length($name) - 1)) {
		$_ = substr($name, $i, 1);
		if (ord >> 7 || /\+/) {
			# We don't care about Unicode here
			substr($name, $i, 1) = '_';
			$modified = 1;
		} elsif (/\./ && $i != 0 && $sep_i == -1) {
			$sep_i = $i;
		} elsif (/ / || /\./) {
			substr($name, $i) = substr($name, $i + 1);
			$sep_i-- if $sep_i != -1;
			$modified = 1;
		} else {
			substr($name, $i, 1) = uc;
		}
	}
	my $ext = '';
	if ($sep_i != -1) {
		$ext = substr($name, $sep_i + 1);
		$name = substr($name, 0, $sep_i);
	}

	$modified = 1 if length($name) > 8 || length($ext) > 3;
	if ($modified) {
		$name = substr($name, 0, 6);
		# Number to append to filename (after '~')
		my $num = ++$fn_map->{$name};
		if ($num >= 10) {
			$name = substr($name, 0, 5);
		}
		$name .= "~$num";
	}
	$ext = substr($ext, 0, 3);
	return [$name, $ext];
}

sub filename_cmp {
	my $result = 0;
	if (($result = $a->[0] cmp $b->[0])) {
		return $result;
	}
	return $a->[1] cmp $b->[1];
}
