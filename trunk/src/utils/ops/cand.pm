package cand;

use 5.008;
use strict;
use warnings;
#use Sys::Hostname;

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = ("stage_directory", "make_directory");
my $mkdir_command = "/bin/mkdir";
my $scp_command = "/usr/bin/scp";


sub stage_directory {
    my $host = shift;
    my $path = shift;
    my $source = "$host:$path/spktest";
    my $dest = "$host$path";
    my @args = ($scp_command, "-r", $source, $dest);
    print "@args", "\n";
    system(@args);
    my $exit_status = $? >> 8;
    if ($exit_status != 0) {
	die "'scp -r $source $dest' failed\n";
    }
   rename "$dest/spktest", "$dest/spkprod";
}

sub make_directory {
    my $path = shift;
    my @args = ($mkdir_command, "-p", $path);
     system(@args);
    my $exit_status = $? >> 8;
    if ($exit_status != 0) {
	die "Could not make directory '$path'\n";
    }
}
