BEGIN {
    $^W = 1;
    $| = 1;
    $SIG{INT} = sub { die };
    print "1..2\n";
}

use strict;
use IPC::Shareable;
my $t  = 1;
my $ok = 1;

# --- TIESCALAR
my $sv;
tie($sv, 'IPC::Shareable', { destroy => 'yes' })
    or undef $ok;
print $ok ? "ok $t\n" : "not ok $t\n";

# --- scalar STORE and FETCH
++$t;
$ok = 1;
$sv = 'foo';
($sv eq 'foo') or undef $ok;
print $ok ? "ok $t\n" : "not ok $t\n";

# --- Done!
exit;
