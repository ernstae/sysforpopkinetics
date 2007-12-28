#!/usr/bin/perl

my @programs = qw ( QN01Box
		    ginac
		    ginac-devel
		    atlas
		    atlas-devel
		    lapack
		    lapack-devel
		    xerces-c
		    xerces-c-devel
		    gmp
		    boost
		    boost-devel
		    pvm
		    perl-MIME-Lite
		    perl-Daemon
		    perl
		    perl-DBD-MySQL
		    perl-XML-Simple
		    perl-HTML-Parser
		    perl-MIME-Lite
		    perl-Socket6
		    perl-MailTools
		    perl-IO-Zlib
		    perl-String-CRC32
		    perl-Net-SSLeay
		    perl-Archive-Tar
		    perl-HTML-Tagset
		    perl-IO-Socket-SSL
		    perl-URI
		    perl-XML-Parser
		    perl-Digest-SHA1
		    perl-Net-IP
		    perl-Compress-Zlib
		    perl-SGMLSpm
		    perl-IO-Socket-INET6
		    perl-DBI
		    perl-TimeDate
		    perl
		    perl-Net-DNS
		    perl-Digest-HMAC
		    perl-libwww-perl
		    bison
                    compat-gcc-34-g77
                    mysql-server
                    mysql-client
                    mysql-devel
                    mysql-libs
                    libtool
                    automake
                    autoconf
                    imake
                    mysql-connector-java
                    gcc-java
                    java-1.5.0-gcj-devel
		    );

$rpmcmd = "rpm -q ";

my @not_avail = ();


sub get_os() {
    my $os_ver = "";

    if ( open( FR, "/etc/issue" ) ) {
	while (<FR>) {
	    $contents = $_;
	    $os_ver = "unknown";  # by default, we don't know.
	    $os_ver = "Fedora" if (/^Fedora/);
	    $os_ver = "RedHat" if (/^RedHat/);
	    return($os_ver);
	}
    }
    else {
	print ">> ERROR: Could not determine operating system. RedHat and Fedora Linux are the only operating systems we currently support.\n";
	exit(1);
    }
    close(FR);
    return($os_ver);
}


############################################################
# Main program
############################################################

print "=" x 60 . "\n";
print "System for Population Kinetics (SPK)\nChecking for prerequisite programs...\n";
print "=" x 60 . "\n\n";

foreach $prog ( @programs ) {
    if ( system($rpmcmd .  $prog) != 0 ) {
	push(@not_avail,$prog);
	print " >> missing <<\n";
    }
    else {
	print " OK!\n";
    }
}

print "Would you like to install the packages via YUM? [yes/no]: ";
my $response = <STDIN>;
chop($response);

if ( $response =~ /^yes$/i ) {
    
    if ( get_os() eq 'Fedora' ) {
        print "Installing missing packages via YUM\n";
        @args = ("yum", "install", "-y");
        foreach $item ( @not_avail ) {
            push(@args, $item);
        }
        system(@args) == 0 or die "Could not run yum for update";
    }
    else {
        print get_os();
    }
    
}



