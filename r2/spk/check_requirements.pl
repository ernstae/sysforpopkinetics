#!/usr/bin/perl

#
my @programs_ubuntu = qw (
   gcc-4.2
   gfortran
   libginac1.4
   ginac-tools
   libginac1.4-dbg
   libblas-dev
   libatlas3gf
   liblapack3gf
   libatlas3gf-sse2
   libxerces-c2-dev
   libxerces-c28
   libgmp3c2
   libgmpxx4ldbl
   libgmp3-dev
   libboost-dev
   blacs-pvm-dev
   libpvm3
   netpipe-pvm
   pvm
   pvm-dev
   pvm-examples
   scalapack-pvm-dev
   scalapack-pvm-test
   xpvm
   perl
   libhtml-parser-perl
   libipc-shareable-perl
   libcpan-perl
   libmime-lite-perl
   libxml-simple-perl
   libmime-lite-perl
   libproc-daemon-perl
   libdigest-hmac-perl
   libdigest-sha1-perl
   libxml-simple-perl
   libcompress-zlib-perl
   libstring-crc32-perl
);

my @programs_fedora = qw ( gcc-c++
                    ginac
		    ginac-devel
		    atlas
		    atlas-devel
		    lapack
		    lapack-devel
		    xerces-c
		    xerces-c-devel
		    gmp
                    gsl
                    gsl-devel
		    cppad-devel
                    cppunit
                    cppunit-devel
		    boost
		    boost-devel
		    pvm
		    perl-IPC-Shareable
                    perl-CPAN
		    perl-MIME-Lite
		    perl
                    perl-Proc-Daemon
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
                    docbook-utils
                    doxygen
                    compat-gcc-34-g77
                    mysql-server
                    mysql
                    mysql-devel
                    mysql-libs
                    libtool
                    automake
                    autoconf
                    imake
                    mysql-connector-java
                    gcc-java
                    java-1.6.0-openjdk
                    java-1.6.0-openjdk-devel
                    java-1.6.0-openjdk-javadoc
                    java-1.6.0-openjdk-plugin
                    ant
                    flex
		    );


$rpmcmd = "rpm -q ";
@programs = @programs_fedora;

if ( get_os() eq "Ubuntu" ) {
    $rpmcmd = 'dpkg -s ';
    @programs = @programs_ubuntu;
}

my @not_avail = ();


sub get_os() {
    my $os_ver = "";

    if ( open( FR, "/etc/issue" ) ) {
	while (<FR>) {
	    $contents = $_;
	    $os_ver = "unknown";  # by default, we don't know.
	    $os_ver = "Fedora" if (/^Fedora/);
	    $os_ver = "RedHat" if (/^RedHat/);
	    $os_ver = "Ubuntu" if (/^Ubuntu/);
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

if ( ($#not_avail) > 0 ) {
    print "Would you like to install the missing packages via YUM? yes/no [yes]: ";
    my $response = <STDIN>;
    chop($response);

    if ( $response =~ /^yes$/i || length($response) == 0 ) {
	
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
}
else {
    print "All packages were found and up-to-date\n";
    exit(0);

}
    


