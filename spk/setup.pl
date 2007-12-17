#!/usr/bin/perl

use POSIX;

my $release_info = "
                 System for Population Kinetics (SPK)
                 (c)2007 University of Washington
                 Resource for Population Kinetics";

my %questions = (
	"Install Path of SPK?            " => "/usr/local/spk",
        "Debug mode? (default is release)" => "no",
	"Administrator's E-mail Address? " => "root@localhost",
	"MySQL hostname                  " => "localhost",
	"MySQL username		         " => "root",
	"MySQL password                  " => "secure!",
	"HTTP Port to run Tomcat?        " => "8080",
	"HTTPS Port to run Tomcat?       " => "8081",
	"Job Queue Server hostname?      " => "jobqserver",
	"Host of compiler components     " => "aspk",
	"Host of runtime components      " => "cspk");

my @variables = ( 
	'SPK_PREFIX',
	'SPK_RELEASE',
	'SPK_ADMIN_EMAIL',
	'SPK_MYSQL_HOST',
	'SPK_MYSQL_USER',
	'SPK_MYSQL_PASSWD',
	'SPK_TOMCAT_HTTP',
	'SPK_TOMCAT_HTTPS',
	'SPK_JOBQSERVER_HOST',
	'SPK_COMPILER_HOST',
	'SPK_RUNTIME_HOST' );
			

my @config_files = ( 	'cspk/daemon/spkrund.pl',
	      		'aspk/daemon/spkcmpd.pl' );

my $license_file = "LICENSE";

# display_config() shows the menu of configuration options that can be changed
# by the end user upon installing the SPK system.
sub display_config () {
  my $counter = 1;
  my $retval = "";

  system("clear");

  print "=" x 60;
  print $release_info . "\n";
  print "=" x 60 . "\n\n";

  while (( $question, $answer ) = each ( %questions )) {
	$retval .=  "\n" . $counter . ". " . $question . " [". $answer . "]";
	$counter++;
  }

  $retval .= "\n\nPlease enter the number you would like to change.\nTo quit the install process, press 'Q'.\nTo install SPK, press 'C'\n\nYour choice? ";

return ( $retval );
}


# display the item the user requested to change, and then accept their change.
# upon acceptance, update the hash.
sub solicit_response() {
  my $item = <STDIN>;
  chop($item);

  my @q = keys %questions;
  my @a = values %questions; 


  if ( $item =~ /^[Cc]$/ ) {
	return ( 1 );
  }

  if ( $item ne '' && $item > 0 ) {
	
     --$item;

     $q = keys ( %questions );
     $a = values ( %questions );
    
     print "\n" . $q[$item] . " [" . $a[$item] . "]: ";

     $response = <STDIN>;
     chop ( $response );
     if ( $response ne '' ) {
        $questions{$q[$item]} = $response;
     }
  return ( 0 );
  }

 if ( $item == 0 || $item eq "0" ) {
    return ( 2 );
 }
}


# show_license() asks the user to agree with the license before continuing.
sub show_license() {
my $text = "";

   open ( INFILE, $license_file ) || die "Could not open license document: $!";
   while ( <INFILE> ) {
     $text .= $_;
   }
   close ( INFILE );

   system ("clear");
   print $text;
   print "\n\nDo you agree with the license (please answer 'yes' or 'no')?  ";
   my $answer = <STDIN>;
   chop($answer);

   if ( lc($answer) eq 'yes' ) { return ( 0 ); }
   else { return (1); }
}



# This subroutine opens all the files defined in the @config_files array and looks
# for the variable placeholder.  This will only work with fresh installations.  Newer
# versions of SPK should have all the configuration options saved to a configuration
# file, that is read in at run-time.  Unfortunately, that could not be engineered prior
# to the v1.0 release of SPK.
sub change_config() {
 my $text = "";

   print "\n\nWriting configuration changes out to the system....";

=pod
   foreach $file ( @config_files ) {
     $text = "";
     open ( INFILE, $file ) || die "Could not open $file for reading: $!";
     while ( <INFILE> ) {
       $text .= $_;
     }
     close ( INFILE );
    
     # perform the replacements
     for ( $i = 0; $i < $#variables; $i++ ) {
       $text =~ s/\[\[$variables[$i]\]\]/$questions[$i]/mg;
     }

     # write out to the file.
     open ( OUTFILE, ">$file" ) || die "Could not open $file for writing: $!";
     print OUTFILE $text;
     close ( OUTFILE );
   }
=cut
	
   print " > COMPLETE <\n\n";
}

# check_database() checks to see if the spkdb database has been created, and
# if not, it creates it.
sub check_database() {
  

}


sub check_perl_modules() {

=pod

  my %perl_modules = ( 	"MIME::Lite" => 0,
			"Daemon" => 0,
			"DBD::MySQL" => 0,
			"XML::Simple" => 0,
			"HTML::Parser" => 0,
			"Socket6" => 0,
			"MailTools" => 0,
			"IO::Zlib" => 0,
			"String::CRC32" => 0,
			"Net::SSLeay" => 0,
			"Archive::Tar" => 0,
			"HTML::Tagset" => 0,
			"XML::Parser" => 0,
			"URI" => 0,
			"Digest::SHA1" => 0,
			"Net::IP" => 0,
			"Compress::Zlib" => 0,
			"SGMLSpm" => 0,
			"IO::Socket::INET6" => 0,
			"DBI" => 0,
			"TimeDate" => 0,
			"Net::DNS" => 0,
			"Digest::HMAC" => 0 );
=cut

  my %perl_modules = ( 	"MIME::Lite" => 0,
			"Proc::Daemon" => 0,
			"XML::Simple" => 0,
			"HTML::Parser" => 0,
			"Digest::SHA1" => 0,
			"Digest::HMAC" => 0,
			"DBI" => 0,
			"String::CRC32" => 0,
			"XML::Parser" => 0,
			"Compress::Zlib" => 0,
			"HTML::Tagset" => 0 );

  foreach $pkg ( %perl_modules ) {
        
	$space = 16;
        $f = "%" . $space . "s : %s\n";

	eval "use $pkg;"; $perl_modules{$pkg} = $@ ? 0 : 1;
  }
			

  print "Perl Package Report:  \n\n";
  
  while ( ( $package, $status ) = each ( %perl_modules )) {
	$report .= "\n$package needs to be installed!" if ( $status == 0 );
	printf $f, $package, ($status == 1) ? "> OK <" : ">>>>> NOT PRESENT:  must be installed!";
#	print "\n" . $package . " is " . ($status == 1) ? "present" : "missing";	
  }

  print $report;


  # install missing packages:
  print "\n\n>> WOULD YOU LIKE TO INSTALL THE REQUIRED PERL MODULES NOW [yes]?  ";
  my $answer = <STDIN>;
  chop($answer);

  if ( $answer =~ /yes/i ) {
  use CPAN;
  
  while ( ( $package, $status ) = each ( %perl_modules ) ) {
    if ( $status == 0 ) {
      CPAN::install $package;
    }
  }
  }
  else {
    print "\n\nThis installer will now quit.  You must install the prerequisites and then re-run the setup.pl script.\n\nEND.";
    exit(1);
  }

}

##############
# Main block
##############

# let's require the user to be root before continuing (since we have
# the potential to install Perl Modules, and may have to run yum/up2date
# to install missing programs
die "You must be the root user to run this installer!\n" if ( getuid() != 0 );

# make sure the user agrees to the license.
$ret = 1;
while ( $ret != 0 ) {
   $ret =  show_license();
}

# show the menu until the user wishes to continue.
$stop = 0;
while ( $stop != 1 ) {
   print display_config();
   $stop = solicit_response();
#   $stop = 1 if ( solicit_response() > 0 );
}

# perform the necessary configuration file changes
change_config();

# check to see that all the perl modules are installed.
check_perl_modules();

# call the script to build all the autoconf files.

# see if the database exists, and if not, create it.
#check_database();

# run configure and make as necessary.







############################################################
#
#sub autogen() {
    
#    my @autoconf_dirs = ( 'cspk/spk', 'cspk/spkpred', 'aspk/spkcompiler' );
    
 #   foreach $dir ( @autoconf_dirs ) {
	


