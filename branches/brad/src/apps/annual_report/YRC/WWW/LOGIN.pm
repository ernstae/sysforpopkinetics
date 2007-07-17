################################################################################
################################################################################
##
## LOGIN.pm
## Purpose: To provide functions for handling the login in process to the YRC
## web site.
##
## Author:      Michael Riffle <mriffle@u.washington.edu>
## Date:        April 1, 2002
##
################################################################################
################################################################################

package YRC::WWW::LOGIN;
use strict;

use lib "/usr/local/lib/site_perl";
use YRC::WWW::COOKIE;

# Set a cookie and show a login form.
sub showLogin {
   my($self, %args) = @_;

   if(!$args{QUERY}) { return 0; }

   $self->showLoginHeader($args{QUERY});

   if($args{ERROR}) {
      $self->showError($args{ERROR});
   }

   $self->showLoginForm(REQUESTER=>$args{REQUESTER}, USERNAME=>$args{USERNAME});

   $self->showLoginFooter();

}


sub showLoginHeader {
   my($module, $query) = @_;

   if(!$query) { die("Lost query object in $module showLoginHeader().\n"); }

   # PRINT OUR HTTP HEADER, SET THE COOKIE AND GET THE COOKIE VALUE
   my($cvalue) = YRC::WWW::COOKIE->setCookieHeader($query);

   # PRINT OUR HTML LOGIN HEADER
   open (FILE, "</var/www/html/v2/tmpl/loginHeader.html") || die("Unable to open loginHeader.html.\n");
   foreach (<FILE>) {
      print;
   }
   close (FILE);
}

sub showLoginForm {
   my($self, %args) = @_;

   # PRINT OUR HTML LOGIN FORM
   open (FILE, "</var/www/html/v2/tmpl/loginForm.html") || die("Unable to open loginForm.html.\n");
   foreach (<FILE>) {

      if($args{REQUESTER}) { $_ =~ s/#REQUESTER#/$args{REQUESTER}/; }

      if($args{USERNAME}) { $_ =~ s/#USERNAME#/$args{USERNAME}/; }
      else { $_ =~ s/#USERNAME#//; }

      print;
   }
   close (FILE);
}

sub showLoginFooter {
   # PRINT OUR HTML LOGIN FOOTER
   open (FILE, "</var/www/html/v2/tmpl/loginFooter.html") || die("Unable to open loginFooter.html.\n");
   foreach (<FILE>) {
      print;
   }
   close (FILE);
}

sub showError {
   my($self, $error) = @_;

   # PRINT AN ERROR MESSAGE
   open (FILE, "</var/www/html/v2/tmpl/loginError.html") || die("Unable to open loginError.html.\n");
   foreach (<FILE>) {
  
      $_ =~ s/#ERROR#/$error/;

      print;
   }
   close (FILE);
}

1;
