################################################################################
################################################################################
##
## ERROR.pm
## Purpose: To simply display error messages
##
## Author:      Michael Riffle <mriffle@u.washington.edu>
## Date:        April 10, 2002
##
################################################################################
################################################################################

package YRC::WWW::ERROR;
use strict;

use lib "/usr/local/lib/site_perl";

sub showError {
   my($self, $error) = @_;

   # PRINT AN ERROR MESSAGE
   open (FILE, "</var/www/html/v2/tmpl/error.html") || die("Unable to open error.html.\n");
   foreach (<FILE>) {
  
      $_ =~ s/#ERROR#/$error/;

      print;
   }
   close (FILE);
}

sub showSuccess {
   my($self, $msg) = @_;

   # PRINT A SUCCESS MESSAGE
   open (FILE, "</var/www/html/v2/tmpl/success.html") || die("Unable to open success.html.\n");
   foreach (<FILE>) {
  
      $_ =~ s/#MESSAGE#/$msg/;

      print;
   }
   close (FILE);
}

sub showWarning {
   my($self, $msg) = @_;

   # PRINT A WARNING MESSAGE
   open (FILE, "</var/www/html/v2/tmpl/warning.html") || die("Unable to open warning.html.\n");
   foreach (<FILE>) {
  
      $_ =~ s/#MESSAGE#/$msg/;

      print;
   }
   close (FILE);
}

sub showMsg {
   my($self, %PARMS) = @_;

   if($PARMS{ERROR}) { $self->showError($PARMS{ERROR}); }
   if($PARMS{WARNING}) { $self->showWarning($PARMS{WARNING}); }
   if($PARMS{SUCCESS}) { $self->showSuccess($PARMS{SUCCESS}); }

}

1;
