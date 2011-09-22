################################################################################
################################################################################
##
## FORM.pm
## Purpose: To simply produce HTML for common form element types
##
## Author:      Michael Riffle <mriffle@u.washington.edu>
## Date:        April 10, 2002
##
################################################################################
################################################################################

package YRC::WWW::FORM;
use strict;

# PRODUCES CHECKBOXES FOR EACH OPTION, WITH THE APPROPRIATE DEFAULTS CHECKED
# (checkboxes) ARRAY 1 : [ [NAME1, VALUE1, TEXT1], [NAME2, VALUE2, TEXT2],...]
# (default checked) HASH 1: [ {NAME1=>VALUE1}, {NAME2=>VALUE2},... ]
sub checkboxList {
   my($self, $options, $checked) = @_;
   my($output, $option);

   $output = "";

   foreach $option (@{$options}) {
      $output .= "<NOBR>";
      $output .= "<INPUT TYPE=\"checkbox\" NAME=\"" . $option->[0] . "\" VALUE=\"" . $option->[1] . "\"";

      foreach (@{$checked}) {
         if($_->{$option->[0]} && $_->{$option->[0]} eq $option->[1]) { $output .= " CHECKED"; last; }
      }
      $output .= "> " . $option->[2];
      $output .= "</NOBR>\n";
   }

   return $output;

}


# PRODUCES A SIMPLE YES OR NO RADIO BUTTON OPTION
# TAKES A HASH OF ARGUMENTS AS A VALUE
# ARGS{NAME} => Name of radio button form element
# ARGS{DEFAULT} =>  Should be 'T' or 'F', denotes default value
sub truefalseList {
   my($self, %ARGS) = @_;
   my($output);

   if(!$ARGS{NAME}) { return "Error"; }

   $output = "True ";
   $output .= "<INPUT TYPE=\"RADIO\" VALUE=\"T\" NAME=\"" . $ARGS{NAME} . "\"";
   if($ARGS{DEFAULT} && $ARGS{DEFAULT} eq 'T') { $output .= " checked"; }
   $output .= ">  ";

   $output .= "False ";
   $output .= "<INPUT TYPE=\"RADIO\" VALUE=\"F\" NAME=\"" . $ARGS{NAME} . "\"";
   if($ARGS{DEFAULT} && $ARGS{DEFAULT} eq 'F') { $output .= " checked"; }
   $output .= ">  ";

   return $output;
}


1;
