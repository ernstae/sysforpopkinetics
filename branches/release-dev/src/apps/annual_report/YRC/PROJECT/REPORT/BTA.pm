package YRC::PROJECT::REPORT::BTA;
use strict;

sub getBTAString {
   my($self, $type) = @_;

   if($type eq 'T') {
      return '(D) Dissemination and Training';
   } elsif ($type eq 'C') {
      return '(C) Collaborative Research';
   } elsif ($type eq 'Tech') {
      return '(T) Technological Research & Development';
   }
   return 'UNKNOWN TYPE';
}


1;
