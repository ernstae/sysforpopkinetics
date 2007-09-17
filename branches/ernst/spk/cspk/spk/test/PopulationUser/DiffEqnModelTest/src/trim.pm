use strict;
use warnings;
#
# trim( text ) trims off both heading and tailing white spaces.
#
sub trim
{
  $_[0] =~ s/^\s*([\w.\s]*[\w]+)\s+/$1/;
}
1;
