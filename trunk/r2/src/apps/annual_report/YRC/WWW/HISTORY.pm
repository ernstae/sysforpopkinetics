################################################################################
################################################################################
##
## HISTORY.pm
## Purpose: To provide functions for accessing the recently history of CGI visits
## in the site
##
## Author:      Michael Riffle <mriffle@u.washington.edu>
## Date:        April 16, 2002
##
################################################################################
################################################################################

package YRC::WWW::HISTORY;
use strict;

use lib "/usr/local/lib/site_perl";
use YRC::DB;

sub new {
   my ($class, %args) = @_;
   my ($cookie);

   my ($self) = bless { }, ref($class)||$class;

   $self->{TITLES} = ();
   $self->{LINKS} = ();

   $self->_init(%args);
   return $self;
}

sub _init {
   my ($self, %args) = @_;

   $self->{TABLE} = "tblHistory";

   foreach (keys(%args)) {
      $self->{'_'.$_} = $args{$_};
   }

   return 1;
}

# ADD TO THE HISTORY
# TAKES A HASH OF ARGUMENTS {TITLE=>"title", LINK=>"url", TRUNC=>1||0}
sub setHistory {
   my($self, @myARGS) = @_;
   my(%ARGS);

   if(@myARGS % 2 != 0) {
      # ODD NUMBER OF ARGUMENTS?  TRUNC MUST NOT BE SET
      if($myARGS[$#myARGS] eq 'TRUNC') {
         push(@myARGS, (0));
      } else {
         # HOUSTON, WE HAVE A PROBLEM
         return 1;
      }
   }
   %ARGS = @myARGS;

   if(!$self->{_SESSIONID}) { return 0; }
   if(!$ARGS{TITLE} || !$ARGS{LINK}) { return 0; }

   # STRIP THE TRUNC OFF OF THE LINK
   $ARGS{LINK}  =~ s/\&trunc\=1//g;
   $ARGS{LINK}  =~ s/\?trunc\=1//g;

   # CUT THE LIST DOWN IF THEY CLICKED ON A PREVIOUS NAV LINK
   if($ARGS{TRUNC}) {
      return $self->truncate(%ARGS);
   }

   # IF OUR OBJECT ISN'T POPULATED, POPULATE IT
   if(!($self->{LINKS})) { $self->load(); }


   unless( ($self->{LINKS}->[-1]) eq $ARGS{LINK} ) {
      push( @{$self->{LINKS}}, ($ARGS{LINK}) );
      push( @{$self->{TITLES}}, ($ARGS{TITLE}) );
      $self->save();
   }


   return 1;
}

# TRUNCATE THE HISTORY LIST TO THE LAST ENTRY FOR A GIVEN LINK/TITLE
sub truncate {
   my($self, %ARGS) = @_;
   my($sql, $sth, @arr);

   $sql = "SELECT hisID, hisLink, hisTitle FROM tblHistory WHERE sessionID = '" . $self->{_SESSIONID} . "' ORDER BY hisID DESC";

   if(!$self->{DBH}) { $self->{DBH} = YRC::DB->getDBH(); }
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;

   if(!$sth->rows) {
      # NO HISTORY, BUT NO ERROR:  RETURN 1
      return 1;
   }

   # DUMP THE FIRST ROW
   $sth->fetchrow_array;

   while(@arr = $sth->fetchrow_array) {
      if( ($arr[1] eq $ARGS{LINK}) && ($arr[2] eq $ARGS{TITLE}) ) {
         # FOUND A MATCH, LET'S TRUNCATE
         $sql =  "DELETE FROM tblHistory WHERE sessionID = '" . $self->{_SESSIONID} . "' AND ";
         $sql .= "hisID > " . $arr[0];

         $self->{DBH}->do($sql);
         last;
      }
   }

   $self->load();
   return 1;
}


# SET THE DATABASE HANDLE FOR THIS OBJECT
sub setDBH {
   my($self, $dbh) = @_;
   $self->{DBH} = $dbh;
}


# LOAD THE HISTORY FROM THE DATABASE
# THEN POPULATE THE LOCAL GLOBAL ARRAYS
sub load {
   my($self, $SESSIONID) = @_;
   my($sql, $sth, @arr);

   # IF WE HAVE NO SESSION ID, GO AWAY
   if(!$self->{_SESSIONID} && !($self->{_SESSIONID} = $SESSIONID)) { return 0; }

   # IF WE HAVE NO DBH, GET ONE
   if(!$self->{DBH}) { $self->{DBH} = YRC::DB->getDBH(); }

   $sql = "SELECT hisID, hisLink, hisTitle FROM tblHistory WHERE sessionID = '" . $self->{_SESSIONID} . "' ORDER BY hisID";
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;

   if(!$sth->rows) {
      # NO HISTORY, BUT NO ERROR:  RETURN 1
      return 1;
   }

   # REINITIALIZE THESE VARIABLES
   $self->{TITLES} = ();
   $self->{LINKS} = ();

   while(@arr = $sth->fetchrow_array()) {
      push( @{$self->{TITLES}}, ($arr[2]) );
      push( @{$self->{LINKS}}, ($arr[1]) );
   }

   return 1;
}

# SAVES THE HISTORY TO THE DATABASE
# ACTUALLY JUST SAVES LAST ENTRY IN LINKS AND TITLES INTO DATABASE
sub save {
   my($self) = @_;
   my($sql, $sth);

   if(!$self->{DBH}) { return 0; }
   if(!$self->{_SESSIONID}) { return 0; }

   if(!(@{$self->{LINKS}})) { return 0; }
   if(!(@{$self->{TITLES}})) { return 0; }

   $sql =  "INSERT INTO tblHistory (sessionID, hisLink, hisTitle) VALUES ('" . $self->{_SESSIONID} . "', ";
   $sql .= "'" . $self->{LINKS}->[-1] . "', '" . $self->{TITLES}->[-1] . "')";

   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;

   return 1;
}


# RETURN A STRING CONTAINING OUR HISTORY NAVIGATION
sub getHistoryNav {
   my($self) = @_;
   my($i) = 0;
   my($output) = "";
   my($delim) = ' -> ';

   if( !$self->{LINKS} && !$self->load()) { return 0; }

   $output .= "<FONT STYLE=\"font-family: verdana, helvetica, arial;font-weight: 300;font-size: 10pt\">";
   $output .= "You are here: ";

   $i = @{$self->{LINKS}} - 4;
   if($i < 0) { $i = 0; }

   while($i < @{$self->{LINKS}}) {

      if($i == (@{$self->{LINKS}} - 1)) {
         $output .= "<B>" . $self->{TITLES}->[$i] . "</B>\n";
      } elsif($self->{LINKS}->[$i] =~ /\?/) {
         $output .= "<A HREF=\"" . $self->{LINKS}->[$i] . "&trunc=1\">" . $self->{TITLES}->[$i] . "</A>$delim\n";
      } else {
         $output .= "<A HREF=\"" . $self->{LINKS}->[$i] . "?trunc=1\">" . $self->{TITLES}->[$i] . "</A>$delim\n";
      }

      $i++;
   }

   $output .= "</FONT><BR><BR>\n";

   return $output;

}

1;
