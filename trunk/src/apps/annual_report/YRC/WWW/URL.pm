package YRC::WWW::URL;

sub link {
	my $self=shift;
	my %param = ( @_ );
	my $link = $ENV{REQUEST_URI};
	if ($param{keep}) {
		my @tmparr;
		for (keys %{ $param{keep} }) {
			$link =~ /$_\=([\w\.\+\/\-\%\?\,]+)/;
			push @tmparr, "$_=$1";
		}
		my $tmplink = $ENV{'SCRIPT_NAME'}.'?'.join( '&', @tmparr );
		$link = $tmplink;
	}
			
	if ($param{script}) {
		if ($param{script} =~ /\//) {
			$param{script} = (split(/\//, $param{script}))[-1];
		}
		$link =~ s/((2DDB|lars):?\/)[\w\.]+/$1$param{script}/;
	}
	for (keys %{ $param{flip} }) {
		if ($link =~ s/([&?]$_\=)[\w\.\+\/\-\%\?\,]*/$1$param{flip}->{$_}/) {
			$link =~ s/\&$_\=[\w\.\+\/\-\%\?\,\|]+//;
		} else {
			$link .= "&$_=$param{flip}->{$_}";
		}
	}
	for (keys %{ $param{change} }) {
		if ($link =~ s/([&?]$_\=)[\w\.\+\/\-\%\?\,\:]*/$1$param{change}->{$_}/) {
		} else {
			$link .= "&$_=$param{change}->{$_}";
		}
	}
	for(keys %{ $param{remove} } ) {
		$link =~ s/\&$_\=[\w\.\+\/\-\%\?\,\|]+//;
		$link =~ s/\?$_\=[\w\.\+\/\-\%\?\,\|]+&/?/;
	}
	return $link;
}
1;
