SHELL = /bin/sh

all:			cronscp cronssh get-agent-data

cronscp:		cronscp.sh

cronssh:		cronssh.sh

get-agent-data:		get-agent-data.sh

clean:
	rm cronscp cronssh get-agent-data

% : $.sh
	cp $> $@
