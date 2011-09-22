SHELL = /bin/sh

all:			cronscp cronssh get-agent-data

cronscp:		cronscp.sh

cronssh:		cronssh.sh

get-agent-data:		get-agent-data.sh

clean:
	rm cronscp cronssh get-agent-data

install:
	cp cronscp cronssh get-agent-data ${HOME}/bin/shell

% : $.sh
	cp $> $@
