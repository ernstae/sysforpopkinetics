SHELL = /bin/sh

all:			backup-home-full

backup-home-full:	backup-home-full.sh

clean:
	rm backup-home-full

install:
	cp backup-home-full ${HOME}/bin/shell

% : $.sh
	cp $> $@
