include ../../webdir.mk
T = add-regression
WEB = $(WEBDIR)/howto/rhel3/$T

all:	htm

htm:	$T.htm

$T.htm : $T.omh
	omhelp  ./$T.omh

install:
	scp *.htm $(WEB)
clean:
	rm -f *.htm

