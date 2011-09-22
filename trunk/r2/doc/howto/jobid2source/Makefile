include ../../webdir.mk
T = jobid2source
HTML_OPS = --nochunks
STYLE = ../../stylesheet
WEB = $(WEBDIR)/howto/rhel3/$T

all:	$T.htm

$T.htm : $T.omh
	omhelp ./$T.omh

install:
	scp *.htm $(WEB)

clean:
	rm -f *.htm 
