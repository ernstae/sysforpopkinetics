HOST = "localhost"
include ../../../../../../doc/webdir.mk
T = dbAPIjava
WEB = $(WEBDIR)/v0.1/specs/$T

java:	
	javac @sourcefiles
	javadoc -d docs rfpk.spk.spkdb

test:
	load_spktest.pl 
	java TestSpkdb $(HOST)

installdocs:
	scp -r docs/*.html docs/*.css docs/resources docs/rfpk $(WEB)


