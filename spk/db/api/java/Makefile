HOST = "localhost"
include ../../../../../../doc/webdir.mk
T = dbAPIjava
WEB = $(WEBDIR)/v0.1/specs/$T

java:	
	javac @sourcefiles
	javadoc -d docs rfpk.spk.spkdb

test:
	bash reset_db.sh $(HOST)
	java TestSpkdb $(HOST)

install:
	scp -r docs/*.html docs/*.css docs/resources docs/rfpk $(WEB)


