HOST = "localhost.localdomain"

java:	
	javac @sourcefiles
	javadoc -d docs rfpk.spk.spkdb

test:
	bash reset_db.sh
	java TestSpkdb $(HOST)

install:
	scp -r docs/*.html docs/*.css docs/resources docs/rfpk whitechuck:/var/www/html/soft/v0.1/specs/dbAPIjava


