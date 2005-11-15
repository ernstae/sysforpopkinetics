HOST = "localhost"
include ../../../../doc/webdir.mk
T = jobqserver
WEB = $(WEBDIR)/v0.1/specs/$T

java:	
	/usr/java/j2sdk1.4.2_03/bin/javac uw/rfpk/jobqs/JobQueue.java
	/usr/java/j2sdk1.4.2_03/bin/javac -classpath ../db/api/java/:. TestJobqs.java
	/usr/java/j2sdk1.4.2_03/bin/javadoc -d docs uw.rfpk.jobqs

test:
	load_spktest.pl --schema ../db/api/schema.sql --basedata ../db/api/basedata.sql --userdata ../db/api/userdata.sql
	java -cp /usr/local/lib/mysql-connector-java-3.0.10-stable-bin.jar:../db/api/java/:. TestJobqs $(HOST) "spktest" "tester" "tester" "/usr/local/lib/mysql-connector-java-3.0.10-stable-bin.jar:."

installdoc:
	scp -r docs/*.html docs/*.css docs/resources docs/uw $(WEB)


