#!/bin/bash

# clean up
rm -rf release/java-components

# mda
mkdir -p release/java-components/mda
rsync -rvl --exclude .svn mda/build.xml release/java-components/mda/

mkdir -p release/java-components/mda/lib
rsync -rvl --exclude .svn mda/lib/*.jar release/java-components/mda/lib

mkdir -p release/java-components/mda/src/uw/rfpk/mda
rsync -rvl --exclude .svn mda/uw/rfpk/mda/*.java release/java-components/mda/src/uw/rfpk/mda/
rsync -rvl --exclude .svn mda/uw/rfpk/mda/*.png release/java-components/mda/src/uw/rfpk/mda/

mkdir -p release/java-components/mda/src/uw/rfpk/mda/nonmem
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/*.java release/java-components/mda/src/uw/rfpk/mda/nonmem/

mkdir -p release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/wizard/*.java release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
rsync -rvl --exclude .svn -R mda/uw/rfpk/mda/nonmem/wizard/icons release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard

mkdir -p release/java-components/mda/src/uw/rfpk/mda/nonmem/display
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/display/*.java release/java-components/mda/src/uw/rfpk/mda/nonmem/display

mkdir -p release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/compartment/*.java release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/compartment/*.png release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/compartment/*.jpg release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment

mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/*.java release/java-components/mda/src/org/netbeans/ui/wizard

mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard/plaf
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/plaf/*.java release/java-components/mda/src/org/netbeans/ui/wizard/plaf

mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/plaf/basic/*.java release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic

mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/plaf/basic/icons/*.gif release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/plaf/basic/resources/*.properties release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources


# webapps
mkdir -p release/java-components/webapps
rsync -rvl --exclude .svn webapps/build.xml release/java-components/webapps
rsync -rvl --exclude .svn webapps/README release/java-components/webapps
mkdir -p release/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.jsp release/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.shtml release/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.css release/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.java release/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.class release/java-components/webapps/user
rsync -rvl --exclude .svn mda/lib/*.jar release/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/JavaHelp.jar release/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/SPK_User_Manual.pdf release/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/RFPK_SPK_TERMS_OF_SERVICE.html release/java-components/webapps/user
rsync -rvl --exclude .svn -R webapps/user/WebHelp release/java-components/webapps/user
rsync -rvl --exclude .svn -R webapps/user/images release/java-components/webapps/user
mkdir -p release/java-components/webapps/user/jnlp
mkdir -p release/java-components/webapps/user/WEB-INF
rsync -rvl --exclude .svn webapps/user/WEB-INF/web.xml_release release/java-components/webapps/user/WEB-INF/web.xml
mkdir -p release/java-components/webapps/src/uw/rfpk/servlets
rsync -rvl --exclude .svn webapps/user/WEB-INF/classes/uw/rfpk/servlets/*.java release/java-components/webapps/src/uw/rfpk/servlets
mkdir -p release/java-components/webapps/src/uw/rfpk/beans
rsync -rvl --exclude .svn webapps/user/WEB-INF/classes/uw/rfpk/beans/*.java release/java-components/webapps/src/uw/rfpk/beans
mkdir -p release/java-components/webapps/src/uw/rfpk/rcs
rsync -rvl --exclude .svn webapps/user/WEB-INF/classes/uw/rfpk/rcs/*.java release/java-components/webapps/src/uw/rfpk/rcs
mkdir -p release/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/jaxp-api.jar release/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/jstl.jar release/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/servlet-api.jar release/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/standard.jar release/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/oraclasses_3_0.jar release/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/orataglib_3_0.jar release/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/mysql-connector-java-3.0.10-stable-bin.jar release/java-components/webapps/user/WEB-INF/lib

# rcs
mkdir -p release/java-components/rcs
rsync -rvl --exclude .svn rcs/*.* release/java-components/rcs
rsync -rvl --exclude .svn rcs/install release/java-components/rcs

# mail
mkdir -p release/java-components/mail
rsync -rvl --exclude .svn mail/*.* release/java-components/mail
rsync -rvl --exclude .svn mail/install release/java-components/mail

# dbapi
mkdir -p release/java-components/dbapi
rsync -rvl --exclude .svn db/api/java/build.xml release/java-components/dbapi
mkdir -p release/java-components/dbapi/src
rsync -rvl --exclude .svn -R db/api/java/rfpk release/java-components/dbapi/src
mkdir -p release/java-components/dbapi/test
rsync -rvl --exclude .svn db/api/java/TestSpkdb.java release/java-components/dbapi/test

# jobqs
mkdir -p release/java-components/jobqs
rsync -rvl --exclude .svn jobqs/build.xml release/java-components/jobqs
rsync -rvl --exclude .svn jobqs/README release/java-components/jobqs
rsync -rvl --exclude .svn jobqs/install release/java-components/jobqs
rsync -rvl --exclude .svn jobqs/jobqd release/java-components/jobqs
rsync -rvl --exclude .svn jobqs/jobqtestd release/java-components/jobqs
rsync -rvl --exclude .svn jobqs/jobqd.pl release/java-components/jobqs
mkdir -p release/java-components/jobqs/src
rsync -rvl --exclude .svn -R jobqs/uw release/java-components/jobqs/src
mkdir -p release/java-components/jobqs/test
rsync -rvl --exclude .svn jobqs/TestJobqs.java release/java-components/jobqs/test



cat <<EOF >release/java-components/Build.sh
#!/bin/bash

for file in dbapi mda webapps; 
  do cd \${file}; 
  ant;
  cd ..;
done;

EOF

chmod +x release/java-components/Build.sh


