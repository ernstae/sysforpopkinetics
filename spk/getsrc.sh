#!/bin/bash

# clean up
rm -rf release/java-components

# mda
mkdir -p release/java-components/mda
cp mda/build.xml release/java-components/mda/
mkdir -p release/java-components/mda/lib
cp mda/lib/*.jar release/java-components/mda/lib
mkdir -p release/java-components/mda/src/uw/rfpk/mda
cp mda/uw/rfpk/mda/*.java release/java-components/mda/src/uw/rfpk/mda/
cp mda/uw/rfpk/mda/*.png release/java-components/mda/src/uw/rfpk/mda/
mkdir -p release/java-components/mda/src/uw/rfpk/mda/nonmem
cp mda/uw/rfpk/mda/nonmem/*.java release/java-components/mda/src/uw/rfpk/mda/nonmem/
mkdir -p release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
cp mda/uw/rfpk/mda/nonmem/wizard/*.java release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
cp -R mda/uw/rfpk/mda/nonmem/wizard/icons release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
mkdir -p release/java-components/mda/src/uw/rfpk/mda/nonmem/display
cp mda/uw/rfpk/mda/nonmem/display/*.java release/java-components/mda/src/uw/rfpk/mda/nonmem/display
mkdir -p release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
cp mda/uw/rfpk/mda/nonmem/compartment/*.java release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
cp mda/uw/rfpk/mda/nonmem/compartment/*.png release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
cp mda/uw/rfpk/mda/nonmem/compartment/*.jpg release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard
cp mda/org/netbeans/ui/wizard/*.java release/java-components/mda/src/org/netbeans/ui/wizard
mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard/plaf
cp mda/org/netbeans/ui/wizard/plaf/*.java release/java-components/mda/src/org/netbeans/ui/wizard/plaf
mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic
cp mda/org/netbeans/ui/wizard/plaf/basic/*.java release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic
mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
cp mda/org/netbeans/ui/wizard/plaf/basic/icons/*.gif release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
mkdir -p release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources
cp mda/org/netbeans/ui/wizard/plaf/basic/resources/*.properties release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources

# webapps
mkdir -p release/java-components/webapps
cp webapps/build.xml release/java-components/webapps
cp webapps/README release/java-components/webapps
mkdir -p release/java-components/webapps/user
cp webapps/user/*.jsp release/java-components/webapps/user
cp webapps/user/*.shtml release/java-components/webapps/user
cp webapps/user/*.css release/java-components/webapps/user
cp webapps/user/*.java release/java-components/webapps/user
cp webapps/user/*.class release/java-components/webapps/user
cp mda/lib/*.jar release/java-components/webapps/user
cp webapps/user/JavaHelp.jar release/java-components/webapps/user
cp webapps/user/SPK_User_Manual.pdf release/java-components/webapps/user
cp webapps/user/RFPK_SPK_TERMS_OF_SERVICE.html release/java-components/webapps/user
cp -R webapps/user/WebHelp release/java-components/webapps/user
cp -R webapps/user/images release/java-components/webapps/user
mkdir -p release/java-components/webapps/user/jnlp
mkdir -p release/java-components/webapps/user/WEB-INF
cp webapps/user/WEB-INF/web.xml_release release/java-components/webapps/user/WEB-INF/web.xml
mkdir -p release/java-components/webapps/src/uw/rfpk/servlets
cp webapps/user/WEB-INF/classes/uw/rfpk/servlets/*.java release/java-components/webapps/src/uw/rfpk/servlets
mkdir -p release/java-components/webapps/src/uw/rfpk/beans
cp webapps/user/WEB-INF/classes/uw/rfpk/beans/*.java release/java-components/webapps/src/uw/rfpk/beans
mkdir -p release/java-components/webapps/src/uw/rfpk/rcs
cp webapps/user/WEB-INF/classes/uw/rfpk/rcs/*.java release/java-components/webapps/src/uw/rfpk/rcs
mkdir -p release/java-components/webapps/user/WEB-INF/lib
cp webapps/user/WEB-INF/lib/jaxp-api.jar release/java-components/webapps/user/WEB-INF/lib
cp webapps/user/WEB-INF/lib/jstl.jar release/java-components/webapps/user/WEB-INF/lib
cp webapps/user/WEB-INF/lib/servlet-api.jar release/java-components/webapps/user/WEB-INF/lib
cp webapps/user/WEB-INF/lib/standard.jar release/java-components/webapps/user/WEB-INF/lib
cp webapps/user/WEB-INF/lib/oraclasses_3_0.jar release/java-components/webapps/user/WEB-INF/lib
cp webapps/user/WEB-INF/lib/orataglib_3_0.jar release/java-components/webapps/user/WEB-INF/lib
cp webapps/user/WEB-INF/lib/mysql-connector-java-3.0.10-stable-bin.jar release/java-components/webapps/user/WEB-INF/lib

# rcs
mkdir -p release/java-components/rcs
cp rcs/*.* release/java-components/rcs
cp rcs/install release/java-components/rcs

# mail
mkdir -p release/java-components/mail
cp mail/*.* release/java-components/mail
cp mail/install release/java-components/mail

# dbapi
mkdir -p release/java-components/dbapi
cp db/api/java/build.xml release/java-components/dbapi
mkdir -p release/java-components/dbapi/src
cp -R db/api/java/rfpk release/java-components/dbapi/src
mkdir -p release/java-components/dbapi/test
cp db/api/java/TestSpkdb.java release/java-components/dbapi/test

# jobqs
mkdir -p release/java-components/jobqs
cp jobqs/build.xml release/java-components/jobqs
cp jobqs/README release/java-components/jobqs
cp jobqs/install release/java-components/jobqs
cp jobqs/jobqd release/java-components/jobqs
cp jobqs/jobqtestd release/java-components/jobqs
cp jobqs/jobqd.pl release/java-components/jobqs
mkdir -p release/java-components/jobqs/src
cp -R jobqs/uw release/java-components/jobqs/src
mkdir -p release/java-components/jobqs/test
cp jobqs/TestJobqs.java release/java-components/jobqs/test



cat <<EOF >release/java-components/Build.sh
#!/bin/bash

for file in dbapi mda webapps; 
  do cd \${file}; 
  ant;
  cd ..;
done;

EOF

chmod +x release/java-components/Build.sh


