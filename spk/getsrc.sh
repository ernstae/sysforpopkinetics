#!/bin/bash

# clean up
rm -rf ~/r2/trunk/release/java-components

# mda
mkdir -p ~/r2/trunk/release/java-components/mda
cp ~/r2/trunk/spk/mda/build.xml ~/r2/trunk/release/java-components/mda/
mkdir -p ~/r2/trunk/release/java-components/mda/lib
cp ~/r2/trunk/spk/mda/lib/*.jar ~/r2/trunk/release/java-components/mda/lib
mkdir -p ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/*.java ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/*.png ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/
mkdir -p ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/*.java ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/
mkdir -p ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/wizard/*.java ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
cp -R ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/wizard/icons ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
mkdir -p ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/display
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/display/*.java ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/display
mkdir -p ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/compartment/*.java ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/compartment/*.png ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/compartment/*.jpg ~/r2/trunk/release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
mkdir -p ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/*.java ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard
mkdir -p ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard/plaf
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/plaf/*.java ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard/plaf
mkdir -p ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/plaf/basic/*.java ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic
mkdir -p ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/plaf/basic/icons/*.gif ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
mkdir -p ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/plaf/basic/resources/*.properties ~/r2/trunk/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources

# webapps
mkdir -p ~/r2/trunk/release/java-components/webapps
cp ~/r2/trunk/spk/webapps/build.xml ~/r2/trunk/release/java-components/webapps
cp ~/r2/trunk/spk/webapps/README ~/r2/trunk/release/java-components/webapps
mkdir -p ~/r2/trunk/release/java-components/webapps/user
cp ~/r2/trunk/spk/webapps/user/*.jsp ~/r2/trunk/release/java-components/webapps/user
cp ~/r2/trunk/spk/webapps/user/*.shtml ~/r2/trunk/release/java-components/webapps/user
cp ~/r2/trunk/spk/webapps/user/*.css ~/r2/trunk/release/java-components/webapps/user
cp ~/r2/trunk/spk/webapps/user/*.java ~/r2/trunk/release/java-components/webapps/user
cp ~/r2/trunk/spk/webapps/user/*.class ~/r2/trunk/release/java-components/webapps/user
cp ~/r2/trunk/spk/mda/lib/*.jar ~/r2/trunk/release/java-components/webapps/user
cp ~/r2/trunk/spk/webapps/user/JavaHelp.jar ~/r2/trunk/release/java-components/webapps/user
cp ~/r2/trunk/spk/webapps/user/SPK_User_Manual.pdf ~/r2/trunk/release/java-components/webapps/user
cp ~/r2/trunk/spk/webapps/user/RFPK_SPK_TERMS_OF_SERVICE.html ~/r2/trunk/release/java-components/webapps/user
cp -R ~/r2/trunk/spk/webapps/user/WebHelp ~/r2/trunk/release/java-components/webapps/user
cp -R ~/r2/trunk/spk/webapps/user/images ~/r2/trunk/release/java-components/webapps/user
mkdir -p ~/r2/trunk/release/java-components/webapps/user/jnlp
mkdir -p ~/r2/trunk/release/java-components/webapps/user/WEB-INF
cp ~/r2/trunk/spk/webapps/user/WEB-INF/web.xml_release ~/r2/trunk/release/java-components/webapps/user/WEB-INF/web.xml
mkdir -p ~/r2/trunk/release/java-components/webapps/src/uw/rfpk/servlets
cp ~/r2/trunk/spk/webapps/user/WEB-INF/classes/uw/rfpk/servlets/*.java ~/r2/trunk/release/java-components/webapps/src/uw/rfpk/servlets
mkdir -p ~/r2/trunk/release/java-components/webapps/src/uw/rfpk/beans
cp ~/r2/trunk/spk/webapps/user/WEB-INF/classes/uw/rfpk/beans/*.java ~/r2/trunk/release/java-components/webapps/src/uw/rfpk/beans
mkdir -p ~/r2/trunk/release/java-components/webapps/src/uw/rfpk/rcs
cp ~/r2/trunk/spk/webapps/user/WEB-INF/classes/uw/rfpk/rcs/*.java ~/r2/trunk/release/java-components/webapps/src/uw/rfpk/rcs
mkdir -p ~/r2/trunk/release/java-components/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/jaxp-api.jar ~/r2/trunk/release/java-components/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/jstl.jar ~/r2/trunk/release/java-components/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/servlet-api.jar ~/r2/trunk/release/java-components/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/standard.jar ~/r2/trunk/release/java-components/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/oraclasses_3_0.jar ~/r2/trunk/release/java-components/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/orataglib_3_0.jar ~/r2/trunk/release/java-components/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/mysql-connector-java-3.0.10-stable-bin.jar ~/r2/trunk/release/java-components/webapps/user/WEB-INF/lib

# rcs
mkdir -p ~/r2/trunk/release/java-components/rcs
cp ~/r2/trunk/spk/rcs/*.* ~/r2/trunk/release/java-components/rcs
cp ~/r2/trunk/spk/rcs/install ~/r2/trunk/release/java-components/rcs

# mail
mkdir -p ~/r2/trunk/release/java-components/mail
cp ~/r2/trunk/spk/mail/*.* ~/r2/trunk/release/java-components/mail
cp ~/r2/trunk/spk/mail/install ~/r2/trunk/release/java-components/mail

# dbapi
mkdir -p ~/r2/trunk/release/java-components/dbapi
cp ~/r2/trunk/spk/db/api/java/build.xml ~/r2/trunk/release/java-components/dbapi
mkdir -p ~/r2/trunk/release/java-components/dbapi/src
cp -R ~/r2/trunk/spk/db/api/java/rfpk ~/r2/trunk/release/java-components/dbapi/src
mkdir -p ~/r2/trunk/release/java-components/dbapi/test
cp ~/r2/trunk/spk/db/api/java/TestSpkdb.java ~/r2/trunk/release/java-components/dbapi/test

# jobqs
mkdir -p ~/r2/trunk/release/java-components/jobqs
cp ~/r2/trunk/spk/jobqs/build.xml ~/r2/trunk/release/java-components/jobqs
cp ~/r2/trunk/spk/jobqs/README ~/r2/trunk/release/java-components/jobqs
cp ~/r2/trunk/spk/jobqs/install ~/r2/trunk/release/java-components/jobqs
cp ~/r2/trunk/spk/jobqs/jobqd ~/r2/trunk/release/java-components/jobqs
cp ~/r2/trunk/spk/jobqs/jobqtestd ~/r2/trunk/release/java-components/jobqs
cp ~/r2/trunk/spk/jobqs/jobqd.pl ~/r2/trunk/release/java-components/jobqs
mkdir -p ~/r2/trunk/release/java-components/jobqs/src
cp -R ~/r2/trunk/spk/jobqs/uw ~/r2/trunk/release/java-components/jobqs/src
mkdir -p ~/r2/trunk/release/java-components/jobqs/test
cp ~/r2/trunk/spk/jobqs/TestJobqs.java ~/r2/trunk/release/java-components/jobqs/test



cat <<EOF >~/r2/trunk/release/java-components/Build.sh
#!/bin/bash

for file in dbapi mda webapps; 
  do cd \${file}; 
  ant;
  cd ..;
done;

EOF

chmod +x ~/r2/trunk/release/java-components/Build.sh


