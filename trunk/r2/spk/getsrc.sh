#!/bin/bash

# clean up
rm -rf ~/sysforpopkinetics/r2/release/java-components

# mda
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda
cp ~/sysforpopkinetics/r2/spk/mda/build.xml ~/sysforpopkinetics/r2/release/java-components/mda/
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/lib
cp ~/sysforpopkinetics/r2/spk/mda/lib/*.jar ~/sysforpopkinetics/r2/release/java-components/mda/lib
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda
cp ~/sysforpopkinetics/r2/spk/mda/uw/rfpk/mda/*.java ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/
cp ~/sysforpopkinetics/r2/spk/mda/uw/rfpk/mda/*.png ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem
cp ~/sysforpopkinetics/r2/spk/mda/uw/rfpk/mda/nonmem/*.java ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
cp ~/sysforpopkinetics/r2/spk/mda/uw/rfpk/mda/nonmem/wizard/*.java ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
cp -R ~/sysforpopkinetics/r2/spk/mda/uw/rfpk/mda/nonmem/wizard/icons ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/display
cp ~/sysforpopkinetics/r2/spk/mda/uw/rfpk/mda/nonmem/display/*.java ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/display
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
cp ~/sysforpopkinetics/r2/spk/mda/uw/rfpk/mda/nonmem/compartment/*.java ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
cp ~/sysforpopkinetics/r2/spk/mda/uw/rfpk/mda/nonmem/compartment/*.png ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
cp ~/sysforpopkinetics/r2/spk/mda/uw/rfpk/mda/nonmem/compartment/*.jpg ~/sysforpopkinetics/r2/release/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard
cp ~/sysforpopkinetics/r2/spk/mda/org/netbeans/ui/wizard/*.java ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard/plaf
cp ~/sysforpopkinetics/r2/spk/mda/org/netbeans/ui/wizard/plaf/*.java ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard/plaf
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic
cp ~/sysforpopkinetics/r2/spk/mda/org/netbeans/ui/wizard/plaf/basic/*.java ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
cp ~/sysforpopkinetics/r2/spk/mda/org/netbeans/ui/wizard/plaf/basic/icons/*.gif ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources
cp ~/sysforpopkinetics/r2/spk/mda/org/netbeans/ui/wizard/plaf/basic/resources/*.properties ~/sysforpopkinetics/r2/release/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources

# webapps
mkdir -p ~/sysforpopkinetics/r2/release/java-components/webapps
cp ~/sysforpopkinetics/r2/spk/webapps/build.xml ~/sysforpopkinetics/r2/release/java-components/webapps
cp ~/sysforpopkinetics/r2/spk/webapps/README ~/sysforpopkinetics/r2/release/java-components/webapps
mkdir -p ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp ~/sysforpopkinetics/r2/spk/webapps/user/*.jsp ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp ~/sysforpopkinetics/r2/spk/webapps/user/*.shtml ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp ~/sysforpopkinetics/r2/spk/webapps/user/*.css ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp ~/sysforpopkinetics/r2/spk/webapps/user/*.java ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp ~/sysforpopkinetics/r2/spk/webapps/user/*.class ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp ~/sysforpopkinetics/r2/spk/mda/lib/jhall.jar ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp ~/sysforpopkinetics/r2/spk/webapps/user/JavaHelp.jar ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp ~/sysforpopkinetics/r2/spk/webapps/user/SPK_User_Manual.pdf ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp ~/sysforpopkinetics/r2/spk/webapps/user/RFPK_SPK_TERMS_OF_SERVICE.html ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp -R ~/sysforpopkinetics/r2/spk/webapps/user/WebHelp ~/sysforpopkinetics/r2/release/java-components/webapps/user
cp -R ~/sysforpopkinetics/r2/spk/webapps/user/images ~/sysforpopkinetics/r2/release/java-components/webapps/user
mkdir -p ~/sysforpopkinetics/r2/release/java-components/webapps/user/jnlp
mkdir -p ~/sysforpopkinetics/r2/release/java-components/webapps/user/WEB-INF
cp ~/sysforpopkinetics/r2/spk/webapps/user/WEB-INF/web.xml_release ~/sysforpopkinetics/r2/release/java-components/webapps/user/WEB-INF/web.xml
mkdir -p ~/sysforpopkinetics/r2/release/java-components/webapps/src/uw/rfpk/servlets
cp ~/sysforpopkinetics/r2/spk/webapps/user/WEB-INF/classes/uw/rfpk/servlets/*.java ~/sysforpopkinetics/r2/release/java-components/webapps/src/uw/rfpk/servlets
mkdir -p ~/sysforpopkinetics/r2/release/java-components/webapps/src/uw/rfpk/beans
cp ~/sysforpopkinetics/r2/spk/webapps/user/WEB-INF/classes/uw/rfpk/beans/*.java ~/sysforpopkinetics/r2/release/java-components/webapps/src/uw/rfpk/beans
mkdir -p ~/sysforpopkinetics/r2/release/java-components/webapps/src/uw/rfpk/rcs
cp ~/sysforpopkinetics/r2/spk/webapps/user/WEB-INF/classes/uw/rfpk/rcs/*.java ~/sysforpopkinetics/r2/release/java-components/webapps/src/uw/rfpk/rcs
mkdir -p ~/sysforpopkinetics/r2/release/java-components/webapps/user/WEB-INF/lib
cp ~/sysforpopkinetics/r2/spk/webapps/user/WEB-INF/lib/jstl.jar ~/sysforpopkinetics/r2/release/java-components/webapps/user/WEB-INF/lib
cp ~/sysforpopkinetics/r2/spk/webapps/user/WEB-INF/lib/standard.jar ~/sysforpopkinetics/r2/release/java-components/webapps/user/WEB-INF/lib
cp ~/sysforpopkinetics/r2/spk/webapps/user/WEB-INF/lib/orataglib_3_0.jar ~/sysforpopkinetics/r2/release/java-components/webapps/user/WEB-INF/lib
cp ~/sysforpopkinetics/r2/spk/webapps/user/WEB-INF/lib/mysql-connector-java-3.0.10-stable-bin.jar ~/sysforpopkinetics/r2/release/java-components/webapps/user/WEB-INF/lib

# rcs
mkdir -p ~/sysforpopkinetics/r2/release/java-components/rcs
cp ~/sysforpopkinetics/r2/spk/rcs/*.* ~/sysforpopkinetics/r2/release/java-components/rcs
cp ~/sysforpopkinetics/r2/spk/rcs/install ~/sysforpopkinetics/r2/release/java-components/rcs

# mail
mkdir -p ~/sysforpopkinetics/r2/release/java-components/mail
cp ~/sysforpopkinetics/r2/spk/mail/*.* ~/sysforpopkinetics/r2/release/java-components/mail
cp ~/sysforpopkinetics/r2/spk/mail/install ~/sysforpopkinetics/r2/release/java-components/mail

# dbapi
mkdir -p ~/sysforpopkinetics/r2/release/java-components/dbapi
cp ~/sysforpopkinetics/r2/spk/db/api/java/build.xml ~/sysforpopkinetics/r2/release/java-components/dbapi
mkdir -p ~/sysforpopkinetics/r2/release/java-components/dbapi/src
cp -R ~/sysforpopkinetics/r2/spk/db/api/java/rfpk ~/sysforpopkinetics/r2/release/java-components/dbapi/src
mkdir -p ~/sysforpopkinetics/r2/release/java-components/dbapi/test
cp ~/sysforpopkinetics/r2/spk/db/api/java/TestSpkdb.java ~/sysforpopkinetics/r2/release/java-components/dbapi/test

# jobqs
mkdir -p ~/sysforpopkinetics/r2/release/java-components/jobqs
cp ~/sysforpopkinetics/r2/spk/jobqs/build.xml ~/sysforpopkinetics/r2/release/java-components/jobqs
cp ~/sysforpopkinetics/r2/spk/jobqs/README ~/sysforpopkinetics/r2/release/java-components/jobqs
cp ~/sysforpopkinetics/r2/spk/jobqs/install ~/sysforpopkinetics/r2/release/java-components/jobqs
cp ~/sysforpopkinetics/r2/spk/jobqs/jobqd ~/sysforpopkinetics/r2/release/java-components/jobqs
cp ~/sysforpopkinetics/r2/spk/jobqs/jobqtestd ~/sysforpopkinetics/r2/release/java-components/jobqs
cp ~/sysforpopkinetics/r2/spk/jobqs/jobqd.pl ~/sysforpopkinetics/r2/release/java-components/jobqs
mkdir -p ~/sysforpopkinetics/r2/release/java-components/jobqs/src
cp -R ~/sysforpopkinetics/r2/spk/jobqs/uw ~/sysforpopkinetics/r2/release/java-components/jobqs/src
mkdir -p ~/sysforpopkinetics/r2/release/java-components/jobqs/test
cp ~/sysforpopkinetics/r2/spk/jobqs/TestJobqs.java ~/sysforpopkinetics/r2/release/java-components/jobqs/test



cat <<EOF >~/sysforpopkinetics/r2/release/java-components/Build.sh
#!/bin/bash

for file in dbapi mda webapps; 
  do cd \${file}; 
  ant;
  cd ..;
done;

EOF

chmod +x ~/sysforpopkinetics/r2/release/java-components/Build.sh


