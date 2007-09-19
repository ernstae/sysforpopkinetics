# clean up
rm -rf Build

# mda
mkdir -p Build/mda
cp ~/r2/trunk/spk/mda/build.xml Build/mda/
mkdir -p Build/mda/lib
cp ~/r2/trunk/spk/mda/lib/*.jar Build/mda/lib
mkdir -p Build/mda/src/uw/rfpk/mda
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/*.java Build/mda/src/uw/rfpk/mda/
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/*.png Build/mda/src/uw/rfpk/mda/
mkdir -p Build/mda/src/uw/rfpk/mda/nonmem
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/*.java Build/mda/src/uw/rfpk/mda/nonmem/
mkdir -p Build/mda/src/uw/rfpk/mda/nonmem/wizard
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/wizard/*.java Build/mda/src/uw/rfpk/mda/nonmem/wizard
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/wizard/icons/*.* Build/mda/src/uw/rfpk/mda/nonmem/wizard
mkdir -p Build/mda/src/uw/rfpk/mda/nonmem/display
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/display/*.java Build/mda/src/uw/rfpk/mda/nonmem/display
mkdir -p Build/mda/src/uw/rfpk/mda/nonmem/compartment
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/compartment/*.java Build/mda/src/uw/rfpk/mda/nonmem/compartment
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/compartment/*.png Build/mda/src/uw/rfpk/mda/nonmem/compartment
cp ~/r2/trunk/spk/mda/uw/rfpk/mda/nonmem/compartment/*.jpg Build/mda/src/uw/rfpk/mda/nonmem/compartment
mkdir -p Build/mda/src/org/netbeans/ui/wizard
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/*.java Build/mda/src/org/netbeans/ui/wizard
mkdir -p Build/mda/src/org/netbeans/ui/wizard/plaf
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/plaf/*.java Build/mda/src/org/netbeans/ui/wizard/plaf
mkdir -p Build/mda/src/org/netbeans/ui/wizard/plaf/basic
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/plaf/basic/*.java Build/mda/src/org/netbeans/ui/wizard/plaf/basic
mkdir -p Build/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/plaf/basic/icons/*.gif Build/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
mkdir -p Build/mda/src/org/netbeans/ui/wizard/plaf/basic/resources
cp ~/r2/trunk/spk/mda/org/netbeans/ui/wizard/plaf/basic/resources/*.properties Build/mda/src/org/netbeans/ui/wizard/plaf/basic/resources

# webapps
mkdir -p Build/webapps
cp ~/r2/trunk/spk/webapps/build.xml Build/webapps
cp ~/r2/trunk/spk/webapps/README Build/webapps
mkdir -p Build/webapps/user
cp ~/r2/trunk/spk/webapps/user/*.jsp Build/webapps/user
cp ~/r2/trunk/spk/webapps/user/*.shtml Build/webapps/user
cp ~/r2/trunk/spk/webapps/user/*.css Build/webapps/user
cp ~/r2/trunk/spk/mda/lib/*.jar Build/webapps/user
cp ~/r2/trunk/spk/webapps/user/JavaHelp.jar Build/webapps/user
cp ~/r2/trunk/spk/webapps/user/SPK_User_Manual.pdf Build/webapps/user
cp ~/r2/trunk/spk/webapps/user/RFPK_SPK_TERMS_OF_SERVICE.html Build/webapps/user
cp -R ~/r2/trunk/spk/webapps/user/WebHelp Build/webapps/user
mkdir -p Build/webapps/user/images
cp ~/r2/trunk/spk/webapps/user/images/*.gif Build/webapps/user/images
cp ~/r2/trunk/spk/webapps/user/images/*.psd Build/webapps/user/images
mkdir -p Build/webapps/user/jnlp
mkdir -p Build/webapps/user/WEB-INF
cp ~/r2/trunk/spk/webapps/user/WEB-INF/web.xml_release Build/webapps/user/WEB-INF/web.xml
mkdir -p Build/webapps/src/uw/rfpk/servlets
cp ~/r2/trunk/spk/webapps/user/WEB-INF/classes/uw/rfpk/servlets/*.java Build/webapps/src/uw/rfpk/servlets
mkdir -p Build/webapps/src/uw/rfpk/beans
cp ~/r2/trunk/spk/webapps/user/WEB-INF/classes/uw/rfpk/beans/*.java Build/webapps/src/uw/rfpk/beans
mkdir -p Build/webapps/src/uw/rfpk/rcs
cp ~/r2/trunk/spk/webapps/user/WEB-INF/classes/uw/rfpk/rcs/*.java Build/webapps/src/uw/rfpk/rcs
mkdir -p Build/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/jaxp-api.jar Build/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/jstl.jar Build/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/servlet-api.jar Build/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/standard.jar Build/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/oraclasses_3_0.jar Build/webapps/user/WEB-INF/lib
cp ~/r2/trunk/spk/webapps/user/WEB-INF/lib/orataglib_3_0.jar Build/webapps/user/WEB-INF/lib

# rcs
mkdir -p Build/rcs
cp ~/r2/trunk/spk/rcs/*.* Build/rcs
cp ~/r2/trunk/spk/rcs/install Build/rcs

# mail
mkdir -p Build/mail
cp ~/r2/trunk/spk/mail/*.* Build/mail
cp ~/r2/trunk/spk/mail/install Build/mail

# dbapi
mkdir -p Build/dbapi
cp ~/r2/trunk/spk/db/api/java/build.xml Build/dbapi
mkdir -p Build/dbapi/src
cp -R ~/r2/trunk/spk/db/api/java/rfpk Build/dbapi/src
mkdir -p Build/dbapi/test
cp ~/r2/trunk/spk/db/api/java/TestSpkdb.java Build/dbapi/test

# jobqs
mkdir -p Build/jobqs
cp ~/r2/trunk/spk/jobqs/build.xml Build/jobqs
cp ~/r2/trunk/spk/jobqs/README Build/jobqs
cp ~/r2/trunk/spk/jobqs/install Build/jobqs
cp ~/r2/trunk/spk/jobqs/jobqd Build/jobqs
cp ~/r2/trunk/spk/jobqs/jobqtestd Build/jobqs
cp ~/r2/trunk/spk/jobqs/jobqd.pl Build/jobqs
mkdir -p Build/jobqs/src
cp -R ~/r2/trunk/spk/jobqs/uw Build/jobqs/src
mkdir -p Build/jobqs/test
cp ~/r2/trunk/spk/jobqs/TestJobqs.java Build/jobqs/test






