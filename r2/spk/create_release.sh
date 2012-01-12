#!/bin/bash

RELEASE_NAME="spk-release";
RELEASE_VER_BASE="1.0.";
RELEASE_VER_BUILD=`svnversion -n | sed 's/:.*$//g' |  sed 's/[^0-9]*//g'`;

SVN_BASE="svn+ssh://toronto.rfpk.washington.edu/u01/local";

RELEASE_DIR="${RELEASE_NAME}-${RELEASE_VER_BASE}${RELEASE_VER_BUILD}";





# clean up
rm -rf ${RELEASE_NAME}*
mkdir -p ${RELEASE_DIR}/contrib

# spk libraries
rsync -rvl --exclude .svn aspk cspk db *.pl LICENSE INSTALL README ${RELEASE_DIR}

echo "Getting CPPAD from COIN"
CPPAD_URL="http://www.coin-or.org/download/source/CppAD"
CPPAD_TARBALL="cppad-20120111.gpl.tgz"
(cd ${RELEASE_DIR}/contrib; wget ${CPPAD_URL}/${CPPAD_TARBALL}; tar xvfz ${CPPAD_TARBALL}; rm -rf ${CPPAD_TARBALL}; mv cppad* cppad )

echo "Getting QN01Box from r1"
cp -a ../../r1/Optimizer/QN01Box ${RELEASE_DIR}/contrib/QN01Box

cp -a ../../r1/brad/non_par ${RELEASE_DIR}/contrib/non_par

echo "Getting mat2cpp from the archive"

(cd ${RELEASE_DIR}/contrib; cp -a ../../../../archive/mat2cpp . )

echo "Getting OMHelp from the web"
OMHELP_URL="http://www.seanet.com/~bradbell"
OMHELP_TARBALL="OMhelp.unix.tar.gz"
(cd ${RELEASE_DIR}/contrib; wget ${OMHELP_URL}/${OMHELP_TARBALL}; tar xvfz ${OMHELP_TARBALL}; rm -rf ${OMHELP_TARBALL}; mv omhelp* omhelp )


# fix webdir.mk problems
echo "Patching spkcompiler Makefile.am"
sed -i "/^include/s/.*webdir.mk/WEBDIR=./g" ${RELEASE_DIR}/aspk/spkcompiler/doc/Makefile.am

for file in cspk/spk cspk/spkpred aspk/spkcompiler contrib/non_par contrib/QN01Box;
do
echo "Performing Autotools within ${file}"
(cd ${RELEASE_DIR}/${file}; libtoolize -c --force; aclocal; autoheader; autoconf; automake -c --add-missing; )
done



# mda
mkdir -p ${RELEASE_DIR}/java-components/contrib
(cd ${RELEASE_DIR}/java-components/contrib; wget http://www.hansbergsten.com/jspbook3.zip; unzip jspbook3.zip; )

mkdir -p ${RELEASE_DIR}/java-components/mda
rsync -rvl --exclude .svn mda/build.xml ${RELEASE_DIR}/java-components/mda/

mkdir -p ${RELEASE_DIR}/java-components/mda/lib
rsync -rvl --exclude .svn mda/lib/*.jar ${RELEASE_DIR}/java-components/mda/lib

mkdir -p ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda
rsync -rvl --exclude .svn mda/uw/rfpk/mda/*.java ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/
rsync -rvl --exclude .svn mda/uw/rfpk/mda/*.png ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/

mkdir -p ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/*.java ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/

mkdir -p ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/wizard/*.java ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/wizard
rsync -rvl --exclude .svn -R mda/uw/rfpk/mda/nonmem/wizard/icons ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/wizard

mkdir -p ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/display
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/display/*.java ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/display

mkdir -p ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/compartment/*.java ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/compartment/*.png ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/compartment
rsync -rvl --exclude .svn mda/uw/rfpk/mda/nonmem/compartment/*.jpg ${RELEASE_DIR}/java-components/mda/src/uw/rfpk/mda/nonmem/compartment

mkdir -p ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/*.java ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard

mkdir -p ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard/plaf
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/plaf/*.java ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard/plaf

mkdir -p ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/plaf/basic/*.java ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic

mkdir -p ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/plaf/basic/icons/*.gif ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/icons
mkdir -p ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources
rsync -rvl --exclude .svn mda/org/netbeans/ui/wizard/plaf/basic/resources/*.properties ${RELEASE_DIR}/java-components/mda/src/org/netbeans/ui/wizard/plaf/basic/resources


# webapps
mkdir -p ${RELEASE_DIR}/java-components/webapps
rsync -rvl --exclude .svn webapps/build.xml ${RELEASE_DIR}/java-components/webapps
rsync -rvl --exclude .svn webapps/README ${RELEASE_DIR}/java-components/webapps
mkdir -p ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.jsp ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.shtml ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.css ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.java ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/*.class ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn mda/lib/*.jar ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/JavaHelp.jar ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/SPK_User_Manual.pdf ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn webapps/user/RFPK_SPK_TERMS_OF_SERVICE.html ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn -R webapps/user/WebHelp ${RELEASE_DIR}/java-components/webapps/user
rsync -rvl --exclude .svn -R webapps/user/images ${RELEASE_DIR}/java-components/webapps/user
mkdir -p ${RELEASE_DIR}/java-components/webapps/user/jnlp
mkdir -p ${RELEASE_DIR}/java-components/webapps/user/WEB-INF
rsync -rvl --exclude .svn webapps/user/WEB-INF/web.xml_release ${RELEASE_DIR}/java-components/webapps/user/WEB-INF/web.xml
mkdir -p ${RELEASE_DIR}/java-components/webapps/src/uw/rfpk/servlets
rsync -rvl --exclude .svn webapps/user/WEB-INF/classes/uw/rfpk/servlets/*.java ${RELEASE_DIR}/java-components/webapps/src/uw/rfpk/servlets
mkdir -p ${RELEASE_DIR}/java-components/webapps/src/uw/rfpk/beans
rsync -rvl --exclude .svn webapps/user/WEB-INF/classes/uw/rfpk/beans/*.java ${RELEASE_DIR}/java-components/webapps/src/uw/rfpk/beans
mkdir -p ${RELEASE_DIR}/java-components/webapps/src/uw/rfpk/rcs
rsync -rvl --exclude .svn webapps/user/WEB-INF/classes/uw/rfpk/rcs/*.java ${RELEASE_DIR}/java-components/webapps/src/uw/rfpk/rcs
mkdir -p ${RELEASE_DIR}/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/jaxp-api.jar ${RELEASE_DIR}/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/jstl.jar ${RELEASE_DIR}/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/servlet-api.jar ${RELEASE_DIR}/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/standard.jar ${RELEASE_DIR}/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/oraclasses_3_0.jar ${RELEASE_DIR}/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/orataglib_3_0.jar ${RELEASE_DIR}/java-components/webapps/user/WEB-INF/lib
rsync -rvl --exclude .svn webapps/user/WEB-INF/lib/mysql-connector-java-3.0.10-stable-bin.jar ${RELEASE_DIR}/java-components/webapps/user/WEB-INF/lib

# rcs
mkdir -p ${RELEASE_DIR}/java-components/rcs
rsync -rvl --exclude .svn rcs/*.* ${RELEASE_DIR}/java-components/rcs
rsync -rvl --exclude .svn rcs/install ${RELEASE_DIR}/java-components/rcs

# mail
mkdir -p ${RELEASE_DIR}/java-components/mail
rsync -rvl --exclude .svn mail/*.* ${RELEASE_DIR}/java-components/mail
rsync -rvl --exclude .svn mail/install ${RELEASE_DIR}/java-components/mail

# dbapi
mkdir -p ${RELEASE_DIR}/java-components/dbapi
rsync -rvl --exclude .svn db/api/java/build.xml ${RELEASE_DIR}/java-components/dbapi
mkdir -p ${RELEASE_DIR}/java-components/dbapi/src
rsync -rvl --exclude .svn -R db/api/java/rfpk ${RELEASE_DIR}/java-components/dbapi/src
mkdir -p ${RELEASE_DIR}/java-components/dbapi/test
rsync -rvl --exclude .svn db/api/java/TestSpkdb.java ${RELEASE_DIR}/java-components/dbapi/test

# jobqs
mkdir -p ${RELEASE_DIR}/java-components/jobqs
rsync -rvl --exclude .svn jobqs/build.xml ${RELEASE_DIR}/java-components/jobqs
rsync -rvl --exclude .svn jobqs/README ${RELEASE_DIR}/java-components/jobqs
rsync -rvl --exclude .svn jobqs/install ${RELEASE_DIR}/java-components/jobqs
rsync -rvl --exclude .svn jobqs/jobqd ${RELEASE_DIR}/java-components/jobqs
rsync -rvl --exclude .svn jobqs/jobqtestd ${RELEASE_DIR}/java-components/jobqs
rsync -rvl --exclude .svn jobqs/jobqd.pl ${RELEASE_DIR}/java-components/jobqs
mkdir -p ${RELEASE_DIR}/java-components/jobqs/src
rsync -rvl --exclude .svn -R jobqs/uw ${RELEASE_DIR}/java-components/jobqs/src
mkdir -p ${RELEASE_DIR}/java-components/jobqs/test
rsync -rvl --exclude .svn jobqs/TestJobqs.java ${RELEASE_DIR}/java-components/jobqs/test



cat <<EOF >${RELEASE_DIR}/java-components/Build.sh
#!/bin/bash

for file in dbapi mda webapps; 
  do cd \${file}; 
  ant;
  cd ..;
done;

EOF

chmod +x ${RELEASE_DIR}/java-components/Build.sh

# create the version.h file in the cspk/spk/spk directory, so the makefile doesn't need to generate it
# for the end user.  Because it relies on svnversion -n at compile time, and the user will not be 
# building this from an SVN working directory, svnversion will report "exported", and the user will
# end up with a version.h file that indicates version 1.0.exported rather than the proper SVN build
# number
echo '#ifndef VERSION_H' > cspk/spk/spk/version.h
echo '#define VERSION_H' >> cspk/spk/spk/version.h
echo -n 'const char* version = "1.0.' >> cspk/spk/spk/version.h
svnversion -n . >> cspk/spk/spk/version.h
echo '"; ' >> cspk/spk/spk/version.h
echo '#endif' >> cspk/spk/spk/version.h




tar cvfz ${RELEASE_DIR}.tar.gz ${RELEASE_DIR}
