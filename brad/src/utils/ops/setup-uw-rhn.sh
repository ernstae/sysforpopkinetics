#!/bin/bash

DATEVAR=`/bin/date "+%Y%m%d"`
UW_ACTIVATION_KEY="811419de5dbf14b0c415cfbea6f88cfb"
REDHAT_RELEASE=`head -1 /etc/issue.net | awk {'print $7'}`
PLATFORM_ARCH=`uname -i`
RFPK_RPM_SERVER="http://toronto.rfpk.washington.edu/software"
VALID_STAFF_USERS="ernst
watrous
brad
salinger
jiaji
"

DEFAULT_PASSWORD="rfpk_member"

UPDATE_DEPENDENCIES="keychain lapack xerces-c xerces-c-devel xerces-c-debuginfo autoconf automake socbook-utils perl-MIME-Lite pvm pvm-gui"
UPDATE_DEVELOPER_PACKAGES="flash-plugin emacs doxygen fonts-xorg-truetype thunderbird firefox acroread cervista bluefish"

echo ""
echo "============================================================"
echo "Importing YUM and RHN repository GPG Keys"
echo "============================================================"
/bin/rpm --import /usr/share/rhn/RPM-GPG-KEY
/bin/rpm --import http://dries.ulyssis.org/rpm/RPM-GPG-KEY.dries.txt

echo ""
/usr/bin/wget http://www.washington.edu/computing/ca/load-pem.crt -O /usr/share/rhn/UW-CA-CERT

# make backup of up2date file and 
# configure up2date to connect to the UW RHN server (as part of our site license)
echo ""
echo "============================================================"
echo "Registering your computer with the UW RHN Satellite Servers"
echo "============================================================"
cp -fv /etc/sysconfig/rhn/up2date /etc/sysconfig/rhn/up2date.bak.$DATEVAR.$$
sed -e 's|sslCACert=/usr/share/rhn/RHNS-CA-CERT|sslCACert=/usr/share/rhn/UW-CA-CERT|g' -e 's|serverURL=https://xmlrpc.rhn.redhat.com/XMLRPC|serverURL=https://rhnsat.washington.edu/XMLRPC|g' /etc/sysconfig/rhn/up2date.bak.$DATEVAR.$$ >/etc/sysconfig/rhn/up2date


# Add the Dries repository to /etc/sysconfig/rhn/sources as a YUM option
echo ""
echo "============================================================"
echo "Adding the DRIES software repository to your RedHat Network"
echo "configuration file (/etc/sysconfig/rhn/sources)"
echo "============================================================"
if grep -i "Red Hat" /etc/issue
then
    cp /etc/sysconfig/rhn/sources /etc/sysconfig/rhn/sources.$DATEVAR.$$
    echo "yum dries http://ftp.belnet.be/packages/dries.ulyssis.org/redhat/el$REDHAT_RELEASE/en/$PLATFORM_ARCH/dries" >>/etc/sysconfig/rhn/sources
else
    echo "You are not running RedHat"
    exit 1
fi

echo ""
echo "============================================================"
echo "Activating your computer for use on the UW RHN Satellite"
echo "servers."
echo "============================================================"
if /usr/sbin/rhnreg_ks --force --activationkey=$UW_ACTIVATION_KEY
then
    echo "System has been activated and configured for use at the University of Washington"
else
    echo "System configuration has failed.  Please contact your system administrator"
    exit $?
fi


# install required packages for the system
echo ""
echo "============================================================"
echo "Now attempting to connect to update servers to patch your"
echo "operating system and programs for critical bugs and security"
echo "fixes."
echo "============================================================"
if up2date -u -f
then
    echo "All updates have been applied to your computer from the up2date servers."
else
    echo "At least one update has failed.  Please contact your system administrator."
    echo "Error Code: $?"
    exit $?
fi

# install SPK related dependencies
echo ""
echo "============================================================"
echo "Installing 3rd Party Packages for use by SPK and developer"
echo "packages"
echo "============================================================"
if up2date -u -f $UPDATE_DEPENDENCIES $UPDATE_DEVELOPER_PACKAGES
then
    echo "All 3rd party dependencies for SPK have been installed or updated"
else
    echo "At least one update has failed.  Please contact your system administrator."
    echo "Error Code:  $?"
    exit $?
fi

# install SPK packages from our repository
echo ""
echo "============================================================"
echo "Installing locally maintained RPM packages from the RFPK"
echo "servers"
echo "============================================================"
TMP_DIR=/tmp/rfpk_software_updates.$$
mkdir -p $TMP_DIR
pushd $TMP_DIR
/usr/bin/wget -nd -l1 -r --accept "*.rpm" $RFPK_RPM_SERVER/rhel$REDHAT_RELEASE
if rpm -Uvh *.rpm
then
    echo "All locally maintained packages were installed or updated"
else
    ERROR_CODE=$?
    echo "At least one update has failed.  Please contact your system administrator."
    echo "Error Code: $ERROR_CODE"
    rm -rf $TMP_DIR
popd
rm -rf $TMP_DIR
fi


# write SPK specific configuration (e.g. CVS environment variables)
cp -fv /etc/profile /etc/profile.bak
sed -e 's|^HISTSIZE=\(.*\)$|HISTSIZE=\1\nCVSPATH=/usr/local/cvs_repos/rfpk/r2\nCVS_RSH=ssh\nCVSHOST=whitechuck.rfpk.washington.edu\nCVSROOT=:ext:$USER@$CVSHOST:$CVSPATH\nEDITOR=emacs|g' -e 's|^\(export PATH USER.*\)$|\1 CVSROOT CVS_RSH EDITOR|g' -e 's|^\(unset pathmunge\)$|\1\n\nkeychain ~/.ssh/id_dsa\n. ~/.keychain/${HOSTNAME}-sh\n|g' < /etc/profile.bak > /etc/profile

# configure sudoers file
cp -fv /etc/sudoers /etc/sudoers.bak
sed -e 's|^# \(%wheel.*ALL=(ALL)\tALL$\)|\1\n%staff\t\tALL=(ALL)\tALL\n|g' < /etc/sudoers.bak > /etc/sudoers

if /usr/sbin/groupadd -g 333 staff
then
    echo "Added group:  staff"
else
    echo "Group staff already exists"
    /usr/sbin/groupdel staff
    /usr/sbin/groupadd -g 333 staff
fi

# configure computer to use single sign on in the future
# kerberos is a good idea

# adding users to the system
for user in $VALID_STAFF_USERS;
  do
  if id $user
      then
      echo "User $user exists... continuing..."
      echo ""
  else
      useradd -c "User $user" -g staff -p ${DEFAULT_PASSWORD}_$user $user
  fi
done;

exit 0

