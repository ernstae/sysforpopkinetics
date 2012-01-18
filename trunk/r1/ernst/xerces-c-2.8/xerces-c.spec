Summary:	Validating XML Parser
Name:		xerces-c
Version:	2.8.0
Release:	5%{?dist}
License:	ASL 2.0
Group:		System Environment/Libraries
URL:		http://xml.apache.org/xerces-c/
Source0:	http://www.apache.org/dist/xerces/c/sources/xerces-c-src_2_8_0.tar.gz
PAtch0:		xerces-c--CVE-2009-1885.diff
BuildRoot:	%{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

%description
Xerces-C is a validating XML parser written in a portable subset of
C++. Xerces-C makes it easy to give your application the ability to
read and write XML data. A shared library is provided for parsing,
generating, manipulating, and validating XML documents. Xerces-C is
faithful to the XML 1.0 recommendation and associated standards ( DOM
1.0, DOM 2.0. SAX 1.0, SAX 2.0, Namespaces).

%package	devel
Summary:	Header files, libraries and development documentation for %{name}
Group:		Development/Libraries
Requires:	%{name} = %{version}-%{release}

%description devel
This package contains the header files, static libraries and development
documentation for %{name}. If you like to develop programs using %{name},
you will need to install %{name}-devel.

%package doc
Group:		Documentation
Summary:	Documentation for Xerces-C++ validating XML parser

%description doc
Documentation for Xerces-C++.

Xerces-C++ is a validating XML parser written in a portable subset of C++.
Xerces-C++ makes it easy to give your application the ability to read and
write XML data. A shared library is provided for parsing, generating,
manipulating, and validating XML documents.

#%package	samples
#Summary:	Sample applications using Xerces-C++
#Group:		Applications/Text
#Requires:	%{name} = %{version}-devel-%{release}

#%description	samples
#Sample applications using Xerces-C++.


%prep
%setup -q -n xerces-c-src_2_8_0
rm -rf doc/html/resources/.svn
find ./doc -type f -perm 755 -exec chmod 644 {} \;
find ./samples -type f -perm 755 -exec chmod 644 {} \;
%{__perl} -pi.orig -e 's|(PREFIX.)/lib\b|$1/%{_lib}|g' src/xercesc/configure */Makefile.in
rm doc/html/apiDocs/XMLRegisterCleanup_8hpp__incl.map
rm doc/html/apiDocs/XSConstants_8hpp__incl.map
%patch0 -p0 -b .CVE-2009-1885

# make rpmlint happy
sed -i 's/\r//' doc/charter.xml
iconv -f iso8859-1 -t utf-8 credits.txt > credits.utf8 && mv -f credits.{utf8,txt}
iconv -f iso8859-1 -t utf-8 doc/feedback.xml > doc/feedback.utf8 && mv -f doc/feedback.{utf8,xml}
iconv -f iso8859-1 -t utf-8 doc/migration.xml > doc/migration.utf8 && mv -f doc/migration.{utf8,xml}
iconv -f iso8859-1 -t utf-8 doc/releases_archive.xml > doc/releases_archive.utf8 && mv -f doc/releases_archive.{utf8,xml}

%build
export XERCESCROOT="$PWD"

# Let Makefiles be verbose
find -name 'Makefile.*' | while read f; do
	sed -i -e 's/$Q//g' \
	-e 's/{MAKE} -s/(MAKE)/g' \
	-e '/echo \"  (/d' \
	$f
done

# Remove conflicting flags from runConfigure
find -name runConfigure | while read f; do
	sed -i -e 's/-w -O -DNDEBUG/-DNDEBUG/g' $f
done

cd $XERCESCROOT/src/xercesc
%ifarch alpha ppc64 s390x sparc64 x86_64
CXXFLAGS="${RPM_OPT_FLAGS}" CFLAGS="${RPM_OPT_FLAGS}" ./runConfigure -plinux -cgcc -xg++ -minmem -nsocket -tnative -rpthreads -b64 -P %{_prefix} -C --libdir="%{_libdir}"
%else
CXXFLAGS="${RPM_OPT_FLAGS}" CFLAGS="${RPM_OPT_FLAGS}" ./runConfigure -plinux -cgcc -xg++ -minmem -nsocket -tnative -rpthreads -b32 -P %{_prefix} -C --libdir="%{_libdir}"
%endif
# not smp safe
%{__make}
#cd $XERCESCROOT/samples
#CXXFLAGS="${RPM_OPT_FLAGS}" CFLAGS="${RPM_OPT_FLAGS}" ./runConfigure -plinux -cgcc -xg++
#%{__make}

%install
%{__rm} -rf $RPM_BUILD_ROOT
export XERCESCROOT="$PWD"
%{__make} install -C src/xercesc DESTDIR="$RPM_BUILD_ROOT"
#mkdir -p $RPM_BUILD_ROOT%{_datadir}/%{name}
#rm -rf $XERCESCROOT/samples/Projects
#cp -a $XERCESCROOT/samples $RPM_BUILD_ROOT%{_datadir}/%{name}

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%clean
%{__rm} -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%doc LICENSE.txt
%{_libdir}/libxerces*.so.*

%files devel
%defattr(-,root,root,-)
%{_libdir}/libxerces*.so
%{_includedir}/xercesc/

%files doc
%defattr(-,root,root,-)
%doc Readme.html LICENSE NOTICE STATUS credits.txt doc samples

#%files samples
#%defattr(-,root,root,-)
#%{_datadir}/%{name}/samples

%changelog
* Thu Aug  6 2009 Peter Lemenkov <lemenkov@gmail.com> 2.8.0-5
- Fix CVE-2009-1885

* Mon Jul 27 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.8.0-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Thu Feb 26 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.8.0-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_11_Mass_Rebuild

* Mon Jun 30 2008 Peter Lemenkov <lemenkov@gmail.com> 2.8.0-2
- Spec cleanups ( https://bugzilla.redhat.com/show_bug.cgi?id=435132 )

* Sun Feb 10 2008 Peter Lemenkov <lemenkov@gmail.com> 2.8.0-1
- Ver. 2.8.0

* Sat Nov 25 2006 Peter Lemenkov <lemenkov@gmail.com> 2.7.0-6
- typo fix

* Sat Nov 25 2006 Peter Lemenkov <lemenkov@gmail.com> 2.7.0-5
- fixed some rpmlint warnings

* Fri Nov 24 2006 Peter Lemenkov <lemenkov@gmail.com> 2.7.0-4
- Added samples to docs-package

* Sat Nov 18 2006 Peter Lemenkov <lemenkov@gmail.com> 2.7.0-3
- improvements suggested by Aurelien Bompard

* Sat Oct 14 2006 Peter Lemenkov <lemenkov@gmail.com> 2.7.0-2
- Disabled package 'samples'

* Fri Oct 13 2006 Peter Lemenkov <lemenkov@gmail.com> 2.7.0-1
- initial build for FE

* Fri Jan 06 2006 Dag Wieers <dag@wieers.com> - 2.7.0-1 - 3891/dag
- Cleaned SPEC file.

* Tue Jan 03 2006 Dries Verachtert <dries@ulyssis.org> - 2.7.0-1
- Updated to release 2.7.0.

* Thu Sep 22 2005 C.Lee Taylor <leet@leenx.co.za> 2.6.1-1
- Update to 2.6.1
- Build for FC4 32/64bit

* Sat Aug 20 2005 Che
- initial rpm release
