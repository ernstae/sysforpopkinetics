Name:           perl-IPC-Shareable
Version:        0.60
Release:        3%{?dist}
Summary:        Share Perl variables between processes

Group:          Development/Libraries
License:        GPL
URL:            http://search.cpan.org/dist/IPC-Shareable/
Source0:        http://www.cpan.org/authors/id/B/BS/BSUGARS/IPC-Shareable-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

BuildArch:      noarch
Requires:       perl(:MODULE_COMPAT_%(eval "`%{__perl} -V:version`"; echo $version))

%description
IPC::Shareable allows you to tie a variable to shared memory making it
easy to share the contents of that variable with other Perl processes.
Scalars, arrays, and hashes can be tied.  The variable being tied may
contain arbitrarily complex data structures - including references to
arrays, hashes of hashes, etc.


%prep
%setup -q -n IPC-Shareable-%{version}
find eg -type f | xargs chmod -c 644


%build
%{__perl} Makefile.PL INSTALLDIRS=vendor
make %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
make pure_install PERL_INSTALL_ROOT=$RPM_BUILD_ROOT
find $RPM_BUILD_ROOT -type f -name .packlist -exec rm -f {} ';'
find $RPM_BUILD_ROOT -type d -depth -exec rmdir {} 2>/dev/null ';'
chmod -R u+w $RPM_BUILD_ROOT/*


%check
make test


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc CHANGES COPYING CREDITS DISCLAIMER README TO_DO eg/
%{perl_vendorlib}/IPC/
%{_mandir}/man3/*.3pm*


%changelog
* Fri Sep  8 2006 Jose Pedro Oliveira <jpo at di.uminho.pt> - 0.60-3
- Rebuild for FC6.

* Mon Feb 20 2006 Jose Pedro Oliveira <jpo at di.uminho.pt> - 0.60-2
- Rebuild for FC5 (perl 5.8.8).

* Sat Oct 22 2005 Jose Pedro Oliveira <jpo at di.uminho.pt> - 0.60-1
- First build.
