Summary: SXM (CXEMA) version 1.1
Name: sxm
Version: 1.1
Release: 1
Copyright: Sergei Egorov <esl@acm.org>
Group: Development/Languages
Source: sxm-1.1.tar.gz
URL: html://www.malgil.com/sxm/
BuildRoot: /var/tmp/%{name}-root
Packager: Andrew Pochinsky <avp@alum.mit.edu>
ExclusiveOS: linux

%description
SXM is a portable implementation of the Scheme Programming language.
It conforms to IEEE/ANSI standard of Scheme and supports all features
of the R5RS Report on Scheme. In addition, SXM supports numerous 
features of Chez Scheme v6.0 and SRFIs 0, 6, 8, 9, 11, and 16.

%prep
%setup -q

%build
make sxm sxi sxm.1 sxi.1 Formlist

%install
mkdir -p $RPM_BUILD_ROOT/usr/local/bin
mkdir -p $RPM_BUILD_ROOT/usr/local/man/man1
make install INSTALL_ROOT=$RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc Readme
%doc Copying
%doc History
%doc Formlist
%doc Todo
/usr/local/bin/sxm
/usr/local/bin/sxi
/usr/local/man/man1/sxm.1
/usr/local/man/man1/sxi.1

%post

%clean
rm -fr $RPM_BUILD_ROOT
