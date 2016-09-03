## About qt-libs <a href="https://travis-ci.org/Shinmera/qt-libs"><img src="https://travis-ci.org/Shinmera/qt-libs.svg?branch=master" alt="Build Status" align="right" /></a>
Working with Qtools requires working with CommonQt. This in turn currently requires working with SmokeQt. However, SmokeQt is not easily available on all platforms (especially OS X). This project should simply ensure that the libraries are either available by your system directly, or that they are built automatically for you. This ensures that Qtools can be used directly with hopefully no further user intervention being necessary.

## The Details
There are two systems to be aware of: `qt-lib-generator` and `qt-libs`. The former is what's responsible for building and ensuring the external smoke libraries. The latter invokes the former if necessary, and stores the desired libraries in an easily accessible location, while also ensuring that CommonQt is tricked into using the proper paths when loading its libraries.

In effect this means that if you are running a project that depends on CommonQt (`:qt`), you should instead depend on `:qt-libs`, which will try to make sure that everything concerning foreign libraries will go smoothly. By default it uses prebuilt binaries that come from relatively recent, but not too recent systems. This should hopefully mean that all the dependant versions will be available on most systems out there. If that is not the case, you can try to build your own versions to ship.

In the future, CommonQt will use its own system to hook into Qt and drop Smoke support. When that time comes, this project will hopefully become obsolete. However, for the time being, Smoke is a necessary evil, and as such, this library hopes to make things a bit easier.

## Precompiled Packages
Included in the precompiled packages for each platform are unless otherwise noted:

* Qt 4.8.7
* Qwt 5
* QImageBlitz
* QScintilla 2
* Phonon
* Smoke
* SmokeQt
* libcommonqt

## Notes About the Platforms
### Windows

* You will need an x86-64 Windows copy, version 7 or higher. Lower versions might work, but I will not test them.
* You can run both x86 and x86-64 versions of your implementation, but not both at the same time.
* You do *not* need to install or download Qt itself. Qt-libs will download a precompiled version.
* Drakma requires OpenSSL to download the archives. See <https://slproweb.com/products/Win32OpenSSL.html>. This is only required for the first-time setup. After that, drakma will not be loaded and you do not need to ship the SSL dlls for things to work.
* QtTest is not available as it fails to build and despite hours being spent on it I cannot figure out how to fix it.

### Linux

* You will need an x86-64 Linux setup.
* You can run both x86 and x86-64 versions of your implementation, although precompiled versions only exist for x86-64.
* If you want to build the binaries yourself, you will need to install Qt. Preferably version 4.8.7 . The following packages should suffice for this:
  * Arch: `base-devel qt4 qwt qwt5 qimageblitz qscintilla-qt4 phonon-qt4`
  * Debian & Ubuntu: `gcc g++ cmake libqt4-dev libqwt5-qt4-dev libqimageblitz-dev libqscintilla2-dev libphonon-dev`

### Mac OS X

* You will need an x86-64 OS X setup, preferably Yosemite (10.10). Lower versions might work, but I will not test them.
* You can run both x86 and x86-64 versions of your implementation, although precompiled versions only exist for x86-64.
* If you want to build the binaries yourself, you will need to install Qt. Preferably version 4.8.7 . The following packages should suffice for this:
  * Xcode: Install its Command Line Tools
  * MacPorts: `cmake qt4-mac qwt qimageblitz qscintilla phonon`
  * Homebrew: `cmake qt4 qscintilla2` qimageblitz and qwt5 are not supported.

### Lisp
The following implementations have been tested to work. Others may as well, but are not officially supported.

* SBCL (1.2.12+)
* CCL (1.10+)
* ECL (16.1.2+)

## Cook It Up!
On Linux and OS X you can automatically compile new versions of the libraries (except for Qt) if you need to. For Windows, see [this article](https://blog.tymoon.eu/article/323). In order to do this, follow these steps:

    (ql:quickload :qt-libs)
    (qt-libs:ensure-standalone-libs :mode :install-sources :force T)

That should automatically download the necessary sources, copy the Qt binaries from your system, and compile the rest of the libraries.
