## About qt-libs <a href="https://travis-ci.org/Shinmera/qt-libs"><img src="https://travis-ci.org/Shinmera/qt-libs.svg?branch=master" alt="Build Status" align="right" /></a>
Working with Qtools requires working with CommonQt. This in turn currently requires working with SmokeQt. However, SmokeQt is not easily available on all platforms (especially OS X). This project should simply ensure that the libraries are either available by your system directly, or that they are built automatically for you. This ensures that Qtools can be used directly with hopefully no further user intervention being necessary.

## The Details
There are two systems to be aware of: `qt-lib-generator` and `qt-libs`. The former is what's responsible for building and ensuring the external smoke libraries. The latter invokes the former if necessary, and stores the desired libraries in an easily accessible location, while also ensuring that CommonQt is tricked into using the proper paths when loading its libraries.

In effect this means that if you are running a project that depends on CommonQt (`:qt`), you should instead depend on `:qt-libs`, which will try to make sure that everything concerning foreign libraries will go smoothly.

In the future, CommonQt will use its own system to hook into Qt and drop Smoke support. When that time comes, this project will hopefully become obsolete. However, for the time being, Smoke is a necessary evil, and as such, this library hopes to make things a bit easier.

The deed is done by providing a couple of ASDF extensions in the form of extra operations `download-op`, `generate-op`, and `install-op`, as well as extra system classes `build-system`, `make-build-system`, and `cmake-build-system`. Using these, systems for Qt4, smokegen, smokeqt, and libcommonqt are defined that will step through the downloading of source files, and the subsequent build and installation thereof. Do not worry, none of these will try to install anything to your system folders. No administration rights are required. Everything is stored in ASDF's cache folders.

The build system tries to invoke `make` with as many parallel jobs as supported. If this is for some reason not desired (eg running out of RAM), you can set `*max-cpus*` to a different value.

## What You Still Need
While qt-libs provides the foreign libraries for Smoke and CommonQt, you still need certain system tools to actually run everything. What that is, depends heavily on your particular system.

### Windows

* You will need a 64x Windows copy, version 7 or higher. Lower versions might work, but I will not test them.
* You can run both 32x and 64x versions of your implementation, but not both at the same time.
* You do *not* need to install or download Qt itself. Qt-libs will download a precompiled version.

### Linux

* You will need a 64x Linux setup.
* You can run both 32x and 64x versions of your implementation, although precompiled versions only exist for 64x.
* You will need to install Qt itself. Preferably version 4.8.7 . The following packages should suffice for this:
  * Arch: `qt4` `base-devel`
  * Debian & Ubuntu: `libqt4-dev` `gcc` `g++` `cmake`
  * Fedora: `qt-devel` `gcc` `gcc-c++` `cmake`
* If your implementation provides them, you may also use packages for the smokeqt libraries directly. Qt-libs *should* detect and use them instead of building from scratch.
  * Arch: `AUR/kdebindings-smokeqt`
  * Debian & Ubuntu: `libsmokeqt4-dev`
  * Fedora: `smokeqt-devel`

### Mac OS X

* You will need a 64x OS X setup, preferably Yosemite (10.10). Lower versions might work, but I will not test them.
* You can run both 32x and 64x versions of your implementation, although precompiled versions only exist for 64x.
* You will need to install Qt itself. Preferably version 4.8.7 .
  * Xcode: Install its Command Line Tools
  * MacPorts: `qt4-mac` `cmake`

### Lisp

* SBCL (1.2.12) and CCL (1.10) should work out of the box. Any other implementation and version is untested, but may work.
* If things get stuck or fail for weird reasons, the solution is often to
  1. Delete the `standalone` directory from the qt-libs system directory. See `qt-libs:*standalone-libs-dir*`.
  2. Delete your FASL cache. Qt-libs abuses ASDF to build, so C++ sources and results are cached there as well.

## Stop All The Buildin'
By default on linux and mac, qt-libs will rebuild the libraries from source. This can take a while, and is usually unnecessary if you didn't change anything about them. If you want it to stop building things anew when Quicklisp fetches you a new qt-libs, simply copy over the `standalone` folder from the old directory. Qt-libs will detect that the folder and its contents exist, avoiding rebuilding everything.

If for some reason you cannot at all build the libraries, you can tell qt-libs to download them instead:

    (ql:quickload :qt-lib-generator)
    (dolist (sys '(:smokegen :smokeqt :libcommonqt))
      (qt-lib-generator:install-system sys :source-type :compiled))
    (ql:quickload :qt-libs)

This is already done on Windows by default, as building the libraries requires a working Visual Studio --as well as several days of potential frustration and agony-- something which I do not believe is feasible to demand of potential users. Another exclusivity to Windows is that it downloads the Qt libraries as well. This is because there are no official x64 libraries to download for that platform. On any other platform, they should be easily available though, making this divergence unnecessary.
