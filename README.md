About qt-libs <a href="https://travis-ci.org/Shinmera/qt-libs"><img src="https://travis-ci.org/Shinmera/qt-libs.svg?branch=master" alt="Build Status" align="right" /></a>
-------------
Working with Qtools requires working with CommonQt. This in turn currently requires working with SmokeQt. However, SmokeQt is not easily available on all platforms (especially OS X). This project should simply ensure that the libraries are either available by your system directly, or that they are built automatically for you. This ensures that Qtools can be used directly with hopefully no further user intervention being necessary.

The Details
-----------
There are two systems to be aware of: `qt-lib-generator` and `qt-libs`. The former is what's responsible for building and ensuring the external smoke libraries. The latter invokes the former if necessary, and stores the desired libraries in an easily accessible location, while also ensuring that CommonQt is tricked into using the proper paths when loading its libraries.

In effect this means that if you are running a project that depends on CommonQt (`:qt`), you should instead depend on `:qt-libs`, which will try to make sure that everything concerning foreign libraries will go smoothly.

In the future, CommonQt will use its own system to hook into Qt and drop Smoke support. When that time comes, this project will hopefully become obsolete. However, for the time being, Smoke is a necessary evil, and as such, this library hopes to make things a bit easier.

The deed is done by providing a couple of ASDF extensions in the form of extra operations `download-op`, `generate-op`, and `install-op`, as well as extra system classes `build-system`, `make-build-system`, and `cmake-build-system`. Using these, systems for Qt4, smokegen, smokeqt, and libcommonqt are defined that will step through the downloading of source files, and the subsequent build and installation thereof. Do not worry, none of these will try to install anything to your system folders. No administration rights are required. Everything is stored in ASDF's cache folders.

The build system tries to invoke `make` with as many parallel jobs as supported. If this is for some reason not desired (eg running out of RAM), you can set `*max-cpus*` to a different value.
