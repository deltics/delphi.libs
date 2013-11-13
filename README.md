Deltics delphi.libs
===================

As the name of this repository suggests, here-in you will find a collection 
of libraries that contain a diverse array of useful functionality.

Some of these are entirely stand-alone libraries, but most have some dependency
on others.  `rtl` in particular is often required by the other libraries
as this library contains a great deal of small, highly re-usable classes and
functions etc.

Delphi.libs contains the Smoketest testing framework library.



Installation
------------

The only 'installation' required for using these libraries is that you add the 
includes folder `+inc` to your compiler search path either on a per project basis 
or in your IDE Library settings (recommended).

Options set in the test projects reference the following environment variables:

   $(bin)       - folder for resulting executable
   $(dcu)       - folder for DCU output
   $(deltics)   - identifies the "root" folder containing delphi.libs

                  $(deltics) = \path\to\folder
                                              \+inc
                                              \+tests
                                              \bonjour
                                              \rtl
                                              \smoketest
                                              .. etc

For IDE versions supporting configuration sets and multiple platforms, project 
options are set with output and DCU folders to platform and configuration 
specific locations (where appropriate) as follows:

  Version          Debug Builds         Release Builds
  -------------------------------------------------------------
  Delphi 7-2007    $(bin)\Win32
                   $(dcu)

  Delphi 2009-XE   $(bin)\Win32Debug    $(bin)\Win32Release
                   $(dcu)\debug         $(bin)\release

  Delphi XE2-      $(bin)\Win32Debug    $(bin)\Win32Release
                   $(bin)\Win64Debug    $(bin)\Win64Release
                   $(dcu)\Win32Debug    $(dcu)\Win32Release
                   $(dcu)\Win64Debug    $(dcu)\Win64Release


External Dependencies
---------------------

Test projects currently assume the use of the FastMM4 memory manager with
FastMM4 on the IDE search path.



Documentation
-------------

In addition to any README files which may be found in various locations in the
libs repository, additional documentation will be provided in the repository Wiki.



Support
-------

Email me at jsmith@deltics.co.nz or visit my blog at http://www.deltics.co.nz/blog