Ada-Fuse
========

Ada-Fuse provides Ada bindings for Fuse. Our goal was to make it possible to
use the Fuse operations with Ada-like types and functions.

You can install it with [Alire](https://alire.ada.dev).

You can use most of the Fuse operations. The missing operations are `lock`,
`utimens`, `bmap`, `ioctl` and `poll`. For most file systems this should not be
a problem, in fact we never saw a Fuse-Filesystem using these operations. Be
aware that not all of the implemented functions are tested. Untested functions
are marked in the source.

Ada-Fuse should work on 32-bit and 64-bit Linux and macOS. See "Known
limitations/bugs" for more information.

Requirements
------------

*   FUSE (with dev files)
*   Gnat 2012
*   gprbuild

Package Layout
--------------
* `Fuse`: important types and constants
* `Fuse.System`: system specific declarations
* `Fuse.General`: general type and procedure declarations; depends on `Fuse.System`

* `Fuse.Main`: main, operations, wrapper; depends on `Fuse.General` and `Fuse.System`

* `Fuse.Aux`: procedures for fs implementation; is not needed by fuse lib

Getting started
---------------

Take a look at our [example file systems](examples). In a nutshell, you have to
instantiate `Fuse.Main`, write the functions you want to implement and register
them. Then you can call the main procedure, which mounts the filesystem. For a
list of functions take a look at [fuse-main.ads](src/fuse-main.ads). As most of
our documentation is only about Ada-Fuse but not Fuse in general, you should
also have a look into the [Fuse Wiki](http://sourceforge.net/apps/mediawiki/fuse)
and their [API reference](http://fuse.sourceforge.net/doxygen/index.html). Also
the corresponding pages in `man 2` are helpful.

Known limitations/bugs
------------------------

* We made no efforts to support multi threading. Some things might work but
  unless you know what you are doing you should always call your file system
  with the `-s` flag.

* Not much testing has been done yet. We tried to fix or at least document all
  the bugs we found, but there might still be major problems we didn't notice
  yet.

* Ada-Fuse was developed and tested under Linux x86_64. Most things should work
  on 32 bit Linux and Mac OS as well, but especially Fuse.Aux can be
  problematic, as Fuse uses many 64 bit types internally and importing C
  functions often doesn't work anymore.

* Fuse.Aux depends on some Ada and C functions that were probably not meant to
  be used that way. It should be seen as a quick way to get things running,
  however it is not yet suitable for a reliable file system.

* We cannot specify the Fuse version to use. We think that is why we get a
  warning when running a file system:

      fuse: warning: library too old, some operations may not not work

* When instantiating Fuse.Main we get warnings about not every byte being used.
  This is because of Fuse's data structures. Avoid these with

  ```ada
  pragma Warnings (Off, "* bits of * unused");
  package My_Fuse is new Fuse.Main (...);
  pragma Warnings (On, "* bits of * unused");
  ```

* There are functions that have wrappers implemented that have not been tested.
  There is a good chance they will not work the way they are. However they are
  marked and already registered everywhere, so fixing them should be easy.

* We have no implementations for `lock`, `utimens`, `bmap`, `ioctl` and `poll`.

TODO
----

* remaining representation clauses in Fuse.System

* `lock`, `utimens`, `bmap`, `ioctl` and `poll`

* test the untested functions, fix if necessary

* functions for Fuse.Aux, that will work on every OS

* a larger example filesystem would be nice


Useful Links
------------

* [Fuse Wiki](http://sourceforge.net/apps/mediawiki/fuse)
* [Fuse-API reference](http://fuse.sourceforge.net/doxygen/index.html)
* [Writing a FUSE Filesystem: a Tutorial by Joseph J. Pfeiffer, Jr.,
  Ph.D.](http://www.cs.nmsu.edu/~pfeiffer/fuse-tutorial/)

Copyright
---------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
