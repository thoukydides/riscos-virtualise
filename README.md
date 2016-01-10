# Virtualise #

:warning: **The author is no longer developing for RISC OS, so this code is provided as-is without support.** :warning:

The Virtualise system provides a limited form of virtual memory (VM) for RiscPC (26-bit ARMv3 and ARMv4 based computers running Acorn’s RISC OS) dynamic areas. It is composed of two parts: A module which actually provides the virtual memory, and a front-end which provides an easy to use interface to most of the features.

The main features are:
* Simple to use WIMP front-end application to control virtual memory.
* The memory usage can be controlled using the bar in the Task Manager.
* Any number of virtual dynamic areas can be created and managed.
* Can create virtual dynamic areas larger than the amount of physical memory.
* Any dynamic area can be converted to/from virtual memory at any time.
* Supports draggable virtual dynamic areas and those with handlers.
* Intercepts OS SWIs to minimize changes required to existing applications.
* Supports memory accesses from USR26 and SVC26 modes.
* Pages may be locked to allow access from interrupt routines.
* Parameters for file operations are automatically locked and unlocked.
* File accesses transparently split to allow operations larger than memory.
* Handles all processor instructions, including coprocessor data transfer.
* Does not affect other programs that use processor hardware vectors.
* Global page allocation policy across all virtual dynamic areas.
* Choice of page replacement policies: NFU with ageing, FIFO or random.
* Whole WIMP tasks can be suspended and swapped to disc.

It was originally launched at Acorn World 1995 and marketed commercially by Clares Micro Supplies. A [review](http://www.thoukydides.webspace.virginmedia.com/images/scan-virtualise.jpg) was published in the June 1996 issue of Acorn Archimedes World. In December 2002 the sales and marketing rights were acquired by APDL who included it on the [cover disc](http://www.apdl.org.uk/riscworld/volumes/volume8/issue2/virtual/) of Risc World magazine (volume 8, issue 2).

The rights to the Virtualise source code were retained by the author. The source code (excluding the copy protection code derived from source files provided by Clares) has now been released under the GNU General Public License (GPL).

These source files have been rearranged to make them more convenient to navigate and view on non RISC OS platforms. Most significantly, instead of the RISC OS convention of using separate directories for different source file types, more traditional file extensions have been used. BBC BASIC files have also been detokenised to plain text. The Makefile has not been updated to match these changes, so the code will not build without some modification.

***
<sup> Copyright 1995-1999, 2016