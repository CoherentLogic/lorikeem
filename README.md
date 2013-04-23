LorikeeM
========

LorikeeM MUMPS Developer Tools for GNU Emacs

This major mode for emacs provides basic syntax highlighting for [M]UMPS.
It supports most features of GT.M and InterSystems Cache.

This major mode can also provide a dump of any GT.M global from within the
editor. For this to work, the KBAWDUMP.m file (included in the package) must
be placed in one of the directories referenced in your $gtmroutines
environment variable.

In order to use the MUMPS Global Examiner, place the cursor over the
global you wish to examine, and perform one of the following actions:

* Press the F6 key on your keyboard 
* Type M-x lkm-global-at-point
* Select "Examine Global at Cursor" from the LorikeeM menu

To examine a global that is not in the current buffer, perform
one of the following actions:

* Press F7 on your keyboard
* Type M-x lkm-gtm-global-lookup
* Select "Examine Global" from the LorikeeM menu

NOTE: The global lookup feature essentially performs
a ZWR on the selected/typed global. In this release,
it dumps the _entire_ contents of the requested global.
This can be a huge time and memory drain, especially for
large globals, as found in e.g. VistA.

Lookup of a specific subscript is planned for a future
release.


INSTALLATION:
