LorikeeM
========

LorikeeM MUMPS Developer Tools for Emacs

This major mode for emacs provides basic syntax highlighting for MUMPS.
It supports most features of Fidelity GT.M.

###License###

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License (AGPL)
as published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see http://www.gnu.org/licenses/.

###Global Lookup###

This major mode can also provide a dump of any GT.M global from within the
editor. For this to work, the KBAWDUMP.m and KBBMLRKM.m files (included 
in the distribution package) must be placed in one of the directories 
referenced by your $gtmroutines environment variable.

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

###Routine Jump###

This major mode provides the ability to jump from a routine
reference to its definition using a tagfile created by 
Exuberant CTAGS.

The script mktags, located in the bin/ directory of the 
distribution, allows you to create a tagfile in the unique
format required by Emacs.

To create the tagfile for all the source directories referenced
in your $gtmroutines environment variable, run mktags -eg from
your ~/bin directory. You will need to run this script each time 
you make a change to your MUMPS routines.

For more advanced usage of mktags, refer to mktags(1)

###Symbol Completion###

LorikeeM provides symbol and keyword completion. To use it, 
move your cursor to the keyword and press the F5 key or 
*M-x lkm-complete-symbol*

###Installation###

1) Create the directory ~/.emacs.d if it does not already exist.

2) Copy src/lorikeem/lorikeem.el to ~/.emacs.d

3) Add the following to your ~/.emacs file

    (add-to-list 'load-path "~/.emacs.d")
    (autoload 'mumps-mode "lorikeem" "MUMPS mode" t)
    (setq auto-mode-alist (append '(("\\.m$" . mumps-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\\.ewd$" . xml-mode)) auto-mode-alist))

4) Copy src/mumps/*.m to a directory defined in your $gtmroutines

5) Create the directory ~/bin

6) Copy bin/mktags to ~/bin (must have the execute permission bit set)

###Credits###

KBAWDUMP.m and mktags were written by DL Wicksell for his excellent 
Axiom tools for the vim editor. https://bitbucket.org/dlw/axiom
