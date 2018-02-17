;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LorikeeM MUMPS Developer Tools
;;
;; Provides syntax highlighting and global dump for GNU Emacs
;;
;; Note that I have concentrated mostly on GT.M/YottaDB syntax here. the
;; list of keywords and functions comes from Edition 6.1 of 
;; Jon Diamond's "Standard M Pocket Guide." I have not left out
;; any InterSystems Cache-specific syntax, but have not tested
;; its use on any Cache-specific code.
;;
;; Written by John Willis
;; <jpw@coherent-logic.com>
;;
;; Copyright (C) 2010, 2012, 2013, 2017 Coherent Logic Development LLC
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License (AGPL)
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 

;; hooks for run before mode run
(defvar mumps-mode-hook nil)


;; keywords for syntax highlighting
(defvar mumps-keywords-ucase-abbrev
  '("B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "TC" "TRE" "TRO" "TS" "U" "V" "W" "X" "ZA" "ZB" "ZD" "ZH" "ZHO" "ZI" "ZK" "ZL" "ZN" "ZP" "ZQ" "ZR" "ZS" "ZSY" "ZTC" "ZT" "ZTS" "ZU" "ZW")
  "MUMPS uppercase abbreviated keywords")

(defvar mumps-keywords-lcase-abbrev
  '("b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "tc" "tre" "tro" "ts" "u" "v" "w" "x" "za" "zb" "zd" "zh" "zho" "zi" "zk" "zl" "zn" "zp" "zq" "zr" "zs" "zsy" "ztc" "zt" "zts" "zu" "zw")
  "MUMPS lowercase abbreviated keywords")

(defvar mumps-keywords-ucase-full
  '("BREAK" "CLOSE" "CONTINUE" "DO" "ELSE" "ELSEIF" "FOR" "GOTO" "HALT" "HANG" "IF" "JOB" "KILL" "LOCK" "MERGE" "NEW" "OPEN" "PRINT" "QUIT" "READ" "SET" "TCOMMIT" "TRESTART" "TROLLBACK" "TSTART" "USE" "VIEW" "WHILE" "WRITE" "XECUTE" "ZALLOCATE" "ZBREAK" "ZDEALLOCATE" "ZHANG" "ZHOROLOG" "ZINSERT" "ZKILL" "ZLOAD" "ZNSPACE" "ZPRINT" "ZQUIT" "ZREMOVE" "ZSAVE" "ZSYNC" "ZSYSTEM" "ZTCOMMIT" "ZTRAP" "ZTSTART" "ZUSE" "ZWITHDRAW" "ZWRITE" "ZZDUMP")
  "MUMPS uppercase full-length keywords")

(defvar mumps-keywords-lcase-full
  '("break" "close" "continue" "do" "else" "elseif" "for" "goto" "halt" "hang" "if" "job" "kill" "lock" "merge" "new" "open" "print" "quit" "read" "set" "tcommit" "trestart" "trollback" "tstart" "use" "view" "while" "write" "xecute" "zallocate" "zbreak" "zdeallocate" "zhang" "zhorolog" "zinsert" "zkill" "zload" "znspace" "zprint" "zquit" "zremove" "zsave" "zsync" "zsystem" "ztcommit" "ztrap" "ztstart" "zuse" "zwithdraw" "zwrite" "zzdump")
  "MUMPS lowercase full-length keywords")

(defvar mumps-functions-ucase-abbrev
  '("A" "C" "D" "E" "F" "FN" "G" "IN" "I" "J" "L" "LI" "LB" "LD" "LF" "LFS" "LG" "LL" "LS" "LTS" "NA" "N" "NUM" "O" "P" "Q" "QL" "QS" "R" "S" "ST" "T" "TR" "V" "ZBA" "ZBC" "ZBF" "ZBG" "ZBL" "ZBN" "ZB" "ZBSE" "ZBST" "ZBX" "ZCVT" "ZC" "ZD" "ZDH" "ZDT" "ZDTH" "ZDEV" "ZI" "ZO" "ZP" "ZSE" "ZSO" "ZT" "ZTH" "ZTL" "ZU")
  "MUMPS uppercase abbreviated functions")

(defvar mumps-functions-lcase-abbrev
  '("a" "c" "d" "e" "f" "fn" "g" "in" "i" "j" "l" "li" "lb" "ld" "lf" "lfs" "lg" "ll" "ls" "lts" "na" "n" "num" "o" "p" "q" "ql" "qs" "r" "s" "st" "t" "tr" "v" "zba" "zbc" "zbf" "zbg" "zbl" "zbn" "zb" "zbse" "zbst" "zbx" "zcvt" "zc" "zd" "zdh" "zdt" "zdth" "zdev" "zi" "zo" "zp" "zse" "zso" "zt" "zth" "ztl" "zu")
  "MUMPS lowercase abbreviated functions")

(defvar mumps-functions-ucase-full
  '("ASCII" "BIT" "BITCOUNT" "BITFIND" "BITLOGIC" "CASE" "CHAR" "DATA" "EXTRACT" "FACTOR" "FIND" "FNUMBER" "GET" "INCREMENT" "INUMBER" "ISOBJECT" "ISVALIDNUM" "JUSTIFY" "LENGTH" "LIST" "LISTBUILD" "LB" "LISTDATA" "LISTFIND" "LISTFROMSTRING" "LISTGET" "LISTLENGTH" "LISTNEXT" "LISTSAME" "LISTTOSTRING" "NAME" "NEXT" "NORMALIZE" "NUMBER" "ORDER" "PIECE" "QLENGTH" "QSUBSCRIPT" "QUERY" "RANDOM" "REVERSE" "SELECT" "SORTBEGIN" "SORTEND" "STACK" "SYSTEM" "TEXT" "TRANSLATE" "VIEW" "ZABS" "ZARCCOS" "ZARCSIN" "ZARCTAN" "ZBAND" "ZBCOUNT" "ZBFIND" "ZBGET" "ZBIT" "ZZBITAND" "ZBITCOUNT" "ZBITFIND" "ZBITGET" "ZBITLEN" "ZBITNOT" "ZBITOR" "ZBITSET" "ZBITSTR" "ZBITXOR" "ZBLEN" "ZBNOT" "ZBOOLEAN" "ZBOR" "ZBSET" "ZBSTR" "ZBXOR" "ZCONVERT" "ZCVT" "ZCOS" "ZCOT" "ZCRC" "ZCSC" "ZCYC" "ZDATE" "ZDATEH" "ZDATETIME" "ZDATETIMEH" "ZDEVICE" "ZEXP" "ZF" "ZHEX" "ZINCREMENT" "ZINFO" "ZLN" "ZLOG" "ZMESSAGE" "ZNAME" "ZNEXT" "ZOBJCLASSMETHOD" "ZOBJPROPERTY" "ZORDER" "ZPARSE" "ZPOWER" "ZPREVIOUS" "ZSEARCH" "ZSEC" "ZSEEK" "ZSIN" "ZSOCKET" "ZSORT" "ZSQR" "ZSTRIP" "ZTAN" "ZTEXP" "ZTIME" "ZTIMEH" "ZTLOG" "ZTRNLMN" "ZUCI")
  "MUMPS uppercase full-length functions")

(defvar mumps-functions-lcase-full
    '("ascii" "bit" "bitcount" "bitfind" "bitlogic" "case" "char" "data" "extract" "factor" "find" "fnumber" "get" "increment" "inumber" "isobject" "isvalidnum" "justify" "length" "list" "listbuild" "lb" "listdata" "listfind" "listfromstring" "listget" "listlength" "listnext" "listsame" "listtostring" "name" "next" "normalize" "number" "order" "piece" "qlength" "qsubscript" "query" "random" "reverse" "select" "sortbegin" "sortend" "stack" "system" "text" "translate" "view" "zabs" "zarccos" "zarcsin" "zarctan" "zband" "zbcount" "zbfind" "zbget" "zbit" "zzbitand" "zbitcount" "zbitfind" "zbitget" "zbitlen" "zbitnot" "zbitor" "zbitset" "zbitstr" "zbitxor" "zblen" "zbnot" "zboolean" "zbor" "zbset" "zbstr" "zbxor" "zconvert" "zcvt" "zcos" "zcot" "zcrc" "zcsc" "zcyc" "zdate" "zdateh" "zdatetime" "zdatetimeh" "zdevice" "zexp" "zf" "zhex" "zincrement" "zinfo" "zln" "zlog" "zmessage" "zname" "znext" "zobjclassmethod" "zobjproperty" "zorder" "zparse" "zpower" "zprevious" "zsearch" "zsec" "zseek" "zsin" "zsocket" "zsort" "zsqr" "zstrip" "ztan" "ztexp" "ztime" "ztimeh" "ztlog" "ztrnlmn" "zuci")
    "MUMPS lowercase full-length functions")

(defvar mumps-sv-ucase-abbrev
  '("D" "EC" "ES" "ET" "H" "IO" "J" "K" "P" "Q" "R" "ST" "S" "SY" "T" "TL" "TR" "X" "Y" "ZA" "ZAL" "ZB" "ZCH" "ZCM" "ZCO" "ZCS" "ZCL" "ZDA" "ZD" "ZED" "ZEO" "ZE" "ZG" "ZH" "ZINT" "ZINI" "ZIO" "ZJ" "ZK" "ZL" "ZMAXPTI" "ZMO" "ZONLN" "ZPATN" "ZPIN" "ZPOUT" "ZPOS" "ZPROM" "ZRE" "ZRO" "ZSO" "ZS" "ZST" "ZSY" "ZTE" "ZUS" "ZUT" "ZV" "ZYER" "ZTDA" "ZTDE" "ZTL" "ZTNAME" "ZTOL" "ZTRI" "ZTS" "ZTUP" "ZTVA" "ZTWO" "ZYRE")
  "MUMPS uppercase abbreviated instrinsic SVs")

(defvar mumps-sv-lcase-abbrev
  '("d" "ec" "es" "et" "h" "io" "j" "k" "p" "q" "r" "st" "s" "sy" "t" "tl" "tr" "x" "y" "za" "zal" "zb" "zch" "zcm" "zco" "zcs" "zcl" "zda" "zd" "zed" "zeo" "ze" "zg" "zh" "zint" "zini" "zio" "zj" "zk" "zl" "zmaxpti" "zmo" "zonln" "zpatn" "zpin" "zpout" "zpos" "zprom" "zre" "zro" "zso" "zs" "zst" "zsy" "zte" "zus" "zut" "zv" "zyer" "ztda" "ztde" "ztl" "ztname" "ztol" "ztri" "zts" "ztup" "ztva" "ztwo" "zyre")
  "MUMPS lowercase abbreviated instrinsic SVs")

(defvar mumps-sv-ucase-full
  '("DEVICE" "ECODE" "ESTACK" "ETRAP" "HOROLOG" "IO" "JOB" "KEY" "PRINCIPAL" "QUIT" "REFERENCE" "STACK" "STORAGE" "SYSTEM" "TEST" "TLEVEL" "TRESTART" "X" "Y" "ZA" "ZALLOCSTOR" "ZB" "ZCHSET" "ZCMDLINE" "ZCOMPILE" "ZCSTATUS" "ZCLOSE" "ZDATEFORM" "ZDIRECTORY" "ZEDIT" "ZEOF" "ZERROR" "ZGBLDIR" "ZHOROLOG" "ZINTERRUPT" "ZININTERRUPT" "ZIO" "ZJOB" "ZKEY" "ZLEVEL" "ZMAXPTIME" "ZMODE" "ZONLNRBLK" "ZPATNUMERIC" "ZPIN" "ZPOUT" "ZPOSITION" "ZPROMPT" "ZREALSTOR" "ZROUTINES" "ZSOURCE" "ZSTATUS" "ZSTEP" "ZSYSTEM" "ZTEXIT" "ZUSEDSTOR" "ZUT" "ZVERSION" "ZYERROR" "ZTDATA" "ZTDELIM" "ZTLEVEL" "ZTNAME" "ZTOLDVAL" "ZTRIGGEROP" "ZTSLATE" "ZTUPDATE" "ZTVALUE" "ZTWORMHOLE" "ZYRELEASE")
  "MUMPS uppercase full-length intrinsic SVs")

(defvar mumps-sv-lcase-full
  '("device" "ecode" "estack" "etrap" "horolog" "io" "job" "key" "principal" "quit" "reference" "stack" "storage" "system" "test" "tlevel" "trestart" "x" "y" "za" "zallocstor" "zb" "zchset" "zcmdline" "zcompile" "zcstatus" "zclose" "zdateform" "zdirectory" "zedit" "zeof" "zerror" "zgbldir" "zhorolog" "zinterrupt" "zininterrupt" "zio" "zjob" "zkey" "zlevel" "zmaxptime" "zmode" "zonlnrblk" "zpatnumeric" "zpin" "zpout" "zposition" "zprompt" "zrealstor" "zroutines" "zsource" "zstatus" "zstep" "zsystem" "ztexit" "zusedstor" "zut" "zversion" "zyerror" "ztdata" "ztdelim" "ztlevel" "ztname" "ztoldval" "ztriggerop" "ztslate" "ztupdate" "ztvalue" "ztwormhole" "zyrelease")
  "MUMPS lowercase full-length intrinsic SVs")

;; define keywords for completion
(defvar mumps-keywords
  '("BREAK" "CLOSE" "CONTINUE" "DO" "ELSE" "ELSEIF" "FOR" "GOTO" "HALT" "HANG" "IF" "JOB" "KILL" "LOCK" "MERGE" "NEW" "OPEN" "PRINT" "QUIT" "READ" "SET" "TCOMMIT" "TRESTART" "TROLLBACK" "TSTART" "USE" "VIEW" "WHILE" "WRITE" "XECUTE" "ZALLOCATE" "ZBREAK" "ZDEALLOCATE" "ZHANG" "ZHOROLOG" "ZINSERT" "ZKILL" "ZLOAD" "ZNSPACE" "ZPRINT" "ZQUIT" "ZREMOVE" "ZSAVE" "ZSYNC" "ZSYSTEM" "ZTCOMMIT" "ZTRAP" "ZTSTART" "ZUSE" "ZWITHDRAW" "ZWRITE" "ZZDUMP" "break" "close" "continue" "do" "else" "elseif" "for" "goto" "halt" "hang" "if" "job" "kill" "lock" "merge" "new" "open" "print" "quit" "read" "set" "tcommit" "trestart" "trollback" "tstart" "use" "view" "while" "write" "xecute" "zallocate" "zbreak" "zdeallocate" "zhang" "zhorolog" "zinsert" "zkill" "zload" "znspace" "zprint" "zquit" "zremove" "zsave" "zsync" "zsystem" "ztcommit" "ztrap" "ztstart" "zuse" "zwithdraw" "zwrite" "zzdump" "ASCII" "BIT" "BITCOUNT" "BITFIND" "BITLOGIC" "CASE" "CHAR" "DATA" "EXTRACT" "FACTOR" "FIND" "FNUMBER" "GET" "INCREMENT" "INUMBER" "ISOBJECT" "ISVALIDNUM" "JUSTIFY" "LENGTH" "LIST" "LISTBUILD" "LB" "LISTDATA" "LISTFIND" "LISTFROMSTRING" "LISTGET" "LISTLENGTH" "LISTNEXT" "LISTSAME" "LISTTOSTRING" "NAME" "NEXT" "NORMALIZE" "NUMBER" "ORDER" "PIECE" "QLENGTH" "QSUBSCRIPT" "QUERY" "RANDOM" "REVERSE" "SELECT" "SORTBEGIN" "SORTEND" "STACK" "SYSTEM" "TEXT" "TRANSLATE" "VIEW" "ZABS" "ZARCCOS" "ZARCSIN" "ZARCTAN" "ZBAND" "ZBCOUNT" "ZBFIND" "ZBGET" "ZBIT" "ZZBITAND" "ZBITCOUNT" "ZBITFIND" "ZBITGET" "ZBITLEN" "ZBITNOT" "ZBITOR" "ZBITSET" "ZBITSTR" "ZBITXOR" "ZBLEN" "ZBNOT" "ZBOOLEAN" "ZBOR" "ZBSET" "ZBSTR" "ZBXOR" "ZCONVERT" "ZCVT" "ZCOS" "ZCOT" "ZCRC" "ZCSC" "ZCYC" "ZDATE" "ZDATEH" "ZDATETIME" "ZDATETIMEH" "ZDEVICE" "ZEXP" "ZF" "ZHEX" "ZINCREMENT" "ZINFO" "ZLN" "ZLOG" "ZMESSAGE" "ZNAME" "ZNEXT" "ZOBJCLASSMETHOD" "ZOBJPROPERTY" "ZORDER" "ZPARSE" "ZPOWER" "ZPREVIOUS" "ZSEARCH" "ZSEC" "ZSEEK" "ZSIN" "ZSOCKET" "ZSORT" "ZSQR" "ZSTRIP" "ZTAN" "ZTEXP" "ZTIME" "ZTIMEH" "ZTLOG" "ZTRNLMN" "ZUCI" "ascii" "bit" "bitcount" "bitfind" "bitlogic" "case" "char" "data" "extract" "factor" "find" "fnumber" "get" "increment" "inumber" "isobject" "isvalidnum" "justify" "length" "list" "listbuild" "lb" "listdata" "listfind" "listfromstring" "listget" "listlength" "listnext" "listsame" "listtostring" "name" "next" "normalize" "number" "order" "piece" "qlength" "qsubscript" "query" "random" "reverse" "select" "sortbegin" "sortend" "stack" "system" "text" "translate" "view" "zabs" "zarccos" "zarcsin" "zarctan" "zband" "zbcount" "zbfind" "zbget" "zbit" "zzbitand" "zbitcount" "zbitfind" "zbitget" "zbitlen" "zbitnot" "zbitor" "zbitset" "zbitstr" "zbitxor" "zblen" "zbnot" "zboolean" "zbor" "zbset" "zbstr" "zbxor" "zconvert" "zcvt" "zcos" "zcot" "zcrc" "zcsc" "zcyc" "zdate" "zdateh" "zdatetime" "zdatetimeh" "zdevice" "zexp" "zf" "zhex" "zincrement" "zinfo" "zln" "zlog" "zmessage" "zname" "znext" "zobjclassmethod" "zobjproperty" "zorder" "zparse" "zpower" "zprevious" "zsearch" "zsec" "zseek" "zsin" "zsocket" "zsort" "zsqr" "zstrip" "ztan" "ztexp" "ztime" "ztimeh" "ztlog" "ztrnlmn" "zuci")
  "MUMPS keywords for completion")

;; build the regexps from the lists
(setq mumps-line-label "^[%A-Za-z][A-Za-z0-9]*:?\\|^[0-9]+:?")
(setq mumps-string-error "\\\\\".*$")
(setq mumps-unmatched-open-paren "\(.*$")
(setq mumps-unmatched-close-paren "\).*$")
(defvar mkuf (regexp-opt mumps-keywords-ucase-full 'words))
(defvar mklf (regexp-opt mumps-keywords-lcase-full 'words))
(defvar mkua (regexp-opt mumps-keywords-ucase-abbrev 'words))
(defvar mkla (regexp-opt mumps-keywords-lcase-abbrev 'words))
(defvar mfuf (regexp-opt mumps-functions-ucase-full 'words))
(defvar mflf (regexp-opt mumps-functions-lcase-full 'words))
(defvar mfua (regexp-opt mumps-functions-ucase-abbrev 'words))
(defvar mfla (regexp-opt mumps-functions-lcase-abbrev 'words))
(defvar msua (regexp-opt mumps-sv-ucase-abbrev 'words))
(defvar msla (regexp-opt mumps-sv-lcase-abbrev 'words))
(defvar msuf (regexp-opt mumps-sv-ucase-full 'words))
(defvar mslf (regexp-opt mumps-sv-lcase-full 'words))

;; clear un-needed memory resources
(setq mumps-keywords-ucase-abbrev nil)
(setq mumps-keywords-ucase-full nil)
(setq mumps-keywords-lcase-abbrev nil)
(setq mumps-keywords-lcase-full nil)
(setq mumps-functions-lcase-full nil)
(setq mumps-functions-ucase-full nil)
(setq mumps-functions-ucase-abbrev nil)
(setq mumps-functions-lcase-abbrev nil)

(setq font-lock-warning-face "red2")

;; create the thingy that we'll feed to font-lock-defaults
(setq mumps-font-lock-keywords 
      `(      
	(,";.*$" . font-lock-comment-face)       
	(,mkua . font-lock-keyword-face)
	(,mkuf . font-lock-keyword-face)
	(,mkla . font-lock-keyword-face)
	(,mklf . font-lock-keyword-face)
        (,msua . font-lock-keyword-face)
        (,msla . font-lock-keyword-face)
        (,msuf . font-lock-keyword-face)
        (,mslf . font-lock-keyword-face)
	(,mfuf . font-lock-function-name-face)
	(,mfua . font-lock-function-name-face)
	(,mfla . font-lock-function-name-face)
	(,mflf . font-lock-function-name-face)	
	(,mumps-line-label . font-lock-type-face)
	(,mumps-string-error . font-lock-warning-face)
))

(defun lkm-about ()
  "About LorikeeM"
  (interactive)
  
  (setq lkm-about-buf "*About LorikeeM MUMPS Developer Tools*")
  (get-buffer-create lkm-about-buf)
  (with-current-buffer (get-buffer-create lkm-about-buf)
    (goto-char (point-max))
    (insert "LorikeeM MUMPS Developer Tools\n\n"))
  (switch-to-buffer-other-window lkm-about-buf))

(defun lkm-gtm-compile ()
  "Compiles the current buffer"
  (interactive)

  (setq file-name (buffer-file-name))
  (setq cmd-str (concat "mumps " file-name))
  (compile cmd-str nil))

(defun lkm-global-at-point ()
 "Look up the GT.M/YottaDB global at the word under the current point."

  (interactive)
  (let (mm-current-word)
    (setq current-char (char-to-string (char-after)))
    (if (string= current-char "^") (forward-char 1))
    (setq mm-current-word (thing-at-point 'word))
    (save-excursion
      (setq prev-char (char-to-string (char-before)))
      (if (not (string= prev-char "^")) (backward-word 1))
      (setq prev-char (char-to-string (char-before)))
      (if (string= prev-char "^") 
	(lkm-gtm-global-lookup mm-current-word) 
	(message "%s does not appear to be a MUMPS global." mm-current-word)))))

(defun lkm-is-line-label ()
  "Returns t if the current line is a label" 
  (setq current-line (thing-at-point 'line))
  (string-match "^[%A-Za-z][A-Za-z0-9]*:?\\|^[0-9]+:?" current-line))

(defun lkm-validate-line-label ()
  "Tells you whether or not the current line contains a valid line label"
  (interactive)
  (if (lkm-is-line-label)
      (message "This is a line label"))
  (if (not (lkm-is-line-label)) 
      (message "This is not a line label")))

(defun lkm-parent-label-distance ()
  "Returns the distance from the parent label in number of lines"
  (setq lkm-distance 0)
  (save-excursion
    (catch 'top-of-file
    (while (not (lkm-is-line-label))
      (if (= (line-number-at-pos) 1)
	  (throw 'top-of-file lkm-distance))
      (forward-line -1)
      (incf lkm-distance))))
  (setq lkm-distance lkm-distance))

(defun lkm-print-parent-label-distance ()
  "Print the distance from the parent label"
  (interactive)
  (message "The distance from the parent label is %d lines" (lkm-parent-label-distance)))

(defun lkm-parent-label-name ()
  "Returns the name of the parent label"
  (save-excursion
    (forward-line (- (lkm-parent-label-distance)))
    (setq label-line (thing-at-point 'line)))
  (string-match "^[%A-Za-z][A-Za-z0-9]*:?\\|^[0-9]+:?" label-line)
  (setq label-line (substring label-line 0 (match-end 0)))
  (replace-regexp-in-string "%" "%%" label-line))

(defun lkm-print-parent-label-name ()
  "Print the parent label name"
  (interactive)
  (message "The parent label is '%s'" (lkm-parent-label-name)))

(defun lkm-current-routine ()
  "Get the current routine" 
  (setq pl-routine (file-name-sans-extension (buffer-name)))
  (setq pl-routine (replace-regexp-in-string "_" "%%" pl-routine)))

(defun lkm-print-current-routine ()
  "Print the current routine"
  (interactive)
  (message "The current routine is '%s'" (lkm-current-routine)))

(defun lkm-current-label-offset-routine ()
  "Get the current label+offset^routine"
  (setq pl-label (lkm-parent-label-name))
  (setq pl-offset (lkm-parent-label-distance))
  (setq pl-routine (lkm-current-routine))
  (if (not (= pl-offset 0))
      (setq pl-result (format "%s+%d^%s" pl-label pl-offset pl-routine)))
  (if (= pl-offset 0)
      (setq pl-result (format "%s^%s" pl-label pl-routine)))
  (setq pl-result pl-result))

(defun lkm-print-current-label-offset-routine ()
  "Print the current label+offset^routine"
  (interactive)
  (message (lkm-current-label-offset-routine)))

(defun lkm-gtm-global-lookup (global)
  "Look up a global in GT.M/YottaDB"
  (interactive "sWhat global would you like to look up? ")
  
  (get-buffer-create "*MUMPS Global Examiner*")
  (with-current-buffer (get-buffer-create "*MUMPS Global Examiner*")
    (goto-char (point-max))
    (insert "MUMPS Global Dump of " global "\n\n"))
  (call-process "mumps" nil "*MUMPS Global Examiner*" nil "-r" "KBAWDUMP" global)
  (display-buffer (get-buffer "*MUMPS Global Examiner*") t)
  (with-current-buffer (get-buffer-create "*MUMPS Global Examiner*")
    (goto-char (point-max))))

(defun lkm-jump-to-routine-def ()
  "Jump to the definition of the routine under the cursor."
  (interactive)
  (setq lkm-current-symb (thing-at-point 'symbol))
  (find-tag-other-window lkm-current-symb))

(defun yas/insert-by-name (name)
  (flet ((dummy-prompt
          (prompt choices &optional display-fn)
          (declare (ignore prompt))
          (or (find name choices :key display-fn :test #'string=)
              (throw 'notfound nil))))
    (let ((yas/prompt-functions '(dummy-prompt)))
      (catch 'notfound
        (yas/insert-snippet t)))))

(defun lkm-routine-template ()
  "Get the template of the routine under the cursor."
  (interactive)
  (setq lkm-current-symb (thing-at-point 'symbol))  
  (insert "()")
  (backward-char 1)
  (yas/insert-by-name lkm-current-symb))

(defun lkm-expand-quote ()
  "Set up a string literal."
  (interactive)
  (insert "\"\"")
  (backward-char 1))

(defun lkm-sac-header () 
  "Insert the standard VistA SAC header"
  (interactive)
  (goto-char (point-min))
  (yas/insert-by-name "SACHEADER"))

(defun lkm-complete-symbol ()
  "Perform keyword completion on word before cursor."
  (interactive)
  (let ((posEnd (point))
        (meat (thing-at-point 'symbol))
        maxMatchResult)

    ;; when nil, set it to empty string, so user can see all lang's keywords.
    ;; if not done, try-completion on nil result lisp error.
    (when (not meat) (setq meat ""))
    (setq maxMatchResult (try-completion meat mumps-keywords))

    (cond ((eq maxMatchResult t))
          ((null maxMatchResult)
           (message "Can't find completion for “%s”" meat)
           (ding))
          ((not (string= meat maxMatchResult))
           (delete-region (- posEnd (length meat)) posEnd)
           (insert maxMatchResult))
          (t (message "Making completion list...")
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list 
                (all-completions meat mumps-keywords)
                meat))
             (message "Making completion list...%s" "done")))))


(defun lkm-start-debugger ()
  "Begins the default GT.M/YottaDB child process for debugging" 
  (interactive)
  (setq lkm-debug-window (split-window nil 20))
  (get-buffer-create "Debug")
  (set-window-buffer lkm-debug-window "Debug")
  (setq buffer-saved (current-buffer))
  (set-buffer "Debug")
  (setq buffer-read-only t)
  (set-buffer buffer-saved)
  (setq gtm-dist (getenv "gtm_dist"))
  (setq mcmd (format "%s%s" gtm-dist "/mumps"))
  (start-process "debugger" "Debug" mcmd "-r" "^KBBMDBUG"))   

(defun lkm-stop-debugger ()
  "Stops the debugger"
  (interactive)
  (setq debug-string "HALT\n")
  (process-send-string "debugger" debug-string))

(defun lkm-interrupt-debugger ()
  "Interrupts the running process"
  (interactive)
  (interrupt-process "debugger"))

(defun lkm-resume-debugger ()
  (interactive)
  (setq debug-string "ZCONTINUE\n")
  (process-send-string "debugger" debug-string))

(defun lkm-debug-routine ()
  "Begins debug at current routine"
  (interactive)
  (setq debug-string (format "DO %s^%s\n" (lkm-parent-label-name) (lkm-current-routine)))
  (process-send-string "debugger" debug-string))

(defun lkm-set-breakpoint ()
  "Sets a breakpoint at the current line"
  (interactive)
  (setq debug-string (format "ZBREAK %s\n" (lkm-current-label-offset-routine)))
  (process-send-string "debugger" debug-string))

(defun lkm-clear-breakpoint ()
  "Clears a breakpoint at the current line"
  (interactive)
  (setq debug-string (format "ZBREAK -%s\n" (lkm-current-label-offset-routine)))
  (process-send-string "debugger" debug-string))
 
(defun lkm-clear-all-breakpoints ()
  "Clears all breakpoints"
  (interactive)
  (setq debug-string (format "ZBREAK -*\n" (lkm-current-label-offset-routine)))
  (process-send-string "debugger" debug-string))

(defun lkm-list-breakpoints ()
  "Lists all breakpoints"
  (interactive)
  (setq debug-string "ZSHOW \"B\"\n")
  (process-send-string "debugger" debug-string))

(defun lkm-step-into ()
  "Executes ZSTEP INTO"
  (interactive)
  (setq debug-string "ZSTEP INTO\n")
  (process-send-string "debugger" debug-string))

(defun lkm-step-over ()
  "Executes ZSTEP OVER"
  (interactive)
  (setq debug-string "ZSTEP OVER\n")
  (process-send-string "debugger" debug-string))

(defun lkm-step-out ()
  "Executes ZSTEP OUT"
  (interactive)
  (setq debug-string "ZSTEP OUT\n")
  (process-send-string "debugger" debug-string))

(defun lkm-immediate (value)
  "Executes a line of MUMPS code or sends input to a pending MUMPS READ"
  (interactive "sEnter MUMPS command or input for pending READ: ")
  (setq debug-string (format "%s\n" value))
  (process-send-string "debugger" debug-string))

(define-derived-mode mumps-mode fundamental-mode
  "mumps mode"
  "LorikeeM MUMPS Developer Tools"

  (setq lkm-version "0.99.8")
  (message "LorikeeM MUMPS Developer Tools %s" lkm-version)

  (add-hook 'shell-mode-hook
	    'ansi-color-for-comint-mode-on)

  (setq frame-title-format "LorikeeM MUMPS Developer Tools")

  (set-variable 
   'imenu-generic-expression 
   (list
    (list
     nil 
     "\\(^[%A-Za-z][A-Za-z0-9]*\\):?\\|^[0-9]+:?" 1)))
  (imenu-add-to-menubar "Labels")

  (setq mode-line-format
	(list
	 "   "	 
	 '(:eval (propertize (lkm-current-label-offset-routine) 'face 'font-lock-string-face
			     'help-echo "Help"))
	 "   "

	 ;; relative position, size of file
	 "["
	 (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
	 "/"
	 (propertize "%I" 'face 'font-lock-constant-face) ;; size
	 "] ["
	 (propertize "Line %l Col %c" 'face 'font-lock-constant-face)
	 "] "	 

	 "[" ;; insert vs overwrite mode, input-method in a tooltip
	 '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
			     'face 'font-lock-preprocessor-face
			     'help-echo (concat "Buffer is in "
						(if overwrite-mode "overwrite" "insert") " mode")))

	 ;; was this buffer modified since the last save?
	 '(:eval (when (buffer-modified-p)
		   (concat ","  (propertize "Mod"
					    'face 'font-lock-warning-face
					    'help-echo "Buffer has been modified"))))

	 ;; is this buffer read-only?
	 '(:eval (when buffer-read-only
		   (concat ","  (propertize "RO"
					    'face 'font-lock-type-face
					    'help-echo "Buffer is read-only"))))  
	 "] "

	 ))

  ;;
  ;; set up syntax table entries
  ;;
  
  ;; syntax table entries for symbol constituents
  ;;  we're adding ^ and % to accommodate MUMPS routine calls
  (modify-syntax-entry ?^ "_")
  (modify-syntax-entry ?% "_")
  (modify-syntax-entry ?$ "-")
  (modify-syntax-entry ?\; "<")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?+ ".")
  (modify-syntax-entry ?- ".")
  (modify-syntax-entry ?_ ".")
  (modify-syntax-entry ?[ ".")
  (modify-syntax-entry ?* ".")
  (modify-syntax-entry ?/ ".")
  (modify-syntax-entry ?\\ ".")
  (modify-syntax-entry ?# ".")
  (modify-syntax-entry ?< ".")
  (modify-syntax-entry ?> ".")
  (modify-syntax-entry ?& ".")
  (modify-syntax-entry ?? ".")

  ;;
  ;; modify the tags table list to point to our directory
  ;;
  (setq tags-file-name "~/.MTAGS")

  ;;
  ;; set up keyboard mappings
  ;;
  (global-set-key (kbd "<f6>") 'lkm-global-at-point)
  (global-set-key (kbd "<f7>") 'lkm-gtm-global-lookup)
  (global-set-key (kbd "<f8>") 'lkm-jump-to-routine-def)
  (global-set-key (kbd "<f5>") 'lkm-debug-routine)
  (global-set-key (kbd "<f9>") 'lkm-gtm-compile)
  (global-set-key (kbd "(") 'lkm-routine-template)
  (global-set-key (kbd "\"") 'lkm-expand-quote)

  ;;
  ;; set up the LorikeeM menus to be loaded after the Tools menu
  ;;
  (define-key-after
    global-map
    [menu-bar vista-menu]
    (cons "VistA" (make-sparse-keymap "vista"))
    'tools )

  (define-key-after
    global-map
    [menu-bar globals-menu]
    (cons "Globals" (make-sparse-keymap "globals"))
    'tools )

  (define-key-after
    global-map
    [menu-bar routines-menu]
    (cons "Routines" (make-sparse-keymap "routines"))
    'tools )  

  (define-key-after
    global-map
    [menu-bar debug-menu]
    (cons "Debug" (make-sparse-keymap "debug"))
    'tools )

  (define-key-after
    global-map
    [menu-bar mumps-menu]
    (cons "MUMPS" (make-sparse-keymap "mumps"))
    'tools )

  (define-key
    global-map
    [menu-bar debug-menu send-input]
    '("Immediate" . lkm-immediate))

  (define-key
    global-map
    [menu-bar debug-menu step-over]
    '("Step Over" . lkm-step-over))
  
  (define-key
    global-map
    [menu-bar debug-menu step-out]
    '("Step Out" . lkm-step-out))  

  (define-key
    global-map
    [menu-bar debug-menu step-into]
    '("Step Into" . lkm-step-into))

  (define-key
    global-map
    [menu-bar debug-menu list-breakpoints]
    '("List Breakpoints" . lkm-list-breakpoints))    
  
  (define-key
    global-map
    [menu-bar debug-menu clear-all-breakpoints]
    '("Clear All Breakpoints" . lkm-clear-all-breakpoints))    

  (define-key
    global-map
    [menu-bar debug-menu clear-breakpoint]
    '("Clear Breakpoint" . lkm-clear-breakpoint))
  
  (define-key
    global-map
    [menu-bar debug-menu set-breakpoint]
    '("Set Breakpoint" . lkm-set-breakpoint))

  (define-key
    global-map
    [menu-bar debug-menu resume-debugger]
    '("Resume" . lkm-resume-debugger))

  (define-key
    global-map
    [menu-bar debug-menu break-debugger]
    '("Break" . lkm-interrupt-debugger))
  
  (define-key
    global-map
    [menu-bar debug-menu debug-routine]
    '("Debug Current Routine" . lkm-debug-routine))

  (define-key
    global-map
    [menu-bar debug-menu stop-debugger]
    '("Stop Debugger" . lkm-stop-debugger))    

  (define-key
    global-map
    [menu-bar debug-menu start-debugger]
    '("Start Debugger" . lkm-start-debugger))   
    
  (define-key
    global-map
    [menu-bar globals-menu gli]
    '("Examine" . lkm-gtm-global-lookup))
  (define-key
    global-map
    [menu-bar globals-menu gls]
    '("Examine at Cursor" . lkm-global-at-point))

  (define-key
    global-map
    [menu-bar vista-menu sachdr]
    '("Insert VistA SAC Header" . lkm-sac-header))

  (define-key
   global-map
    [menu-bar routines-menu jmp]
    '("Find Routine" . lkm-jump-to-routine-def))
  (define-key
    global-map
    [menu-bar mumps-menu cmp]
    '("Complete Keyword at Cursor" . lkm-complete-symbol))
  (define-key
    global-map
    [menu-bar mumps-menu com]
    '("Compile Current Buffer" . lkm-gtm-compile))

  (setq font-lock-defaults '((mumps-font-lock-keywords)))

  (setq mkua nil)
  (setq mkuf nil)
  (setq mkla nil)
  (setq mklf nil)
  (setq mfua nil)
  (setq mfuf nil)
  (setq mfla nil)
  (setq mflf nil)
  (setq mo nil) 

  (setq mode-name "LorikeeM MUMPS Developer Tools")
  ;(lkm-start-debugger)
  (run-hooks 'mumps-mode-hook)
)
