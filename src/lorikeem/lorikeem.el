;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LorikeeM MUMPS Developer Tools
;;
;; Provides syntax highlighting and global dump for GNU Emacs
;;
;; Note that I have concentrated mostly on GT.M syntax here. the
;; list of keywords and functions comes from Edition 6.1 of 
;; Jon Diamond's "Standard M Pocket Guide." I have not left out
;; any InterSystems Cache-specific syntax, but have not tested
;; its use on any Cache-specific code.
;;
;; Written by John Willis
;; john@coherent-logic.com
;;
;; Copyright (C) 2010, 2012 Coherent Logic Development LLC
;;
;; This product is provided under the Logical Public License v1.0.
;; The license should be included with the distribution as a file
;; named "COPYING", in the root directory of the distribution archive.
;; 
;; If you did not receive a copy of this license, please see
;; http://www.coherent-logic.com/licenses/lpl.html
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
  '("$A" "$C" "$D" "$E" "$F" "$FN" "$G" "$IN" "$I" "$J" "$L" "$LI" "$LB" "$LD" "$LF" "$LFS" "$LG" "$LL" "$LS" "$LTS" "$NA" "$N" "$NUM" "$O" "$P" "$Q" "$QL" "$QS" "$R" "$S" "$ST" "$T" "$TR" "$V" "$ZBA" "$ZBC" "$ZBF" "$ZBG" "$ZBL" "$ZBN" "$ZB" "$ZBSE" "$ZBST" "$ZBX" "$ZCVT" "$ZC" "$ZD" "$ZDH" "$ZDT" "$ZDTH" "$ZDEV" "$ZI" "$ZO" "$ZP" "$ZSE" "$ZSO" "$ZT" "$ZTH" "$ZTL" "$ZU")
  "MUMPS uppercase abbreviated functions")

(defvar mumps-functions-lcase-abbrev
  '("$a" "$c" "$d" "$e" "$f" "$fn" "$g" "$in" "$i" "$j" "$l" "$li" "$lb" "$ld" "$lf" "$lfs" "$lg" "$ll" "$ls" "$lts" "$na" "$n" "$num" "$o" "$p" "$q" "$ql" "$qs" "$r" "$s" "$st" "$t" "$tr" "$v" "$zba" "$zbc" "$zbf" "$zbg" "$zbl" "$zbn" "$zb" "$zbse" "$zbst" "$zbx" "$zcvt" "$zc" "$zd" "$zdh" "$zdt" "$zdth" "$zdev" "$zi" "$zo" "$zp" "$zse" "$zso" "$zt" "$zth" "$ztl" "$zu")
  "MUMPS lowercase abbreviated functions")

(defvar mumps-functions-ucase-full
  '("$ASCII" "$BIT" "$BITCOUNT" "$BITFIND" "$BITLOGIC" "$CASE" "$CHAR" "$DATA" "$EXTRACT" "$FACTOR" "$FIND" "$FNUMBER" "$GET" "$INCREMENT" "$INUMBER" "$ISOBJECT" "$ISVALIDNUM" "$JUSTIFY" "$LENGTH" "$LIST" "$LISTBUILD" "$LB" "$LISTDATA" "$LISTFIND" "$LISTFROMSTRING" "$LISTGET" "$LISTLENGTH" "$LISTNEXT" "$LISTSAME" "$LISTTOSTRING" "$NAME" "$NEXT" "$NORMALIZE" "$NUMBER" "$ORDER" "$PIECE" "$QLENGTH" "$QSUBSCRIPT" "$QUERY" "$RANDOM" "$REVERSE" "$SELECT" "$SORTBEGIN" "$SORTEND" "$STACK" "$SYSTEM" "$TEXT" "$TRANSLATE" "$VIEW" "$ZABS" "$ZARCCOS" "$ZARCSIN" "$ZARCTAN" "$ZBAND" "$ZBCOUNT" "$ZBFIND" "$ZBGET" "$ZBIT" "$ZZBITAND" "$ZBITCOUNT" "$ZBITFIND" "$ZBITGET" "$ZBITLEN" "$ZBITNOT" "$ZBITOR" "$ZBITSET" "$ZBITSTR" "$ZBITXOR" "$ZBLEN" "$ZBNOT" "$ZBOOLEAN" "$ZBOR" "$ZBSET" "$ZBSTR" "$ZBXOR" "$ZCONVERT" "$ZCVT" "$ZCOS" "$ZCOT" "$ZCRC" "$ZCSC" "$ZCYC" "$ZDATE" "$ZDATEH" "$ZDATETIME" "$ZDATETIMEH" "$ZDEVICE" "$ZEXP" "$ZF" "$ZHEX" "$ZINCREMENT" "$ZINFO" "$ZLN" "$ZLOG" "$ZMESSAGE" "$ZNAME" "$ZNEXT" "$ZOBJCLASSMETHOD" "$ZOBJPROPERTY" "$ZORDER" "$ZPARSE" "$ZPOWER" "$ZPREVIOUS" "$ZSEARCH" "$ZSEC" "$ZSEEK" "$ZSIN" "$ZSOCKET" "$ZSORT" "$ZSQR" "$ZSTRIP" "$ZTAN" "$ZTEXP" "$ZTIME" "$ZTIMEH" "$ZTLOG" "$ZTRNLMN" "$ZUCI")
  "MUMPS uppercase full-length functions")

(defvar mumps-functions-lcase-full
    '("$ascii" "$bit" "$bitcount" "$bitfind" "$bitlogic" "$case" "$char" "$data" "$extract" "$factor" "$find" "$fnumber" "$get" "$increment" "$inumber" "$isobject" "$isvalidnum" "$justify" "$length" "$list" "$listbuild" "$lb" "$listdata" "$listfind" "$listfromstring" "$listget" "$listlength" "$listnext" "$listsame" "$listtostring" "$name" "$next" "$normalize" "$number" "$order" "$piece" "$qlength" "$qsubscript" "$query" "$random" "$reverse" "$select" "$sortbegin" "$sortend" "$stack" "$system" "$text" "$translate" "$view" "$zabs" "$zarccos" "$zarcsin" "$zarctan" "$zband" "$zbcount" "$zbfind" "$zbget" "$zbit" "$zzbitand" "$zbitcount" "$zbitfind" "$zbitget" "$zbitlen" "$zbitnot" "$zbitor" "$zbitset" "$zbitstr" "$zbitxor" "$zblen" "$zbnot" "$zboolean" "$zbor" "$zbset" "$zbstr" "$zbxor" "$zconvert" "$zcvt" "$zcos" "$zcot" "$zcrc" "$zcsc" "$zcyc" "$zdate" "$zdateh" "$zdatetime" "$zdatetimeh" "$zdevice" "$zexp" "$zf" "$zhex" "$zincrement" "$zinfo" "$zln" "$zlog" "$zmessage" "$zname" "$znext" "$zobjclassmethod" "$zobjproperty" "$zorder" "$zparse" "$zpower" "$zprevious" "$zsearch" "$zsec" "$zseek" "$zsin" "$zsocket" "$zsort" "$zsqr" "$zstrip" "$ztan" "$ztexp" "$ztime" "$ztimeh" "$ztlog" "$ztrnlmn" "$zuci")
    "MUMPS lowercase full-length functions")

;; define keywords for completion
(defvar mumps-keywords
  '("BREAK" "CLOSE" "CONTINUE" "DO" "ELSE" "ELSEIF" "FOR" "GOTO" "HALT" "HANG" "IF" "JOB" "KILL" "LOCK" "MERGE" "NEW" "OPEN" "PRINT" "QUIT" "READ" "SET" "TCOMMIT" "TRESTART" "TROLLBACK" "TSTART" "USE" "VIEW" "WHILE" "WRITE" "XECUTE" "ZALLOCATE" "ZBREAK" "ZDEALLOCATE" "ZHANG" "ZHOROLOG" "ZINSERT" "ZKILL" "ZLOAD" "ZNSPACE" "ZPRINT" "ZQUIT" "ZREMOVE" "ZSAVE" "ZSYNC" "ZSYSTEM" "ZTCOMMIT" "ZTRAP" "ZTSTART" "ZUSE" "ZWITHDRAW" "ZWRITE" "ZZDUMP" "break" "close" "continue" "do" "else" "elseif" "for" "goto" "halt" "hang" "if" "job" "kill" "lock" "merge" "new" "open" "print" "quit" "read" "set" "tcommit" "trestart" "trollback" "tstart" "use" "view" "while" "write" "xecute" "zallocate" "zbreak" "zdeallocate" "zhang" "zhorolog" "zinsert" "zkill" "zload" "znspace" "zprint" "zquit" "zremove" "zsave" "zsync" "zsystem" "ztcommit" "ztrap" "ztstart" "zuse" "zwithdraw" "zwrite" "zzdump" "$ASCII" "$BIT" "$BITCOUNT" "$BITFIND" "$BITLOGIC" "$CASE" "$CHAR" "$DATA" "$EXTRACT" "$FACTOR" "$FIND" "$FNUMBER" "$GET" "$INCREMENT" "$INUMBER" "$ISOBJECT" "$ISVALIDNUM" "$JUSTIFY" "$LENGTH" "$LIST" "$LISTBUILD" "$LB" "$LISTDATA" "$LISTFIND" "$LISTFROMSTRING" "$LISTGET" "$LISTLENGTH" "$LISTNEXT" "$LISTSAME" "$LISTTOSTRING" "$NAME" "$NEXT" "$NORMALIZE" "$NUMBER" "$ORDER" "$PIECE" "$QLENGTH" "$QSUBSCRIPT" "$QUERY" "$RANDOM" "$REVERSE" "$SELECT" "$SORTBEGIN" "$SORTEND" "$STACK" "$SYSTEM" "$TEXT" "$TRANSLATE" "$VIEW" "$ZABS" "$ZARCCOS" "$ZARCSIN" "$ZARCTAN" "$ZBAND" "$ZBCOUNT" "$ZBFIND" "$ZBGET" "$ZBIT" "$ZZBITAND" "$ZBITCOUNT" "$ZBITFIND" "$ZBITGET" "$ZBITLEN" "$ZBITNOT" "$ZBITOR" "$ZBITSET" "$ZBITSTR" "$ZBITXOR" "$ZBLEN" "$ZBNOT" "$ZBOOLEAN" "$ZBOR" "$ZBSET" "$ZBSTR" "$ZBXOR" "$ZCONVERT" "$ZCVT" "$ZCOS" "$ZCOT" "$ZCRC" "$ZCSC" "$ZCYC" "$ZDATE" "$ZDATEH" "$ZDATETIME" "$ZDATETIMEH" "$ZDEVICE" "$ZEXP" "$ZF" "$ZHEX" "$ZINCREMENT" "$ZINFO" "$ZLN" "$ZLOG" "$ZMESSAGE" "$ZNAME" "$ZNEXT" "$ZOBJCLASSMETHOD" "$ZOBJPROPERTY" "$ZORDER" "$ZPARSE" "$ZPOWER" "$ZPREVIOUS" "$ZSEARCH" "$ZSEC" "$ZSEEK" "$ZSIN" "$ZSOCKET" "$ZSORT" "$ZSQR" "$ZSTRIP" "$ZTAN" "$ZTEXP" "$ZTIME" "$ZTIMEH" "$ZTLOG" "$ZTRNLMN" "$ZUCI" "$ascii" "$bit" "$bitcount" "$bitfind" "$bitlogic" "$case" "$char" "$data" "$extract" "$factor" "$find" "$fnumber" "$get" "$increment" "$inumber" "$isobject" "$isvalidnum" "$justify" "$length" "$list" "$listbuild" "$lb" "$listdata" "$listfind" "$listfromstring" "$listget" "$listlength" "$listnext" "$listsame" "$listtostring" "$name" "$next" "$normalize" "$number" "$order" "$piece" "$qlength" "$qsubscript" "$query" "$random" "$reverse" "$select" "$sortbegin" "$sortend" "$stack" "$system" "$text" "$translate" "$view" "$zabs" "$zarccos" "$zarcsin" "$zarctan" "$zband" "$zbcount" "$zbfind" "$zbget" "$zbit" "$zzbitand" "$zbitcount" "$zbitfind" "$zbitget" "$zbitlen" "$zbitnot" "$zbitor" "$zbitset" "$zbitstr" "$zbitxor" "$zblen" "$zbnot" "$zboolean" "$zbor" "$zbset" "$zbstr" "$zbxor" "$zconvert" "$zcvt" "$zcos" "$zcot" "$zcrc" "$zcsc" "$zcyc" "$zdate" "$zdateh" "$zdatetime" "$zdatetimeh" "$zdevice" "$zexp" "$zf" "$zhex" "$zincrement" "$zinfo" "$zln" "$zlog" "$zmessage" "$zname" "$znext" "$zobjclassmethod" "$zobjproperty" "$zorder" "$zparse" "$zpower" "$zprevious" "$zsearch" "$zsec" "$zseek" "$zsin" "$zsocket" "$zsort" "$zsqr" "$zstrip" "$ztan" "$ztexp" "$ztime" "$ztimeh" "$ztlog" "$ztrnlmn" "$zuci")
  "MUMPS keywords for completion")

;; build the regexps from the lists
(defvar mkuf (regexp-opt mumps-keywords-ucase-full 'words))
(defvar mklf (regexp-opt mumps-keywords-lcase-full 'words))
(defvar mkua (regexp-opt mumps-keywords-ucase-abbrev 'words))
(defvar mkla (regexp-opt mumps-keywords-lcase-abbrev 'words))
(defvar mfuf (regexp-opt mumps-functions-ucase-full 'words))
(defvar mflf (regexp-opt mumps-functions-lcase-full 'words))
(defvar mfua (regexp-opt mumps-functions-ucase-abbrev 'words))
(defvar mfla (regexp-opt mumps-functions-lcase-abbrev 'words))



;; clear un-needed memory resources
(setq mumps-keywords-ucase-abbrev nil)
(setq mumps-keywords-ucase-full nil)
(setq mumps-keywords-lcase-abbrev nil)
(setq mumps-keywords-lcase-full nil)
(setq mumps-functions-lcase-full nil)
(setq mumps-functions-ucase-full nil)
(setq mumps-functions-ucase-abbrev nil)
(setq mumps-functions-lcase-abbrev nil)


;; create the thingy that we'll feed to font-lock-defaults
(setq mumps-font-lock-keywords 
      `(      
	(,";.*$" . font-lock-comment-face)
	(,mkua . font-lock-keyword-face)
	(,mkuf . font-lock-keyword-face)
	(,mkla . font-lock-keyword-face)
	(,mklf . font-lock-keyword-face)
	(,mfuf . font-lock-function-name-face)
	(,mfua . font-lock-function-name-face)
	(,mfla . font-lock-function-name-face)
	(,mflf . font-lock-function-name-face)
))

(defun lkm-about ()
  "About LorikeeM"
  (interactive)
  
  (setq lkm-about-buf "*About LorikeeM MUMPS Developer Tools*")
  (get-buffer-create lkm-about-buf)
  (with-current-buffer (get-buffer-create lkm-about-buf)
    (goto-char (point-max))
    (insert "LorikeeM MUMPS Developer Tools\n\n")
    (insert "GT.M Version                     :  " lkm-gtm-version "\n")
    (insert "GT.M Database Instance Path      :  " lkm-gtm-database-instance-path "\n")
    (insert "GT.M Global Directory Path       :  " lkm-gtm-global-directory "\n")
    (insert "GT.M Routine Search Path         :  " lkm-gtm-routines "\n")    
    (insert "GT.M Replication Status          :  " lkm-gtm-replication "," (downcase lkm-gtm-replication-side) "\n"))
  (switch-to-buffer-other-window lkm-about-buf))

(defun lkm-gtm-compile ()
  "Compiles the current buffer"
  (interactive)

  (setq file-name (buffer-file-name))
  (setq cmd-str (concat "mumps " file-name))
  (compile cmd-str nil))

(defun lkm-global-at-point ()
 "Look up the GT.M global at the word under the current point."

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
	(message "%s does not appear to be a MUMPS global." mm-current-word))
      )
    
  )
)

(defun lkm-gtm-global-lookup (global)
  "Look up a global in GT.M"
  (interactive "sWhat global would you like to look up? ")
  
  (get-buffer-create "*MUMPS Global Examiner*")
  (with-current-buffer (get-buffer-create "*MUMPS Global Examiner*")
    (goto-char (point-max))
    (insert "MUMPS Global Dump of " global "\n\n"))
  (call-process "mumps" nil "*MUMPS Global Examiner*" nil "-r" "KBBMLRKM" global)
  (display-buffer (get-buffer "*MUMPS Global Examiner*") t)
  (with-current-buffer (get-buffer-create "*MUMPS Global Examiner*")
    (goto-char (point-max))))

(defun lkm-jump-to-routine-def ()
  "Jump to the definition of the routine under the cursor."
  (interactive)
  (setq lkm-current-symb (thing-at-point 'symbol))
  (find-tag-other-window lkm-current-symb))

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



(define-derived-mode mumps-mode fundamental-mode
  "mumps mode"
  "Major mode for MUMPS"

  (setq lkm-version "0.99.1")
  (message "LorikeeM MUMPS Developer Tools %s (GT.M %s)" lkm-version lkm-gtm-version)

  ;;
  ;; set up syntax table entries
  ;;
  
  ;; syntax table entries for symbol constituents
  ;;  we're adding ^ and % to accommodate MUMPS routine calls
  (modify-syntax-entry ?^ "_")
  (modify-syntax-entry ?% "_")
  (modify-syntax-entry ?$ "-")

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
  (global-set-key (kbd "<f5>") 'lkm-complete-symbol)
  (global-set-key (kbd "<f9>") 'lkm-gtm-compile)

  ;;
  ;; set up the MUMPS menu to be loaded after the Tools menu
  ;;
  (define-key-after
    global-map
    [menu-bar mumps-menu]
    (cons "LorikeeM" (make-sparse-keymap "mumps"))
    'tools )
  (define-key
    global-map
    [menu-bar mumps-menu gli]
    '("Examine Global" . lkm-gtm-global-lookup))
  (define-key
    global-map
    [menu-bar mumps-menu gls]
    '("Examine Global at Cursor" . lkm-global-at-point))
  (define-key
   global-map
    [menu-bar mumps-menu jmp]
    '("Jump to Routine Definition" . lkm-jump-to-routine-def))
  (define-key
    global-map
    [menu-bar mumps-menu cmp]
    '("Complete Keyword at Cursor" . lkm-complete-symbol))
  (define-key
    global-map
    [menu-bar mumps-menu abt]
    '("About LorikeeM" . lkm-about))
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

  (setq mode-name "LorikeeM")
  (run-hooks 'mumps-mode-hook)
)
