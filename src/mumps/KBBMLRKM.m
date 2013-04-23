;
; KBBMLRKM.m
;  LorikeeM global dump support routines.
;
; Copyright (C) 2012 Coherent Logic Development LLC
;
; Distributed under the terms of the Logical Public License v1.0.
; This license should have been included with this distribution
; in a file named COPYING.
;
KBBMLRKM
	new cmdLine
	set cmdLine=$zcmd
	do globalDump(cmdLine)
	quit

globalDump(globalName)
	set globalName="^"_globalName
	zwrite @globalName
	quit 