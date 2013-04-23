KBAWDUMP ;FWSLC/DLW-Dump a global on the command line; 10/21/12 1:49pm
 ;;0.20.3;Axiom;****LOCAL RTN**;David Wicksell @2010-2012
 ;
 ; Written by David Wicksell <dlw@linux.com>
 ; Copyright © 2010,2011 Fourth Watch Software, LC
 ;
 ; This program is free software: you can redistribute it and/or modify
 ; it under the terms of the GNU Affero General Public License (AGPL)
 ; as published by the Free Software Foundation, either version 3 of
 ; the License, or (at your option) any later version.
 ;
 ; This program is distributed in the hope that it will be useful,
 ; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 ; GNU Affero General Public License for more details.
 ;
 ; You should have received a copy of the GNU Affero General Public License
 ; along with this program. If not, see http://www.gnu.org/licenses/.
 ;
 ; This routine was created for GT.M in a Unix environment.
 ;
 ; This routine will dump the contents of a MUMPS global
 ; from a shell prompt. It is invoked as a mumps -run
 ; command. E.g. mumps -r KBAWDUMP '^DD(0,0)'
 ;
 ; The single quotes around the global name are required.
 ; If you don't use them, then most shells (BASH for sure)
 ; will think you are trying to start a subshell with the
 ; parens. The "^" is optional, and putting a "-" as the first
 ; character, will dump only the node referenced, otherwise
 ; it will be assumed that you want that node and every one
 ; of its children.
 ;
 ; Added ability to deal with local variables, by dumping 
 ; every option in the global that the local variable could
 ; represent.
 ;
 ; Added the GLOBALS entry point, to simulate calling ^%GD
 ; from a shell. Also added the ability to specify a range.
 ;
 ; If your screen becomes messed up after piping this
 ; program to less, it probably means that "te" is not set
 ; to reset the terminal to the settings that were present
 ; when "ti" was called. GT.M may not be user friendly in
 ; this regard, though I haven't really dug into the issue.
 ; If you don't plan on doing your own custom terminal
 ; handling, a quick solution would be to create an alias
 ; for less to put your screen back to "sane" settings
 ; while using it. I.e. $ alias less='less;stty sane'
 ;
 ;
 N $ET S $ET="G ERR"
 ;
 N ZCMD,VIM S ZCMD=$ZCMD,VIM=0
 I $E(ZCMD)="-" S ZCMD=$E(ZCMD,2,$L(ZCMD)),VIM=1 ;set VIM mode (no children)
 ;
 N ARGS S ARGS=$L(ZCMD," ")
 I 'VIM,ARGS'=1!(ZCMD="") D  Q  ;normal argument handling
 . W "KBAWDUMP takes one argument, the global reference.",!
 ;
 I $E(ZCMD)'="^" S ZCMD="^"_ZCMD ;supply the ^, if not specified
 ;
 N GLOBAL
 I ZCMD["(" D  ;if you have subscripts, you need to isolate the contents
 . S GLOBAL=$P(ZCMD,"(")_"("
 . N CNT S CNT=$L(ZCMD,"(") ;handle multiple parens
 . N ARG S ARG=$P(ZCMD,"(",2,CNT),ARG=$E(ARG,1,$L(ARG)-1)
 . ;
 . N I,FLG,NUM,DONE S FLG=0,NUM=$L(ARG,",")
 . F I=1:1:NUM D
 . . S DONE=0
 . . S ARG(I)=$P(ARG,",",I)
 . . S:$L(ARG(I),"""")=2 FLG=1-FLG ;deal with "," in strings
 . . ;
 . . I $L(ARG(I),"""")=1,$E(ARG(I))'?1(1N,1".",1"-") D  ;local variables
 . . . Q:FLG  ;avoid a ",name," in a string being treated like a variable
 . . . S:VIM VIM=2
 . . . S:ARG(I)'="*" ARG(I)=":" ;subscript wildcard
 . . ;
 . . I $L(ARG(I),"""")=3&(ARG(I)["""_"!(ARG(I)["_""")) D  ;a string and variable
 . . . S:VIM VIM=2
 . . . S:ARG(I)'="*" ARG(I)=":" ;subscript wildcard
 . . S:ARG(I)="""""" ARG(I)="*" ;special case of "" used as a subscript for $O
 . . ;
 . . I $E(ARG(I),2)="""" D  ;deal with strings inside XECUTE strings
 . . . N DIV
 . . . F DIV=1:1:$L(ARG(I)) Q:$E(ARG(I),DIV)'=""""
 . . . S DIV=DIV-1
 . . . ;
 . . . N J,SCNT,NARG,FILL S SCNT=0,NARG=""
 . . . F J=1:1:$L(ARG(I)) D
 . . . . I $E(ARG(I),J)="""" S SCNT=SCNT+1
 . . . . E  D
 . . . . . S:SCNT $P(FILL,"""",SCNT/DIV+1)="",SCNT=0 ;put right amount of quotes
 . . . . . S NARG=NARG_FILL_$E(ARG(I),J),FILL="" ;build the argument back up
 . . . . ;
 . . . . I $L(ARG(I))=J D  ;put right amount of ending quotes
 . . . . . S $P(FILL,"""",SCNT/DIV+1)=""
 . . . . . S NARG=NARG_FILL
 . . . S GLOBAL=GLOBAL_NARG_"," ;rebuild the args
 . . . S DONE=1 ;set DONE to indicate args already built
 . . I $E(ARG(I),1)'=""""&((ARG(I)["(")!(ARG(I)[")")) D  ;complex subscripts
 . . . S ARG(I)=":" ;should catch most functions and variables with subscripts
 . . S:'DONE GLOBAL=GLOBAL_ARG(I)_"," ;rebuild the args
 . S GLOBAL=$E(GLOBAL,1,$L(GLOBAL)-1) ;get rid of the last comma
 . I $E(GLOBAL,$L(GLOBAL))'=")" D  ;some indirection strings get complicated
 . . I VIM S GLOBAL=GLOBAL_")" ;add the last ) back without the ,* in VIM mode
 . . E  S GLOBAL=GLOBAL_",*)" ;add the last ) back with the ,* from the shell
 ;deal with extra spaces at the end of the :ZWR command or a quoted argument
 E  S GLOBAL=$E(ZCMD,1,$S(ZCMD[" ":$F(ZCMD," ")-2,1:$L(ZCMD))) ;no subscripts
 ;ZWR command with no subscripts, postcondition stops WRITE if undefined
 I VIM=1,GLOBAL'["(" W:$D(@GLOBAL)#2 GLOBAL_"="_@GLOBAL,!
 E  ZWR @GLOBAL
 Q
 ;
GLOBALS ;List all the globals to standard out, a simpler version of ^%GD
 ;
 ; You can provide no arguments and it will list every global
 ; in the database. Or you can supply a range of global names,
 ; and it will list every global between the range, inclusive.
 ; You specify the range with a colon and if the colon is the
 ; first character, it will list all the globals from the
 ; beginning of the database to the global listed after the
 ; colon, and if there is nothing after the colon it will list
 ; every global from the global before the colon to the end of
 ; the database.
 ;
 ; E.g. mumps -r GLOBALS^KBAWDUMP A:Z
 ; E.g. mumps -r GLOBALS^KBAWDUMP :z
 ; E.g. mumps -r GLOBALS^KBAWDUMP a:
 ;
 ; A colon by itself will also list every global, same as no
 ; arguments. It is important that you have no spaces between
 ; the global names and the colon, it must be one argument on
 ; the command line.
 ;
 N $ET S $ET="G ERR"
 ;
 N ZCMD,ARGS S ZCMD=$ZCMD,ARGS=$L(ZCMD," ")
 ;
 I ARGS>1 D  Q  ;normal argument handling
 . W "GLOBALS^KBAWDUMP takes one argument, the range of the globals to list,",!
 . W "or no arguments and will list every global in the database.",!
 ;
 ;if given, parse range on the : (ignore multiple colons)
 N GLOBAL,END
 I ZCMD]"" S GLOBAL="^"_$P(ZCMD,":"),END="^"_$P(ZCMD,":",2)
 E  S GLOBAL="^%",END="" ;% sorts first in default ascii
 ;
 S:GLOBAL="^" GLOBAL="^%" S:END="^" END=""
 S:$E(GLOBAL,1,2)="^^" GLOBAL=$E(GLOBAL,2,$L(GLOBAL))
 S:$E(END,1,2)="^^" END=$E(END,2,$L(END))
 ;
 W:$D(@GLOBAL) GLOBAL,! ;need to print the initial global
 ;
 F  S GLOBAL=$O(@GLOBAL) Q:GLOBAL=""!(GLOBAL]]END&(END]""))  W GLOBAL,!
 Q
 ;
ERR ;Handle globals that don't exist
 N $ET S $ET="H"
 W $P($ZS," ",2,$L($ZS," ")),!
 Q
