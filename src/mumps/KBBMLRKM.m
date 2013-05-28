KBBMLRKM ; LorikeeM runtime library
 n cmdLine,oper,param
 s cmdLine=$zcmd
 s oper=$P(cmdLine,"~",1)
 s param=$P(cmdLine,"~",2)
 i oper="EXTRINSIC" d extrinsic(param)
 i oper="ROUTINE" d routine(param)
 i oper="DEBUG" d debugger
 q

extrinsic(parm)
 n execStr s execStr="W $$"_parm_",!"
 x execStr
 q

routine(parm)
 n execStr s execStr="D "_parm
 x execStr
 q

debugger
 w "LorikeeM Interactive Debugger",!
 w " Copyright (C) 2013 Coherent Logic Development LLC",!,!
 w "Debugger is ready.",!
 f i=0:0 u 0 r code x code
 q
