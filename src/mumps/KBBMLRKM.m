KBBMLRKM ; LorikeeM runtime library
 n cmdLine,oper,param
 s cmdLine=$zcmd
 s oper=$P(cmdLine,"~",1)
 s param=$P(cmdLine,"~",2)
 i oper="EXTRINSIC" d extrinsic(param)
 i oper="ROUTINE" d routine(param)
 q

extrinsic(parm)
 n execStr s execStr="W $$"_parm_",!"
 x execStr
 q

routine(parm)
 n execStr s execStr="D "_parm
 x execStr
 q