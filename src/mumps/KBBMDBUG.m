KBBMDBUG
 w "LorikeeM Interactive Debugger",!
 w " Copyright (C) 2013 Coherent Logic Development LLC",!,!
 w "Debugger is ready (Job ID ",$J,")",!
 s $zbreak="d ^KBBMDBUG"
 s $zprompt="DEBUG> "
 f i=0:0 u 0 r code w ! x code
 q
