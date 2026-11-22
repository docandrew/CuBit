# Corrected small CreateField witnesses

Worker83844 terminal0.10 normal compiled attempts:8returned,2runtimeerrors, no compilerrejections/timeouts/missingresults. Every DSDTrevision/runtimewidth/opcode gate passed; ARG HELP declaration additionally checked.13sourceinputs unchanged. Same5s128MiB64MiB1MiBsubprocess caps. Originalck93a1pt untouched.

GEN1 evaluated value type3/Buffer length1:769 at bothwidths. QTYPE evaluated value type3/Buffer length8 at32bit:776; type1/Integer bytewidth8 at64bit:264. Field metadata ObjectType remains14 separately; these measure value throughLocal0, not metadata. Prior rawbytes establish exactcontent.

REPEAT second declaration returns AE_ALREADY_EXISTS with MARK2 bothwidths: index effect executed on seconditeration before duplicate failure. Disassembly has one CreateByteField inside While and TEST.FLD0, establishing true repeatedname in sameinvocation rather than shadowing.

LOCAL and ARG return16706=65*256+66 with MARK1 bothwidths: savedoldIndex reads65, newlydefinedfield reads66 after CHNG. Disassembly preserves CopyObject(Index(oldcell,0),Local2), CHNG(RefOf(cell)), DerefOf(Local2). Pinned exoparg1.c636 copies operand object; utcopy.c907 preserves LOCAL_REFERENCE descriptor and incrementsreferent. Thus oldbuffer didnot change and field resolves new binding afterlaterindexeffects in these exact cases. This closes ambiguity in earlier barebyte results; doesnot establish arbitraryreference recursion or every coercion path.

No unsafeoffset/highword/HUGE, productionchanges, CuBit execution or proof. Slot2released afterterminal. No further probes needed for these gaps.
