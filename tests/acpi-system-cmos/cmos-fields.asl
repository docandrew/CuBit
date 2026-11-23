DefinitionBlock ("", "DSDT", 2, "CUBIT", "CMOSTEST", 1)
{
 OperationRegion (CMOS, SystemCMOS, 0, 63)
 Field (CMOS, ByteAcc, Lock, Preserve) { Offset (4), HOR, 8, Offset (20), RWB0, 8, Offset (50), CENT, 8 }
 Method (RDTM, 0) { Return (HOR) }
 Method (WRT0, 1) { Store (Arg0, RWB0) Return (RWB0) }
 Method (WRO0, 1) { Store (Arg0, HOR) Return (HOR) }
 Method (WRC0, 1) { Store (Arg0, CENT) Return (CENT) }
}
