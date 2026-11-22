DefinitionBlock ("", "DSDT", 1, "CUBIT", "FMTTEST", 1)
{
 Name (MARK, Zero)
 Name (PKG0, Package () {Zero}) Method (SRC0,0) {Store (Add(Multiply(MARK,10),1),MARK) Return(PKG0)} Method (TGT0,0) {Store(Add(Multiply(MARK,10),2),MARK) Return(Zero)}
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TEST, 0) {Return(ToBuffer(ToDecimalString(SRC0(),Index(PKG0,TGT0()))))}
}
