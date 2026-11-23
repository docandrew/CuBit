DefinitionBlock("","DSDT",2,"CUBIT","PCICTX",1){
Scope(\_SB){
Device(PC00){
Name(_HID,"CUB0001") Name(_CID,Package(){
"PNP0A08"}
) Name(_SEG,7) Name(_BBN,11)Device(RP04){
Name(_ADR,1835011) Method(_INI,0,NotSerialized){
If(^PXSX.PNVM()||^PXSX.PAHC()){
Noop}
}
Device(PXSX){
Name(_ADR,Zero)OperationRegion (PCCX, PCI_Config, Zero, 0x10)
 Field (PCCX, ByteAcc, NoLock, Preserve) {
Offset(9), PIXX,8,SCCX,8,BCCX,8}
Method (PNVM,0,Serialized) {
 If (BCCX==One) {
If (SCCX==8) {
If (PIXX==2) {
Return(One)}
}
}
 Return(Zero) }
Method (PAHC,0,Serialized) {
 If (BCCX==One) {
If (SCCX==6) {
If (PIXX==1) {
Return(One)}
}
}
 Return(Zero) }
}
}
Device(RP07){
Name(_ADR,1835014) Method(_INI,0,NotSerialized){
If(^PXSX.PNVM()||^PXSX.PAHC()){
Noop}
}
Device(PXSX){
Name(_ADR,Zero)OperationRegion (PCCX, PCI_Config, Zero, 0x10)
 Field (PCCX, ByteAcc, NoLock, Preserve) {
Offset(9), PIXX,8,SCCX,8,BCCX,8}
Method (PNVM,0,Serialized) {
 If (BCCX==One) {
If (SCCX==8) {
If (PIXX==2) {
Return(One)}
}
}
 Return(Zero) }
Method (PAHC,0,Serialized) {
 If (BCCX==One) {
If (SCCX==6) {
If (PIXX==1) {
Return(One)}
}
}
 Return(Zero) }
}
}
}
}
}

