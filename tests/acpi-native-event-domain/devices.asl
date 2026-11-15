DefinitionBlock ("", "DSDT", 2, "CUBIT", "HIDOWNER", 1)
{
    Name(LIDS,One) Name(ACST,One) Name(DREG,0x20)
    Device (TPD0)
    {
        Name (_HID, "TEST0001")
        Name (_CID, "PNP0C50")
        Name (_STA, 0x0F)
        Method (_CRS, 0)
        {
            
            Return (ResourceTemplate () {
                I2cSerialBusV2 (0x15, ControllerInitiated, 400000, AddressingMode7Bit,
                    "\\I2C0", 0, ResourceConsumer, , Exclusive)
                GpioInt (Level, ActiveLow, Exclusive, PullUp, 0,
                    "\\GPIO", 0, ResourceConsumer) {0x2A}
            })
        }
        Method (_DSM, 4)
        {
            If (LNotEqual (Arg0, ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"))) { Return (Zero) }
            If (LNotEqual (Arg1, One)) { Return (Zero) }
            If (LEqual (Arg2, Zero)) { Return (Buffer () {3}) }
            If (LEqual (Arg2, One)) { Return (DREG) }
            Return (Zero)
        }
    }
    Device (I2C0) { Name (_HID, "TEST0002") }
    Device (GPIO) { Name (_HID, "INTC1057") }
    Device (TPD1)
    {
        Name (_HID, "TEST0003")
        Name (_CID, Package () {"OTHER000", "PNP0C50"})
        Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) }
    }
Scope (_GPE) { Method (_L09,0) {  Notify (\TPD0,0x80) Return (Package (2) {One,2}) }  }
 Device(LID0) { /*INI*/ Name(_HID,EisaId("PNP0C0D")) Method(_LID,0) { Return(LIDS) } }
 Device(AC00) { Name(_HID,"ACPI0003") Method(_PSR,0) { Return(ACST) } }
 Device(BAT0) {
  Name(_HID,EisaId("PNP0C0A")) Name(_STA,0x1F) Name(REMA,20000)
  Method(_BIX,0) { Return(Package(20){0,1,42000,40000,1,11000,2000,1000,12,100000,0xFFFFFFFF,10,500,5,1,10,"CUBIT","SER1","LION",""}) }
  Method(_BST,0) { Local0=Package(4){1,100,0,11000} Local0[2]=REMA Return(Local0) }
 }
 /*BATTERY_RECOVERY*/
 Device(EC0) {
  Name(_HID,EisaId("PNP0C09")) Name(_STA,0x0F) Name(_GLK,Zero) Name(_GPE,0x6E)
  Method(_CRS,0) { Return(ResourceTemplate(){IO(Decode16,0x62,0x62,0,1) IO(Decode16,0x66,0x66,0,1)}) }
  Method(_REG,2) { If(LEqual(Arg1,Zero)) { Notify(\LID0,0x80) } }
  Method(_Q58,0) {
   Store(Zero,LIDS) Store(Zero,ACST) Store(15000,\BAT0.REMA) Store(0x21,DREG)
   Notify(\LID0,0x80) Notify(\AC00,0x80) Notify(\BAT0,0x80) Notify(\TPD0,0x81)
   Return(Package(){One,"event"})
  }
 }
}
