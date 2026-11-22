DefinitionBlock ("", "DSDT", 2, "CUBIT", "METACTX", 1) {
 Device (DEV0) {
  Name (_HID, "TEST0001")
  Method (_INI, 0) { Notify (DEV0, 128) Notify (DEV0, 129) Notify (DEV0, 130) }
 }
}
