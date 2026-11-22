/* Ordinary complete-inventory refusal: seventeen batteries exceed the
 * production bound16, without unsafe IO or retained AML results. The lid and
 * AC source remain readable. Used with ACPI_SHUTDOWN_REFUSAL_TABLE and the
 * actual acpi-shutdown profile; SCI must still reach power.svc and S5 exits0.
 */
DefinitionBlock ("", "SSDT", 2, "CUBIT", "FAMREFUS", 1) {
 Device (LID0) { Name (_HID, "PNP0C0D") Method (_LID, 0) { Return (One) } }
 Device (AC00) { Name (_HID, "ACPI0003") Method (_PSR, 0) { Return (One) } }
 Device (BAT0) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BAT1) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BAT2) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BAT3) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BAT4) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BAT5) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BAT6) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BAT7) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BAT8) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BAT9) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BATA) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BATB) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BATC) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BATD) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BATE) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BATF) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
 Device (BATG) { Name (_HID, "PNP0C0A") Name (_STA, 0x1F) Method (_BIF, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) } }
}
