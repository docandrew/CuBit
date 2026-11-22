pragma Ada_2022;
with Locked_Test_Core;
with Locked_Owner_Model;
with ACPI_Field_Startup_Owner;
package Locked_Test_Owner is new ACPI_Field_Startup_Owner
  (Locked_Test_Core, Locked_Owner_Model.Channel_State,
   Locked_Owner_Model.Peer, 0, Locked_Owner_Model.IPC_Clock, Locked_Owner_Model.Exchange,
   Maximum_Registers => 2, Maximum_Bindings => 4, Maximum_Confirmations => 2,
   Read_Lease_Clock => Locked_Owner_Model.Lease_Clock);
