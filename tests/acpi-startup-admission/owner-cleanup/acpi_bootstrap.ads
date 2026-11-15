-- Hosted test model only; never a native runtime/provider implementation.
with ACPI_Service; package ACPI_Bootstrap is
 type Phase is (Fresh,Loading,Failed,Complete); type Device_Phase is (Not_Started,Incomplete,Device_Failed,Devices_Completed);
 type State is record Stage : Phase:=Fresh; Devices : Device_Phase:=Not_Started; Core : aliased ACPI_Service.State; end record;
 function Valid(Session : State) return Boolean is (Session.Core.Own<=1 and Session.Core.Other<=2 and Session.Core.Notifications<=3);
end ACPI_Bootstrap;