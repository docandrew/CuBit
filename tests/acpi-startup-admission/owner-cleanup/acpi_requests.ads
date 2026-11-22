-- Hosted test model only; never a native runtime/provider implementation.
with ACPI_Service; with ACPI_Bootstrap; with Interfaces; package ACPI_Requests is
 use type Interfaces.Unsigned_64;
 type State is record Boot : ACPI_Bootstrap.State; Version : Interfaces.Unsigned_64:=5; end record;
 function Valid(Server : State) return Boolean is (ACPI_Bootstrap.Valid(Server.Boot));
 function Revision(Server : State) return Interfaces.Unsigned_64 is (Server.Version);
end ACPI_Requests;