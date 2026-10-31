with Interfaces;
with CuBit.Messages;
with ACPI_Region_Policy;
with ACPI_Region_IO;
-- Trusted backend process only. Request must come from kernel IPC receive.
-- Linking this adapter into the AML process would not provide isolation.
generic
   type Hardware_State is private;
   with procedure Transact
     (Hardware : in out Hardware_State;
      Space : ACPI_Region_Policy.Address_Space; Address : Interfaces.Unsigned_64;
      Width : ACPI_Region_Policy.Access_Width; For_Write : Boolean;
      Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64;
      Completed : out Boolean);
package ACPI_Backend_Endpoint is
   procedure Dispatch
     (Region : in out ACPI_Region_Policy.State;
      Hardware : in out Hardware_State;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message;
      Pending_Ticket : out Interfaces.Unsigned_64);
private
   package Core is new ACPI_Region_IO (Hardware_State, Transact);
end ACPI_Backend_Endpoint;
