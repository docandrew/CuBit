pragma Ada_2022;
with Interfaces; with ACPI_Requests; with CuBit.Messages;
package ACPI_Native_Power_Entry is
   type Step_Outcome is (Idle, Progress, Replied, Caller_Gone);
   procedure Dispatch (Server : ACPI_Requests.State; Request : CuBit.Messages.Message;
      Runtime_Selected : Boolean; Reply : out CuBit.Messages.Message; Handled, Reply_Now : out Boolean)
     with Pre => ACPI_Requests.Valid (Server);
   procedure Advance (Server : ACPI_Requests.State; Result : out Step_Outcome)
     with Pre => ACPI_Requests.Valid (Server);
   procedure Begin_Stop;
   function Runnable return Boolean;
   function Blocks_Runtime return Boolean;
   function Quiet return Boolean;
   function Wake_Deadline_Milliseconds return Interfaces.Unsigned_64;
end ACPI_Native_Power_Entry;
