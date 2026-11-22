with ACPI_EC_Registers;
with ACPI_EC_Types;
with ACPI_Region_Protocol;
--  Deferred-reply adapter for the existing named-register Field wire protocol.
--  Native owner must save/authenticate the exact reply channel before Begin_Call,
--  then drive Registers.Step from its loop and return Collect's reply once.
--  A deferred call is never encoded as an Indeterminate wire response merely
--  because hardware is still running. No synchronous peer callback is required.
generic
   with package Registers is new ACPI_EC_Registers (<>);
package ACPI_EC_Endpoint with SPARK_Mode is
   procedure Begin_Call
     (Sender : Registers.Owner_ID; Item : ACPI_Region_Protocol.Packet;
      Deadline : ACPI_EC_Types.Microseconds;
      Budget : ACPI_EC_Types.Work_Budget;
      Deferred : out Boolean; Token : out Registers.Operation_ID;
      Reply : out ACPI_Region_Protocol.Packet);
   procedure Collect
     (Fact : out Registers.Completion; Reply : out ACPI_Region_Protocol.Packet);
end ACPI_EC_Endpoint;
