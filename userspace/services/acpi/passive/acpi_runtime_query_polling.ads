pragma Ada_2022;
with CuBit.Messages;
with ACPI_Passive_Query;
-- Separate native queues. Typed request/event decoding belongs to the service;
-- these wrappers neither combine queues nor invent message selectors.
package ACPI_Runtime_Query_Polling with SPARK_Mode => Off is
   procedure Poll_Request
     (Sender : out CuBit.Messages.Process_ID; Message : out CuBit.Messages.Message; Found : out Boolean);
   procedure Poll_Event
     (Publisher : out CuBit.Messages.Process_ID; Message : out CuBit.Messages.Message; Found : out Boolean);
   procedure Wait_Activity (Deadline : ACPI_Passive_Query.Word; Usable : out Boolean);
end ACPI_Runtime_Query_Polling;
