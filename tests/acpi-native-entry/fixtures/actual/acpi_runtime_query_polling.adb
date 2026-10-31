pragma Ada_2022;
package body ACPI_Runtime_Query_Polling with SPARK_Mode => Off is
   use type CuBit.Messages.Activity_Result;
   procedure Poll_Request
     (Sender : out CuBit.Messages.Process_ID; Message : out CuBit.Messages.Message; Found : out Boolean) is
   begin CuBit.Messages.Poll_Service_Request (Sender, Message, Found); end Poll_Request;
   procedure Poll_Event
     (Publisher : out CuBit.Messages.Process_ID; Message : out CuBit.Messages.Message; Found : out Boolean) is
   begin Found := CuBit.Messages.Poll_Event (Message, Publisher); end Poll_Event;
   procedure Wait_Activity (Deadline : ACPI_Passive_Query.Word; Usable : out Boolean) is
   begin Usable := CuBit.Messages.Wait_For_Activity_Until (Deadline) /= CuBit.Messages.Unavailable; end Wait_Activity;
end ACPI_Runtime_Query_Polling;
