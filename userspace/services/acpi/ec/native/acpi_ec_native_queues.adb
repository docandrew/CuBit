pragma Ada_2022;
with Interfaces;
package body ACPI_EC_Native_Queues is
   procedure No_Local_Work (Runnable, Stop, Receive_Request : out Boolean) is
   begin Runnable := False; Stop := False; Receive_Request := True; end No_Local_Work;
   package M renames CuBit.Messages;
   use type Interfaces.Unsigned_64, M.Activity_Result;
   function Unexpected_Completion return Boolean is
      Item : M.CompletionEntry;
   begin return M.Poll_Completion (Item'Address) /= 0; end Unexpected_Completion;
   function Poll_Event (Item : out M.Message; Publisher : out M.Process_ID) return Boolean is
   begin return M.Poll_Event (Item, Publisher); end Poll_Event;
   procedure Poll_Request
     (Sender : out M.Process_ID; Item : out M.Message; Found : out Boolean) is
   begin M.Poll_Service_Request (Sender, Item, Found); end Poll_Request;
   function Wait_Idle return Boolean is
   begin return M.Wait_For_Activity_Until (M.Wait_Forever) /= M.Unavailable; end Wait_Idle;
   procedure Ignore_Message
     (Sender : M.Process_ID; Item : M.Message; Stop, Handled : out Boolean) is
      pragma Unreferenced (Sender, Item);
   begin Stop := False; Handled := False; end Ignore_Message;
end ACPI_EC_Native_Queues;
