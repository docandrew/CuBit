pragma Ada_2022;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
with GPIO_Native_Transport;
with GPIO_Native_Request_IO;
generic
   Launcher : CuBit.Handles.Process;
package GPIO_Native_Admin_IO is
   subtype Offer_Receipt is CuBit.Handles.IPC.Carried;
   function Authorized (Caller : CuBit.Process_IDs.Process_ID) return Boolean;
   procedure Take_Offer (Caller : CuBit.Process_IDs.Process_ID;
      Receipt : out Offer_Receipt; Owned : out Boolean);
   procedure Inspect (Receipt : Offer_Receipt; Sink : out GPIO_Native_Transport.Event_Sink;
      Owner : out CuBit.Process_IDs.Process_ID; Badge : out GPIO_Control_Wire.Word; Valid : out Boolean);
   procedure Discard (Receipt : Offer_Receipt; Closed : out Boolean);
   procedure Save (Reply : out CuBit.Handles.Reply; Saved : out Boolean)
      renames GPIO_Native_Request_IO.Save;
   procedure Answer_Saved (Reply : CuBit.Handles.Reply;
      Label : GPIO_Control_Wire.Operation; Status : GPIO_Control_Wire.Completion_Status; Delivered : out Boolean);
   procedure Answer_Current (Caller : CuBit.Process_IDs.Process_ID;
      Label : GPIO_Control_Wire.Operation; Status : GPIO_Control_Wire.Completion_Status; Delivered : out Boolean);
   -- Launcher is Startup.launcher Process authority. No raw handle constructor
   -- or raw-capability syscall. Close(Carried) requires coordinator's pending
   -- runtime overload; retained receipts remain owned when closure is refused.
end GPIO_Native_Admin_IO;
