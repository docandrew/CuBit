pragma Ada_2022;
with CuBit.Handles;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
package GPIO_Native_Request_IO is
   procedure Save (Reply : out CuBit.Handles.Reply; Saved : out Boolean);
   procedure Answer_Saved (Reply : CuBit.Handles.Reply;
      Label : GPIO_Control_Wire.Operation; Value : GPIO_Control_Wire.Completion;
      Delivered : out Boolean);
   procedure Answer_Current (Caller : CuBit.Process_IDs.Process_ID;
      Label : GPIO_Control_Wire.Operation; Value : GPIO_Control_Wire.Completion;
      Delivered : out Boolean);
   -- Save_Reply failure leaves the current reply in the thread. Answer_Current
   -- attempts that current reply once; failure is not delivery/consumption
   -- evidence. Never retain a PID instead of saved authority. A one-way message
   -- cannot cause work by failing Save_Reply.
end GPIO_Native_Request_IO;
