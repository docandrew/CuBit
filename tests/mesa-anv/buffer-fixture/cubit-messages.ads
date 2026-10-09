with Interfaces; use Interfaces;
package CuBit.Messages is
   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;
   type MessageTag is record
      label : Unsigned_32;
      length, flags : Unsigned_8;
      reserved : Unsigned_16;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((0, 0, 0, 0), 0, [others => 0]);
   Calls : Natural := 0;
   Accounting_Response : MessageWords := [0, 1, 12288, 8192];
   Fault : Natural := 0;
   Memory_Response : MessageWords := [0, 1, 1, 0];
   Map_Response : MessageWords := [0, 1, 1, 7 * 2 ** 32 + 8];
   Map_Request : MessageWords := [others => 0];
   Prepare_Response : MessageWords := [0, 1, 0, 0];
   Close_Response : MessageWords := [0, 1, 16#4750_0000_0000_0001#, 0];
   Retirement_Response : MessageWords := [0, 1, 0, 0];
   Register_Response : MessageWords := [0, 1, 0, 0];
   Submit_Response : MessageWords := [0, 1, 2, 0];
   Submit_Request : MessageWords := [others => 0];
   Update_Response : MessageWords := [0, 1, 1, 0];
   Update_Request : MessageWords := [others => 0];
   Bound_DMA, Last_Allocation_DMA : Unsigned_64 := 0;
   Wait_Forever : constant Unsigned_64 := Unsigned_64'Last;
   function capCall (Slot : CapabilitySlot; Msg : in out Message;
                     Deadline : Unsigned_64) return MessageTag;
end CuBit.Messages;
