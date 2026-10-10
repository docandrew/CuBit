with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_VM_Image;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
package body CuBit.Messages is
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 and Stamp = 99 then 99 else 0);
   function Ready return Boolean is (True);
   function Allocate
     (Index : Intel_GPU_Buffer_Backing.Slot;
      Pages : Intel_GPU_Buffer_Backing.Page_Count)
      return Intel_GPU_Buffer_Reply.Backing is
      Offset : constant Unsigned_64 := Unsigned_64 (Index - 1) * 8192;
   begin
      pragma Assert (Pages <= 2);
      Last_Allocation_DMA := 16#1000_0000# + Offset;
      return Intel_GPU_Buffer_Reply.From_Linear (16#1000_0000# + Offset,
              Intel_GPU_Buffer_Backing.CPU_Base + Offset,
              Unsigned_64 (Pages) * 4096, 16#1000_0000#);
   end Allocate;
   package Server is new Intel_GPU_Buffer_Requests (Session_Of, Ready);
   Object : Server.Service;
   package VM is new Intel_GPU_VM_Image (4);
   package Binding is new Server.Binding (VM);
   Image : VM.Image;
   VM_Ready : Boolean := False;
   function capCall (Slot : CapabilitySlot; Msg : in out Message;
                     Deadline : Unsigned_64) return MessageTag is
      -- This synchronous transport fixture does not model kernel deadlines.
      pragma Unreferenced (Deadline);
      Response : Server.Words;
      Expected : constant MessageTag := (Msg.tag.label, 4, 0, 0);
      Deferred : Server.Ticket;
      Consumed : Boolean;
   begin
      Calls := Calls + 1;
      pragma Assert (Msg.authorityTag = 0);
      if Msg.tag.label = 16#0A30# then
         pragma Assert (Msg.tag = Expected and Msg.words = [1, 0, 0, 0]);
         Response := (if Slot = 63 then Server.Words (Accounting_Response)
                      else [1, 1, 0, 0]);
      elsif Msg.tag.label = 16#0A20# then
         pragma Assert (Msg.tag = Expected and Msg.words = [1, 3, 0, 0]);
         Response := Server.Words (Memory_Response);
      elsif Msg.tag.label = Binding.Bind_Label then
         if not VM_Ready then
            VM.Initialize (Image, [16#2000000#, 16#2001000#, 16#2002000#, 16#2003000#], VM_Ready);
            pragma Assert (VM_Ready);
         end if;
         Binding.Handle (Object, Image, 99, 42, (if Slot = 63 then 99 else 0),
           Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
           Server.Words (Msg.words), Response);
         Bound_DMA := VM.Lookup (Image, Msg.words (2)) and 16#FFFF_F000#;
      elsif Msg.tag.label in 16#0A25# .. 16#0A26# then
         -- Wire fixture only: no native publication or GuC queueing.
         pragma Assert (Msg.tag = (Msg.tag.label, 4, 0, 0));
         pragma Assert (Msg.words = [1, 0, 0, 0]);
         Response := (if Slot = 63 then Server.Words
                        (if Msg.tag.label = 16#0A25# then Prepare_Response
                         else Register_Response)
                      else [1, 1, 0, 0]);
      elsif Msg.tag.label = 16#0A2D# then
         pragma Assert (Msg.tag = Expected and Msg.words = [1, 0, 0, 0]);
         Response := (if Slot = 63 then Server.Words (Retirement_Response)
                      else [1, 1, 0, 0]);
      elsif Msg.tag.label = 16#0A2C# then
         pragma Assert (Msg.tag = Expected and Msg.words = [1, 0, 0, 0]);
         Response := (if Slot = 63 then Server.Words (Close_Response)
                      else [1, 1, 0, 0]);
      elsif Msg.tag.label = 16#0A28# then
         Update_Request := Msg.words;
         Response := (if Slot = 63 then Server.Words (Update_Response)
                      else [1, 1, 0, 0]);
      elsif Msg.tag.label = 16#0A23# then
         Map_Request := Msg.words;
         Response := (if Slot = 63 then Server.Words (Map_Response)
                      else [1, 1, 0, 0]);
      else
      Server.Handle (Object, 42, (if Slot = 63 then 99 else 0),
                     Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
                     Server.Words (Msg.words), Response, Deferred);
      if Deferred /= 0 then
         Server.Complete (Object, Deferred,
           Allocate (Intel_GPU_Buffer_Backing.Slot (Deferred),
                     Intel_GPU_Buffer_Backing.Page_Count (Msg.words (2) / 4096)),
           Response, Consumed);
         pragma Assert (Consumed);
      end if;
      end if;
      Msg.tag := Expected;
      Msg.words := MessageWords (Response);
      case Fault is
         when 1 => return (0, 0, 0, 0);
         when 2 => Msg.tag.flags := 1;
         when 3 => Msg.words (1) := 2;
         when 4 => Msg.words (2) := 2 ** 32;
         when 5 => Msg.words (3) := 0;
         when 6 => Msg.words := [7, 1, 0, 0];
         when 7 => Msg.words := [1, 1, 123, 0];
         when 8 => Msg.tag.label := Msg.tag.label xor 1;
         when 9 => Msg.tag.length := 3;
         when 10 => Msg.tag.reserved := 1;
         when others => null;
      end case;
      return Expected;
   end capCall;
end CuBit.Messages;
