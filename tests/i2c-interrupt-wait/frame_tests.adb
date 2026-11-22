pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with CuBit.Messages;
with CuBit.Process_IDs;
with Launch_Configuration;
with Device_Vector_ABI;
with I2C_Interrupt_Binding;
procedure Frame_Tests is
   package M renames CuBit.Messages;
   package P renames CuBit.Process_IDs;
   package LC renames Launch_Configuration;
   package V renames Device_Vector_ABI;
   --  Kernel IRQ label; native Main instantiates with Device_Interrupts.EVENT_IRQ.
   Kernel_IRQ_Label : constant Interfaces.Unsigned_32 := 1;
   package B is new I2C_Interrupt_Binding (Kernel_IRQ_Label);
   use type B.Mode, Interfaces.Unsigned_64;
   use type LC.Interrupt_Delivery;
   use type Interfaces.Unsigned_8, M.Protocol_Flags;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "frame check" & Checks'Image;
      end if;
   end Check;
   function Frame (Vector : V.Device_Vector) return M.Message is
      Msg : M.Message := M.NULL_MESSAGE;
   begin
      Msg.tag.label := Kernel_IRQ_Label;
      Msg.tag.length := V.Pending_Words;
      Msg.words (V.Word_Of (Vector)) := V.Mask_Of (Vector);
      return Msg;
   end Frame;
   Binding : B.Selection;
   Msg : M.Message;
   Expected : B.Mode;
   Peer : constant P.Process_ID := P.From_Word (16#F123_4567_89AB_CDEF#);
begin
   for Delivery in LC.Interrupt_Delivery loop
      for N in LC.Vector_Count loop
         for Authority in Boolean loop
            for Valid in Boolean loop
               for Count in V.Block_Size loop
                  for Head in V.Device_Vector loop
                     Expected := B.Invalid;
                     if Delivery = LC.Polled and then N = 0 and then not Authority then
                        Expected := B.Polling;
                     elsif Delivery = LC.MSI and then N = 1 and then Authority
                       and then Valid and then Count = 1
                     then
                        Expected := B.MSI;
                     end if;
                     Binding := B.Select_Mode (Delivery, N, Authority, Valid, Head, Count);
                     Check (Binding.Delivery = Expected);
                     if Expected = B.MSI then
                        Check (Binding.Head = Head);
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   for Head in V.Device_Vector loop
      Binding := (B.MSI, Head);
      for Bit in V.Device_Vector loop
         Msg := Frame (Bit);
         Check (B.Notice (Binding, P.No_Process, Msg) = (Head = Bit));
         Check (not B.Notice (Binding, Peer, Msg));
         Check (not B.Notice ((B.Polling, Head), P.No_Process, Msg));
         Check (not B.Notice ((B.Invalid, Head), P.No_Process, Msg));
      end loop;
      Msg := Frame (Head);
      for Length in Interfaces.Unsigned_8 loop
         Msg.tag.length := Length;
         Check (B.Notice (Binding, P.No_Process, Msg) = (Length = V.Pending_Words));
      end loop;
      Msg := Frame (Head);
      for Protocol in M.Protocol_Flags loop
         Msg.tag.protocol := Protocol;
         Check (B.Notice (Binding, P.No_Process, Msg) = (Protocol = 0));
      end loop;
      Msg := Frame (Head);
      for A in Boolean loop
         for T in Boolean loop
            for R in Boolean loop
               for O in Boolean loop
                  Msg.tag.ipc := (A, T, R, O);
                  Check (B.Notice (Binding, P.No_Process, Msg) =
                    (not A and not T and not R and not O));
               end loop;
            end loop;
         end loop;
      end loop;
      Msg := Frame (Head);
      Msg.tag.label := 0;
      Check (not B.Notice (Binding, P.No_Process, Msg));
      Msg := Frame (Head);
      Msg.tag.reserved := 1;
      Check (not B.Notice (Binding, P.No_Process, Msg));
      Msg.tag.reserved := Interfaces.Unsigned_16'Last;
      Check (not B.Notice (Binding, P.No_Process, Msg));
      Msg := Frame (Head);
      Msg.authorityTag := 1;
      Check (not B.Notice (Binding, P.No_Process, Msg));
      Msg.authorityTag := Interfaces.Unsigned_64'Last;
      Check (not B.Notice (Binding, P.No_Process, Msg));
      Msg := Frame (Head);
      Msg.words (0) := Interfaces.Unsigned_64'Last;
      Msg.words (1) := Interfaces.Unsigned_64'Last;
      Check (B.Notice (Binding, P.No_Process, Msg));
   end loop;
   Ada.Text_IO.Put_Line ("I2C interrupt binding checks " & Checks'Image);
end Frame_Tests;
