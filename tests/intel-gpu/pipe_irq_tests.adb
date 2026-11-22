with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
with Intel_GPU_Pipe_IRQ;
procedure Pipe_IRQ_Tests is
   Read_Steps : constant array (1 .. 6) of Natural := [2, 4, 6, 8, 9, 10];
   procedure Run (Item : Pipe; Fault : Natural; Corrupt : Boolean;
                  Mask_Value : Unsigned_32 := Unsigned_32'Last;
                  Flip_Bit : Natural := 0) is
      Step : Natural := 0;
      Offsets : constant array (1 .. 10) of Unsigned_32 := [4, 4, 12, 12, 8, 8, 8, 8, 4, 12];
      Reading : constant array (1 .. 10) of Boolean :=
        [False, True, False, True, False, True, False, True, True, True];
      Base : constant Unsigned_32 := 16#44400# + 16 * Pipe'Pos (Item);
      procedure Read (Offset : Unsigned_32; Value : out Unsigned_32; Success : out Boolean) is
      begin
         Step := Step + 1;
         pragma Assert (Step <= 10 and then Reading (Step));
         pragma Assert (Offset = Base + Offsets (Step));
         Value := (case Step is when 2 | 9 => Mask_Value,
                   when 6 => 1, when others => 0);
         Success := Step /= Fault or Corrupt;
         if Step = Fault and Corrupt then Value := Value xor Shift_Left (1, Flip_Bit); end if;
      end Read;
      procedure Write (Offset, Value : Unsigned_32; Success : out Boolean) is
      begin
         Step := Step + 1;
         pragma Assert (Step <= 10 and then not Reading (Step));
         pragma Assert (Offset = Base + Offsets (Step));
         pragma Assert (Value = (if Step = 3 then 0 else Unsigned_32'Last));
         Success := Step /= Fault;
      end Write;
      package IRQ is new Intel_GPU_Pipe_IRQ (Item, Read, Write);
      Status : IRQ.Result;
      Saved : Natural;
      Saved_State : IRQ.Phase;
      use type IRQ.Result;
      use type IRQ.Phase;
   begin
      for Owner in Boolean loop
         for Power in Boolean loop
            for Blocked in Boolean loop
               if not (Owner and Power and Blocked) then
                  IRQ.Quiesce (Owner, Power, Blocked, Status);
                  pragma Assert (Status = IRQ.Rejected and Step = 0 and IRQ.State = IRQ.Fresh);
               end if;
            end loop;
         end loop;
      end loop;
      IRQ.Quiesce (True, True, True, Status);
      if Fault = 0 or (Fault = 6 and Corrupt) or
        (Corrupt and Fault in 2 | 9 and Flip_Bit in 17 | 18) then
         pragma Assert (Status = IRQ.Complete and IRQ.State = IRQ.Masked and Step = 10);
      else
         pragma Assert (IRQ.State = IRQ.Uncertain and Step = Fault);
         pragma Assert (Status =
           (if not Reading (Fault) then IRQ.Write_Failed
            elsif not Corrupt then IRQ.Read_Failed
            elsif Fault = 8 then IRQ.Pending_Events else IRQ.Verify_Failed));
      end if;
      Saved := Step; Saved_State := IRQ.State;
      if Fault /= 0 and then Reading (Fault) then
         pragma Assert (IRQ.Last_Read_Offset = Base + Offsets (Step));
         if Corrupt and Fault /= 6 and Fault /= 8 and
           not (Fault in 2 | 9 and Flip_Bit in 17 | 18) then
            pragma Assert ((IRQ.Last_Read_Value and
              (if Fault in 2 | 9 then 16#FFF9FFFF# else Unsigned_32'Last)) =
              (IRQ.Expected_Value xor Shift_Left (1, Flip_Bit)));
         end if;
      end if;
      IRQ.Quiesce (True, True, True, Status);
      pragma Assert (Status = IRQ.Rejected and Step = Saved and IRQ.State = Saved_State);
   end Run;
begin
   for Item in Pipe loop
      for Fault in 0 .. 10 loop
         Run (Item, Fault, False);
      end loop;
      for Fault of Read_Steps loop
         Run (Item, Fault, True);
      end loop;
      for Reserved in Unsigned_32 range 0 .. 3 loop
         declare Mask : constant Unsigned_32 := 16#FFF9FFFF# or Shift_Left (Reserved, 17); begin
            Run (Item, 0, False, Mask);
            for Bit in 0 .. 31 loop
               Run (Item, 2, True, Mask, Bit);
               Run (Item, 9, True, Mask, Bit);
            end loop;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Pipe IRQ PASS: four pipes, exact sequence, admission, every callback failure, verification and reuse rejection");
end Pipe_IRQ_Tests;
