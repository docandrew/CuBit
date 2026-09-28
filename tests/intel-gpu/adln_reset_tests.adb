with Interfaces; use Interfaces;
with Ada.Text_IO;
with Intel_GPU_ADLN_Reset;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
procedure ADLN_Reset_Tests is
   Cases : Natural := 0;
   procedure Run (Fuse : Unsigned_32; Stop_Failure, Prepare_Failure : Natural;
                  Fail_Reset, Fail_Cleanup : Boolean) is
      Selected : constant Inventory := Decode (16#8086#, 16#46D2#, Fuse);
      Stop_Fails : constant Boolean := Stop_Failure /= 0 and then
        Selected.Engines (Engine'Val (Stop_Failure - 1));
      Prepare_Fails : constant Boolean := Prepare_Failure /= 0 and then
        Selected.Engines (Engine'Val (Prepare_Failure - 1));
      Clock : Unsigned_64 := 0;
      Held : Boolean := False;
      Stopped, Prepared, Cancelled : Engine_Set := [others => False];
      Resets, Writes, Saved_Writes : Natural := 0;
      function Read (Offset : Unsigned_32) return Unsigned_32 is
      begin
         pragma Assert (Held);
         if Offset = 16#941C# then return (if Fail_Reset then 1 else 0); end if;
         for E in Engine loop
            if Offset = Engine_Base (E) + 16#9C# then
               pragma Assert (Selected.Engines (E));
               return (if Stop_Failure = Engine'Pos (E) + 1 then 0 else 16#200#);
            end if;
            if Offset = Pending_Register (E) then
               pragma Assert (Selected.Engines (E)); return 0;
            end if;
            if Offset = Engine_Base (E) + 16#D0# then
               pragma Assert (Selected.Engines (E));
               return (if Cancelled (E) then (if Fail_Cleanup then 1 else 0)
                       elsif Prepared (E) and Prepare_Failure /= Engine'Pos (E) + 1
                       then 3 else 0);
            end if;
         end loop;
         raise Program_Error with "unexpected read";
      end Read;
      procedure Write (Offset, Value : Unsigned_32) is
      begin
         pragma Assert (Held);
         Writes := Writes + 1;
         if Offset = 16#941C# then
            pragma Assert (not Stop_Fails and not Prepare_Fails);
            pragma Assert (Value = 1 and (for all E in Engine =>
              (not Selected.Engines (E) or Prepared (E))));
            Resets := Resets + 1; return;
         end if;
         pragma Assert (Engine_Write_Allowed (Selected, Offset, Value));
         for E in Engine loop
            if Offset = Engine_Base (E) + 16#9C# then Stopped (E) := True; end if;
            if Offset = Engine_Base (E) + 16#D0# then
               if Value = 16#10001# then
                  pragma Assert (not Stop_Fails);
                  pragma Assert ((for all Item in Engine =>
                    (not Selected.Engines (Item) or Stopped (Item))));
                  Prepared (E) := True;
               elsif Value = 16#10000# then Cancelled (E) := True;
               end if;
            end if;
         end loop;
      end Write;
      procedure Hold (Success : out Boolean) is
      begin Held := True; Success := True; end Hold;
      function Now return Unsigned_64 is (Clock);
      procedure Pause is
      begin Clock := Clock + 1; end Pause;
      package Reset is new Intel_GPU_ADLN_Reset (Read, Write, Hold, Now, Pause, 100);
      Status : Reset.Result;
      use type Reset.Result;
   begin
      Reset.Execute (0, 0, 0, Status);
      pragma Assert (Status = Reset.Rejected and not Held);
      Reset.Execute (16#8086#, 16#46D2#, Fuse, Status);
      pragma Assert (Status = (if Stop_Fails then Reset.Stop_Failed
                    elsif Fail_Cleanup then Reset.Cleanup_Failed
                    elsif Prepare_Fails then Reset.Prepare_Failed
                    elsif Fail_Reset then Reset.Reset_Failed else Reset.Complete));
      pragma Assert (Resets = (if Stop_Fails or Prepare_Fails then 0
                              elsif Fail_Reset then 1 else 2));
      if Stop_Fails then
         pragma Assert (Reset.Failure_Engine = Stop_Failure);
      elsif Fail_Cleanup then
         declare
            Last_Selected : Natural := 0;
         begin
            for E in Engine loop
               if Selected.Engines (E) then Last_Selected := Engine'Pos (E) + 1; end if;
            end loop;
            pragma Assert (Reset.Failure_Engine = Last_Selected);
         end;
      elsif Prepare_Fails then
         pragma Assert (Reset.Failure_Engine = Prepare_Failure);
      else
         pragma Assert (Reset.Failure_Engine = 0);
      end if;
      pragma Assert ((for all E in Engine =>
        Cancelled (E) = (Selected.Engines (E) and not Stop_Fails)));
      Saved_Writes := Writes;
      Reset.Execute (16#8086#, 16#46D2#, Fuse, Status);
      pragma Assert (Status = Reset.Rejected and Writes = Saved_Writes);
      Cases := Cases + 1;
   end Run;
begin
   for Mask in Unsigned_32 range 0 .. 7 loop
      for Stop_Failure in 0 .. 5 loop
         for Prepare_Failure in 0 .. 5 loop
            for Reset_Fails in Boolean loop
               for Cleanup_Fails in Boolean loop
                  Run ((Mask and 1) or Shift_Left (Mask and 2, 1) or
                       Shift_Left (Mask and 4, 14), Stop_Failure,
                       Prepare_Failure, Reset_Fails, Cleanup_Fails);
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   pragma Assert (Cases = 1152);
   Ada.Text_IO.Put_Line ("PASS: 1152 composed ADLN fuse/stop/prepare/reset/cleanup cases");
end ADLN_Reset_Tests;
