with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Cursor_Decode;
with Intel_GPU_Cursor_Collect;
procedure Cursor_Collect_Tests is
   Held, Begin_OK, End_OK, Sentinel, Changing : Boolean;
   Reads, Ends, Fail_Read : Natural;
   procedure Begin_Access (Success : out Boolean) is
   begin pragma Assert (not Held); Success := Begin_OK; Held := Success; end Begin_Access;
   procedure End_Access (Success : out Boolean) is
   begin pragma Assert (Held); Ends := Ends + 1; Held := False; Success := End_OK; end End_Access;
   procedure Read_Field (Index : Natural; Value : out Unsigned_32; Success : out Boolean) is
   begin
      pragma Assert (Held and Index = Reads mod 4);
      Reads := Reads + 1; Success := Reads /= Fail_Read;
      Value := (case Index is when 0 => 16#27#, when 1 | 2 => 4096, when others => 0);
      if Sentinel and Reads = Fail_Read then Value := Unsigned_32'Last; Success := True; end if;
      if Changing and Reads = 6 then Value := 8192; end if;
   end Read_Field;
   package Collector is new Intel_GPU_Cursor_Collect (Begin_Access, End_Access, Read_Field);
   use Collector;
   use type Intel_GPU_Cursor_Decode.Status;
   use type Intel_GPU_Cursor_Decode.Sample;
   R : Observation;
begin
   for Begin_Succeeds in Boolean loop
      for End_Succeeds in Boolean loop
         for Use_Sentinel in Boolean loop
            for Failure in 0 .. 8 loop
               Held := False; Reads := 0; Ends := 0; Changing := False;
               Begin_OK := Begin_Succeeds; End_OK := End_Succeeds;
               Sentinel := Use_Sentinel; Fail_Read := Failure;
               Inspect (8_388_608, R);
               pragma Assert (not Held);
               if not Begin_OK then
                  pragma Assert (R.State = Access_Unavailable and Reads = 0 and Ends = 0);
               else
                  pragma Assert (Ends = 1 and Reads = (if Failure = 0 then 8 else Failure));
                  if not End_OK then
                     pragma Assert (R.State = Access_End_Failed);
                  elsif Failure /= 0 then
                     pragma Assert (R.State = Read_Failed and R.Reads = Failure - 1);
                  else
                     pragma Assert (R.State = Collected and R.Reads = 8);
                     pragma Assert (R.Decoded.State = Intel_GPU_Cursor_Decode.Ready);
                     pragma Assert (R.Decoded.Memory.Bytes = 16_384);
                  end if;
               end if;
               if R.State /= Collected then
                  pragma Assert (not R.Decoded.Memory.Valid);
                  pragma Assert (R.Before = Intel_GPU_Cursor_Decode.Sample'(others => 0));
                  pragma Assert (R.After = Intel_GPU_Cursor_Decode.Sample'(others => 0));
               end if;
            end loop;
         end loop;
      end loop;
   end loop;
   Held := False; Reads := 0; Ends := 0; Changing := True;
   Begin_OK := True; End_OK := True; Sentinel := False; Fail_Read := 0;
   Inspect (8_388_608, R);
   pragma Assert (R.State = Collected and Ends = 1 and R.Reads = 8);
   pragma Assert (R.Decoded.State = Intel_GPU_Cursor_Decode.Changing and not R.Decoded.Memory.Valid);
   Ada.Text_IO.Put_Line ("cursor collection PASS: 72 failure combinations and changing snapshot");
end Cursor_Collect_Tests;
