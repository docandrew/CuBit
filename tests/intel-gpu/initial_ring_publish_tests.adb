with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_Initial_Ring_Publish;
procedure Initial_Ring_Publish_Tests is
   type Memory is array (Natural range 0 .. 16479) of Unsigned_32;
   RAM : Memory := [others => 0];
   Owner : Boolean := True;
   Step, Fail_At, Lose_At, Corrupt_At : Natural := 0;
   Ring_Visible : Boolean := False;
   function Owned return Boolean is (Owner);
   procedure Tick is
   begin
      Step := Step + 1;
      if Step = Lose_At then Owner := False; end if;
   end Tick;
   procedure Store (Offset, Value : Unsigned_32; OK : out Boolean) is
   begin
      Tick;
      if Offset = 4124 then
         pragma Assert (Ring_Visible and Value = 384 and Step = 196);
      else
         pragma Assert (Step in 3 .. 98 and
                        Offset = 65536 + Unsigned_32 (Step - 3) * 4);
      end if;
      RAM (Natural (Offset / 4)) := Value;
      OK := Step /= Fail_At;
   end Store;
   procedure Load (Offset : Unsigned_32; Value : out Unsigned_32;
                   OK : out Boolean) is
   begin
      Tick;
      case Step is
         when 1 => pragma Assert (Offset = 4116);
         when 2 | 197 => pragma Assert (Offset = 4124);
         when others =>
            pragma Assert (Step in 99 .. 194 and
              Offset = 65536 + Unsigned_32 (Step - 99) * 4);
      end case;
      Value := RAM (Natural (Offset / 4));
      if Step = Corrupt_At then Value := Value xor 1; end if;
      OK := Step /= Fail_At;
   end Load;
   function Flush (Offset, Bytes : Unsigned_32) return Boolean is
   begin
      Tick;
      if Step = 195 then
         pragma Assert (Offset = 65536 and Bytes = 384);
         Ring_Visible := True;
      else
         pragma Assert (Step = 198 and Offset = 4124 and Bytes = 4);
      end if;
      return Step /= Fail_At;
   end Flush;
   package Publisher is new Intel_GPU_Initial_Ring_Publish
     (Owned, Store, Load, Flush);
   use type Publisher.Result;
   Segment : constant Intel_GPU_ADLN_Context_Init.Segment :=
     Intel_GPU_ADLN_Context_Init.Build (True, 16#12345678#);
   Status : Publisher.Result;
begin
   for Fault in 0 .. 198 loop
      for Kind in 0 .. 2 loop
         if Kind /= 2 or else Fault in 0 .. 2 | 99 .. 194 | 197 then
            declare
               Attempt : Publisher.Attempt;
               Last : Natural;
            begin
               RAM := [others => 0]; Owner := True; Step := 0;
               Ring_Visible := False; Fail_At := 0; Lose_At := 0;
               Corrupt_At := 0;
               case Kind is
                  when 0 => Fail_At := Fault;
                  when 1 => Lose_At := Fault;
                  when others => Corrupt_At := Fault;
               end case;
               Publisher.Publish (Attempt, Segment, Status);
               if Fault = 0 then
                  pragma Assert (Status = Publisher.Published and Step = 198);
                  for I in Segment.Words'Range loop
                     pragma Assert (RAM (16384 + I) = Segment.Words (I));
                  end loop;
               else
                  pragma Assert (Step = Fault and Status /= Publisher.Published);
                  if Kind = 1 then
                     pragma Assert (Status = Publisher.Ownership_Lost);
                  elsif Kind = 2 then
                     pragma Assert (Status = Publisher.Verify_Failed);
                  elsif Fault in 1 .. 2 | 99 .. 194 | 197 then
                     pragma Assert (Status = Publisher.Read_Failed);
                  elsif Fault in 195 | 198 then
                     pragma Assert (Status = Publisher.Flush_Failed);
                  else
                     pragma Assert (Status = Publisher.Write_Failed);
                  end if;
                  if Fault < 196 then pragma Assert (RAM (1031) = 0); end if;
               end if;
               Last := Step;
               Publisher.Publish (Attempt, Segment, Status);
               pragma Assert (Status = Publisher.Rejected and Step = Last);
            end;
         end if;
      end loop;
   end loop;
   declare
      Attempt : Publisher.Attempt;
      Invalid : Intel_GPU_ADLN_Context_Init.Segment;
   begin
      Step := 0;
      Publisher.Publish (Attempt, Invalid, Status);
      pragma Assert (Status = Publisher.Rejected and Step = 0);
      Owner := False;
      Publisher.Publish (Attempt, Segment, Status);
      pragma Assert (Status = Publisher.Ownership_Lost and Step = 0);
   end;
   Ada.Text_IO.Put_Line ("Initial ring publication PASS: ordering, every callback failure/ownership loss, readback corruption, no retry");
end Initial_Ring_Publish_Tests;
