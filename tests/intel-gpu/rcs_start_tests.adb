with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_RCS_Start;
procedure RCS_Start_Tests is
   Owner, Page : Boolean := True;
   Writes, Reads, Fail_Write, Lose_On : Natural := 0;
   Raw : Unsigned_32 := 0;
   type Words is array (Positive range 1 .. 4) of Unsigned_32;
   Offsets, Values : Words := [others => 0];
   function Owned return Boolean is (Owner);
   function Prepared (GPU : Unsigned_64) return Boolean is
     (Page and GPU = 16#200000#);
   procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
   begin
      Writes := Writes + 1;
      Offsets (Writes) := Offset; Values (Writes) := Value;
      Success := Writes /= Fail_Write;
      if Writes = Lose_On then Owner := False; end if;
   end Write32;
   function Read32 (Offset : Unsigned_32) return Unsigned_32 is
   begin
      pragma Assert (Offset = 16#209C# and Writes = 4);
      Reads := Reads + 1; return Raw;
   end Read32;
   package Start is new Intel_GPU_RCS_Start (Owned, Prepared, Write32, Read32);
   use type Start.Result;
   use type Start.Rejection_Reason;
   Status : Start.Result;
begin
   for Scenario in 0 .. 6 loop
      declare
         Attempt : Start.Attempt;
         Old_Writes, Old_Reads : Natural;
      begin
         Writes := 0; Reads := 0; Fail_Write := 0; Lose_On := 0;
         Owner := True; Page := True; Raw := 0;
         case Scenario is
            when 1 => Page := False;
            when 2 => Fail_Write := 2;
            when 3 => Lose_On := 3;
            when 4 => Raw := Unsigned_32'Last;
            when 5 => Raw := 16#100#;
            when 6 => Owner := False;
            when others => null;
         end case;
         Start.Start (Attempt, 16#200000#, Status);
         case Scenario is
            when 0 =>
               pragma Assert (Status = Start.Ready and Writes = 4 and Reads = 1);
               pragma Assert (Offsets = [16#2098#,16#2080#,16#229C#,16#209C#]);
               pragma Assert (Values = [16#FFFFFFFF#,16#200000#,16#80008#,16#1000000#]);
            when 1 | 6 => pragma Assert (Status = Start.Ownership_Lost and Writes = 0);
            when 2 => pragma Assert (Status = Start.Write_Failed and Writes = 2);
            when 3 => pragma Assert (Status = Start.Ownership_Lost and Writes = 3);
            when 4 | 5 => pragma Assert (Status = Start.Readback_Failed and Reads = 1);
         end case;
         Old_Writes := Writes; Old_Reads := Reads;
         pragma Assert (Start.Rejection (Attempt) = Start.None);
         Start.Start (Attempt, 16#200000#, Status);
         pragma Assert (Status = Start.Rejected and Writes = Old_Writes and Reads = Old_Reads);
         pragma Assert (Start.Rejection (Attempt) = Start.Already_Attempted);
      end;
   end loop;
   for GPU of Words'(0, 1, 16#FEE00000#, 16#FFFFFFFF#) loop
      declare Attempt : Start.Attempt; begin
         Writes := 0; Reads := 0;
         Start.Start (Attempt, Unsigned_64 (GPU), Status);
         pragma Assert (Status = Start.Rejected and Writes = 0 and Reads = 0);
         pragma Assert (Start.Rejection (Attempt) =
           (if GPU = 0 then Start.Zero_Address
            elsif GPU mod 4096 /= 0 then Start.Unaligned_Address
            else Start.Outside_Runtime_Range));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("RCS startup sequence PASS (mock MMIO, NOT hardware)");
end RCS_Start_Tests;
