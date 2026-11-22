with Interfaces; with Ada.Text_IO; with Intel_GPU_ADLN_PAT;
with Intel_GPU_PAT_Registers;
procedure ADLN_PAT_Tests is
   use Interfaces;
   Expected : constant array (Natural range 0 .. 7) of Unsigned_32 := [3,1,2,0,3,3,3,3];
   Registers : array (Natural range 0 .. 7) of Unsigned_32 := [others => 99];
   Reads, Writes, Mode, Fault : Natural := 0;
   Owner : Boolean := True;
   function Owned return Boolean is (Owner);
   function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      Index : Natural;
   begin
      pragma Assert (Offset in 16#4800# .. 16#481C# and Offset mod 4 = 0);
      Index := Natural ((Offset - 16#4800#) / 4);
      Reads := Reads + 1;
      if Reads = Fault then
         if Mode = 1 then return Unsigned_32'Last;
         elsif Mode = 2 then Owner := False;
         end if;
      end if;
      return Registers (Index);
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
      Index : constant Natural := Natural ((Offset - 16#4800#) / 4);
   begin
      pragma Assert (Offset in 16#4800# .. 16#481C# and Offset mod 4 = 0);
      pragma Assert (Value = Expected (Index));
      Writes := Writes + 1;
      Success := not (Mode = 3 and Writes = Fault);
      if Success and not (Mode = 4 and Writes = Fault) then Registers (Index) := Value; end if;
   end Write32;
   package PAT is new Intel_GPU_ADLN_PAT (Owned, Read32, Write32);
   use PAT;
   Status : Result;
begin
   declare
      use Intel_GPU_PAT_Registers;
      R : PAT_Register;
   begin
      for Kind in Memory_Type loop
         R := (Cache => Kind, others => <>);
         pragma Assert (Encode (R) = Unsigned_32 (Memory_Type'Pos (Kind)));
         pragma Assert (Matches (Encode (R), Kind));
         for Bit in 2 .. 31 loop
            pragma Assert (not Matches (Encode (R) or Shift_Left (Unsigned_32'(1), Bit), Kind));
         end loop;
      end loop;
      for Bit in 0 .. 31 loop
         R := Decode (Shift_Left (Unsigned_32'(1), Bit));
         pragma Assert (Encode (R) = Shift_Left (Unsigned_32'(1), Bit));
      end loop;
   end;
   for M in 0 .. 4 loop
      Mode := M;
      for F in 1 .. (if M in 1 .. 2 then 16 else 8) loop
         declare Object : Attempt; Before : Natural; begin
            Registers := [others => 99]; Reads := 0; Writes := 0; Owner := True; Fault := F;
            Configure (Object, Status);
            pragma Assert (Status = (case M is
              when 0 => Ready, when 1 => (if F mod 2 = 1 then Read_Failed else Readback_Failed),
              when 2 => Ownership_Lost, when 3 => Write_Failed, when others => Readback_Failed));
            if M = 0 then
               pragma Assert (Reads = 16 and Writes = 8 and Last_Index (Object) = 7 and Last_Raw (Object) = 3);
            end if;
            Before := Reads; Owner := True;
            Configure (Object, Status);
            pragma Assert (Status = Rejected and Reads = Before);
         end;
      end loop;
   end loop;
   declare Object : Attempt; begin
      Mode := 0; Owner := False; Reads := 0; Writes := 0;
      Configure (Object, Status); pragma Assert (Status = Rejected and Reads = 0);
      Owner := True;
      for I in Registers'Range loop Registers (I) := Expected (I); end loop;
      Configure (Object, Status); pragma Assert (Status = Ready and Writes = 0 and Reads = 16);
   end;
   Ada.Text_IO.Put_Line ("ADLN PAT PASS: exact register policy, partial faults, owner loss, one attempt (NOT hardware)");
end ADLN_PAT_Tests;
