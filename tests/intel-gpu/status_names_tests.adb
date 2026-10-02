with Intel_GPU_ADLN_PAT;
with Intel_GPU_MOCS_Configure;
with Interfaces;
with Ada.Text_IO;
procedure Status_Names_Tests is
   function Owner return Boolean is (False);
   function Read32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      raise Program_Error;
      return Offset;
   end Read32;
   procedure Write32 (Offset, Value : Interfaces.Unsigned_32; OK : out Boolean) is
   begin
      raise Program_Error;
      OK := False;
   end Write32;
   package PAT is new Intel_GPU_ADLN_PAT (Owner, Read32, Write32);
   package MOCS is new Intel_GPU_MOCS_Configure (Owner, Read32, Write32);
begin
   pragma Assert (PAT.Result_Name (PAT.Rejected) = "REJECTED");
   pragma Assert (PAT.Result_Name (PAT.Ownership_Lost) = "OWNERSHIP-LOST");
   pragma Assert (PAT.Result_Name (PAT.Read_Failed) = "READ-FAILED");
   pragma Assert (PAT.Result_Name (PAT.Write_Failed) = "WRITE-FAILED");
   pragma Assert (PAT.Result_Name (PAT.Readback_Failed) = "READBACK-FAILED");
   pragma Assert (PAT.Result_Name (PAT.Ready) = "READY");
   for I in 0 .. 5 loop
      pragma Assert (PAT.Result_Name (PAT.Result'Val (I)) =
        MOCS.Result_Name (MOCS.Result'Val (I)));
   end loop;
   Ada.Text_IO.Put_Line ("PAT/MOCS explicit status names PASS: all twelve outcomes");
end Status_Names_Tests;
