with Interfaces; use Interfaces;
with Intel_GPU_Firmware;
with Intel_GPU_GuC_RSA;
with Ada.Text_IO;
procedure GuC_RSA_Tests is
   Header : Intel_GPU_Firmware.CSS_Header := [others => 0];
   procedure Put (Offset : Natural; Value : Unsigned_32) is
   begin
      for Byte in 0 .. 3 loop
         Header (Offset + Byte) := Unsigned_8 (Shift_Right (Value, Byte * 8) and 255);
      end loop;
   end Put;
   procedure Run (Bad_Read : Natural := 0; Bad_Write : Natural := 0;
                  Blob_Bytes : Unsigned_64 := 335360;
                  Valid : Boolean := True) is
      Reads, Writes : Natural := 0;
      procedure Read_Byte (Offset : Unsigned_64; Value : out Unsigned_8;
                           Success : out Boolean) is
      begin
         pragma Assert (Writes = 0);
         pragma Assert (Offset = 335104 + Unsigned_64 (Reads));
         Value := Unsigned_8 (Reads);
         Reads := Reads + 1;
         Success := Reads /= Bad_Read;
      end Read_Byte;
      procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
         Base : constant Unsigned_32 := Unsigned_32 (Writes * 4);
      begin
         pragma Assert (Reads = 256);
         pragma Assert (Offset = 16#C200# + Base);
         pragma Assert (Value = (Base or Shift_Left (Base + 1, 8) or
           Shift_Left (Base + 2, 16) or Shift_Left (Base + 3, 24)));
         Writes := Writes + 1;
         Success := Writes /= Bad_Write;
      end Write32;
      package RSA is new Intel_GPU_GuC_RSA (Read_Byte, Write32);
      use type RSA.Result;
      use type RSA.Phase;
      Object : RSA.Attempt;
      Status : RSA.Result;
      Saved : Natural;
      Saved_Phase : RSA.Phase;
   begin
      RSA.Execute (Object, Header, Blob_Bytes, Status);
      pragma Assert (Status =
        (if not Valid then RSA.Rejected elsif Bad_Read /= 0 then RSA.Source_Failed
         elsif Bad_Write /= 0 then RSA.Write_Failed else RSA.Complete));
      pragma Assert (Reads = (if not Valid then 0 elsif Bad_Read /= 0 then Bad_Read else 256));
      pragma Assert (Writes = (if not Valid or Bad_Read /= 0 then 0
                               elsif Bad_Write /= 0 then Bad_Write else 64));
      Saved_Phase := RSA.Current (Object);
      pragma Assert (Saved_Phase =
        (if Writes = 0 then RSA.Consumed elsif Bad_Write /= 0 then RSA.Quarantined
         else RSA.Supplied));
      Saved := Reads + Writes;
      RSA.Execute (Object, Header, Blob_Bytes, Status);
      pragma Assert (Status = RSA.Rejected and Saved = Reads + Writes);
      pragma Assert (RSA.Current (Object) = Saved_Phase);
   end Run;
begin
   Put (0, 6); Put (4, 161); Put (8, 16#10000#);
   Put (16, 16#8086#); Put (24, 16#147C1#);
   Put (28, 64); Put (32, 64); Put (36, 1);
   Put (64, 16#463104#); Put (120, 16#801000#);
   Run;
   for Index in 1 .. 256 loop Run (Bad_Read => Index); end loop;
   for Index in 1 .. 64 loop Run (Bad_Write => Index); end loop;
   Run (Blob_Bytes => 335359, Valid => False);
   Run (Blob_Bytes => Unsigned_64'Last, Valid => False);
   Put (28, 96);
   Run (Valid => False);
   Ada.Text_IO.Put_Line ("PASS: GuC RSA snapshot, byte order, each failed byte/write and no retry (324 cases)");
end GuC_RSA_Tests;
