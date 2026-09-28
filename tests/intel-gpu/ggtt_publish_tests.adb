with Interfaces; use Interfaces;
with Intel_GPU_GGTT_Publish;
procedure GGTT_Publish_Tests is
   Table : array (0 .. 511) of Unsigned_64 := [others => 0];
   Reads, Writes, Flushes, Prepares : Natural := 0;
   Fail_Read, Fail_Write : Natural := 0;
   Prepare_OK, Flush_OK, Corrupt_Write : Boolean := True;
   procedure Prepare (Success : out Boolean) is
   begin Prepares := Prepares + 1; Success := Prepare_OK; end Prepare;
   procedure Read_Entry (Index : Unsigned_64; Value : out Unsigned_64; Success : out Boolean) is
   begin
      Reads := Reads + 1;
      Value := Table (Natural (Index));
      Success := Reads /= Fail_Read;
   end Read_Entry;
   procedure Write_Entry (Index, Value : Unsigned_64; Success : out Boolean) is
   begin
      pragma Assert (Reads >= 4 and Prepares = 1);
      Writes := Writes + 1;
      -- Failure may occur AFTER a store reached the device.
      Table (Natural (Index)) := (if Corrupt_Write then 0 else Value);
      Success := Writes /= Fail_Write;
   end Write_Entry;
   procedure Flush (Success : out Boolean) is
   begin
      pragma Assert (Writes = 4 and Reads = 8);
      Flushes := Flushes + 1; Success := Flush_OK;
   end Flush;
   package Transaction is new Intel_GPU_GGTT_Publish
     (Prepare, Read_Entry, Write_Entry, Flush);
   use Transaction;
   Status : Result;
   -- Each scenario owns one noncopyable attempt. Repeat with both identical
   -- and changed addresses must reject without invoking any hardware callback.
   procedure Publish
     (Table_Bytes, GPU_Start, DMA_Start, Bytes : Unsigned_64; Status : out Result)
   is
      Object : Attempt;
      Again : Result;
      Saved_Reads, Saved_Writes, Saved_Prepares, Saved_Flushes : Natural;
      Saved_Phase : Phase;
   begin
      pragma Assert (Current (Object) = Fresh);
      Transaction.Publish (Object, Table_Bytes, GPU_Start, DMA_Start, Bytes, Status);
      Saved_Phase := Current (Object);
      pragma Assert (Saved_Phase =
        (if Status = Published then Complete
         elsif Status = Quarantined then Possibly_Published else Consumed_No_Writes));
      Saved_Reads := Reads; Saved_Writes := Writes;
      Saved_Prepares := Prepares; Saved_Flushes := Flushes;
      for Changed in Boolean loop
         Transaction.Publish (Object, Table_Bytes,
           (if Changed then 0 else GPU_Start), DMA_Start, Bytes, Again);
         pragma Assert (Again = Rejected and Current (Object) = Saved_Phase);
         pragma Assert (Reads = Saved_Reads and Writes = Saved_Writes and
           Prepares = Saved_Prepares and Flushes = Saved_Flushes);
      end loop;
   end Publish;
   procedure Reset is
   begin
      Table := [others => 0]; Reads := 0; Writes := 0;
      Flushes := 0; Prepares := 0; Fail_Read := 0; Fail_Write := 0;
      Prepare_OK := True; Flush_OK := True; Corrupt_Write := False;
   end Reset;
   procedure Run is
   begin Publish (4096, 4096, 16#100000#, 4 * 4096, Status); end Run;
begin
   Reset; Run;
   pragma Assert (Status = Published and Flushes = 1);
   pragma Assert (Table (0) = 0 and Table (5) = 0);
   for I in 1 .. 4 loop
      pragma Assert (Table (I) = 16#100001# + Unsigned_64 (I - 1) * 4096);
   end loop;
   for Position in 1 .. 4 loop
      Reset; Table (Position) := 2; Run; -- even non-present nonzero is occupied
      pragma Assert (Status = Occupied and Writes = 0 and Prepares = 0);
      Reset; Fail_Read := Position; Run;
      pragma Assert (Status = Read_Failed and Writes = 0);
      Reset; Fail_Write := Position; Run;
      pragma Assert (Status = Quarantined and Writes = Position and Flushes = 0);
      Reset; Fail_Read := 4 + Position; Run;
      pragma Assert (Status = Quarantined and Writes = 4 and Flushes = 0);
   end loop;
   Reset; Prepare_OK := False; Run;
   pragma Assert (Status = Prepare_Failed and Writes = 0);
   Reset; Flush_OK := False; Run;
   pragma Assert (Status = Quarantined and Flushes = 1);
   Reset; Corrupt_Write := True; Run;
   pragma Assert (Status = Quarantined and Flushes = 0);
   Reset; Publish (4096, 4096, 2 ** 32 - 4096, 8192, Status);
   pragma Assert (Status = Rejected and Reads = 0 and Writes = 0);
   Reset; Publish (4096, 4096, 16#100000#, 4097, Status);
   pragma Assert (Status = Rejected and Reads = 0 and Writes = 0);
   Reset; Publish (4096, 511 * 4096, 16#100000#, 8192, Status);
   pragma Assert (Status = Rejected and Reads = 0 and Writes = 0);
   Reset; Publish (4096, 4096, 2 ** 32 - 4 * 4096, 4 * 4096, Status);
   pragma Assert (Status = Published and Table (4) = 16#FFFF_F001#);
   Reset; Publish (4096, 4096, 16#100001#, 4096, Status);
   pragma Assert (Status = Rejected and Reads = 0);
   Reset; Publish (4096, 4096, 16#100000#, 1024 * 1024 + 4096, Status);
   pragma Assert (Status = Rejected and Reads = 0);
   Reset; Publish (4096, 4096, 16#100000#, 0, Status);
   pragma Assert (Status = Rejected and Reads = 0);
end GGTT_Publish_Tests;
