with Interfaces; use Interfaces;
with Intel_GPU_GGTT_Publish;
with Intel_GPU_GGTT_Reservations;
procedure GGTT_Publish_Tests is
   Table : array (0 .. 4096) of Unsigned_64 := [others => 0];
   Expected_Pages : Natural := 4;
   Search_Reads : Natural := 0;
   Reads, Writes, Flushes, Prepares : Natural := 0;
   Fail_Read, Fail_Write : Natural := 0;
   Change_On_Read : Natural := 0;
   Prepare_OK, Flush_OK, Corrupt_Write : Boolean := True;
   Prepared_Start, Prepared_Bytes : Unsigned_64 := 0;
   Allow_Range : Boolean := True;
   Revoke_In_Prepare : Boolean := False;
   Protected_First, Protected_Bytes : Unsigned_64 := 0;
   function Range_Allowed (First, Bytes : Unsigned_64) return Boolean is
     (Allow_Range and then (Protected_Bytes = 0 or else
        First + Bytes <= Protected_First or else
        First >= Protected_First + Protected_Bytes));
   procedure Prepare (GPU_Start, Bytes : Unsigned_64; Success : out Boolean) is
   begin
      pragma Assert (Reads = Search_Reads + Expected_Pages and Writes = 0 and Flushes = 0);
      pragma Assert (Bytes = Unsigned_64 (Expected_Pages) * 4096);
      Prepared_Start := GPU_Start;
      Prepared_Bytes := Bytes;
      Prepares := Prepares + 1;
      Success := Prepare_OK;
      if Revoke_In_Prepare then Allow_Range := False; end if;
   end Prepare;
   procedure Read_Entry (Index : Unsigned_64; Value : out Unsigned_64; Success : out Boolean) is
   begin
      Reads := Reads + 1;
      if Reads = Change_On_Read then Table (Natural (Index)) := 1; end if;
      Value := Table (Natural (Index));
      Success := Reads /= Fail_Read;
   end Read_Entry;
   procedure Write_Entry (Index, Value : Unsigned_64; Success : out Boolean) is
   begin
      pragma Assert (Reads >= Expected_Pages and Prepares = 1);
      Writes := Writes + 1;
      -- Failure may occur AFTER a store reached the device.
      Table (Natural (Index)) := (if Corrupt_Write then 0 else Value);
      Success := Writes /= Fail_Write;
   end Write_Entry;
   procedure Flush (Success : out Boolean) is
   begin
      pragma Assert (Writes = Expected_Pages and Reads = Search_Reads + 2 * Expected_Pages);
      Flushes := Flushes + 1; Success := Flush_OK;
   end Flush;
   package Transaction is new Intel_GPU_GGTT_Publish
     (Range_Allowed, Prepare, Read_Entry, Write_Entry, Flush);
   package ADS_Transaction is new Intel_GPU_GGTT_Publish
     (Range_Allowed, Prepare, Read_Entry, Write_Entry, Flush, 16 * 1024 * 1024);
   use Transaction;
   Status : Result;
   -- Each scenario owns one noncopyable attempt. Repeat with both identical
   -- and changed addresses must reject without invoking any hardware callback.
   procedure Publish
     (Table_Bytes, GPU_Start, DMA_Start, Bytes : Unsigned_64; Status : out Result)
   is
      Object : Attempt;
      Reservations : Intel_GPU_GGTT_Reservations.Ledger;
      Admitted : Boolean;
      Again : Result;
      Saved_Reads, Saved_Writes, Saved_Prepares, Saved_Flushes : Natural;
      Saved_Phase : Phase;
   begin
      pragma Assert (Current (Object) = Fresh);
      Intel_GPU_GGTT_Reservations.Admit
        (Reservations, Table_Bytes, 0, Table_Bytes / 8 * 4096, Admitted);
      pragma Assert (Admitted);
      Transaction.Publish (Object, Reservations, GPU_Start, DMA_Start, Bytes, Status);
      if Prepares /= 0 then
         pragma Assert (Prepared_Start = GPU_Start and Prepared_Bytes = Bytes);
      end if;
      Saved_Phase := Current (Object);
      pragma Assert (Saved_Phase =
        (if Status = Published then Complete
         elsif Status = Quarantined then Possibly_Published else Consumed_No_Writes));
      Saved_Reads := Reads; Saved_Writes := Writes;
      Saved_Prepares := Prepares; Saved_Flushes := Flushes;
      for Changed in Boolean loop
         Transaction.Publish (Object, Reservations,
           (if Changed then 0 else GPU_Start), DMA_Start, Bytes, Again);
         pragma Assert (Again = Rejected and Current (Object) = Saved_Phase);
         pragma Assert (Reads = Saved_Reads and Writes = Saved_Writes and
           Prepares = Saved_Prepares and Flushes = Saved_Flushes);
      end loop;
      -- New attempts with different backing cannot reuse an acquired range,
      -- even after preparation/read failure or an ambiguous first store.
      if Intel_GPU_GGTT_Reservations.Count (Reservations) = 1 then
         declare
            New_Attempt : Attempt;
         begin
            Transaction.Publish (New_Attempt, Reservations,
              GPU_Start, 16#200000#, Bytes, Again);
            pragma Assert (Again = (if Allow_Range then Reservation_Failed else Protected_Range));
            pragma Assert (Reads = Saved_Reads and Writes = Saved_Writes and
              Prepares = Saved_Prepares and Flushes = Saved_Flushes);
         end;
      end if;
   end Publish;
   procedure Reset is
   begin
      Table := [others => 0]; Reads := 0; Writes := 0;
      Change_On_Read := 0;
      Flushes := 0; Prepares := 0; Fail_Read := 0; Fail_Write := 0;
      Prepare_OK := True; Flush_OK := True; Corrupt_Write := False;
      Prepared_Start := 0; Prepared_Bytes := 0;
      Allow_Range := True; Revoke_In_Prepare := False;
      Protected_First := 0; Protected_Bytes := 0;
   end Reset;
   procedure Run is
   begin Publish (4096, 4096, 16#100000#, 4 * 4096, Status); end Run;
begin
   for Fault in 0 .. 3 loop
      Reset;
      Expected_Pages := 4096;
      declare
         Object : ADS_Transaction.Attempt;
         Reservations : Intel_GPU_GGTT_Reservations.Ledger;
         OK : Boolean;
         Outcome : ADS_Transaction.Result;
         use type ADS_Transaction.Result;
         use type ADS_Transaction.Phase;
      begin
         Intel_GPU_GGTT_Reservations.Admit
           (Reservations, 16#10000#, 4096, 16 * 1024 * 1024, OK);
         pragma Assert (OK);
         if Fault = 1 then Fail_Read := 4096;
         elsif Fault = 2 then Fail_Write := 4096;
         elsif Fault = 3 then Fail_Read := 8192;
         end if;
         ADS_Transaction.Publish (Object, Reservations, 4096,
           16#2000000#, 16 * 1024 * 1024, Outcome);
         pragma Assert (Intel_GPU_GGTT_Reservations.Count (Reservations) = 1);
         if Fault = 0 then
            pragma Assert (Outcome = ADS_Transaction.Published and Flushes = 1);
            pragma Assert (Prepared_Start = 4096 and Prepared_Bytes = 16 * 1024 * 1024);
            for I in 1 .. 4096 loop
               pragma Assert (Table (I) = 16#2000001# + Unsigned_64 (I - 1) * 4096);
            end loop;
         elsif Fault = 1 then
            pragma Assert (Outcome = ADS_Transaction.Read_Failed and Writes = 0);
         else
            pragma Assert (Outcome = ADS_Transaction.Quarantined and Flushes = 0);
            pragma Assert (ADS_Transaction.Current (Object) = ADS_Transaction.Possibly_Published);
         end if;
         pragma Assert (Table (0) = 0);
      end;
   end loop;
   Expected_Pages := 4;
   Reset; Allow_Range := False; Run;
   pragma Assert (Status = Protected_Range and Reads = 0 and Writes = 0 and Prepares = 0);
   Reset; Revoke_In_Prepare := True; Run;
   pragma Assert (Status = Protected_Range and Reads = 4 and Writes = 0 and Prepares = 1 and Flushes = 0);
   Reset;
   declare
      Object : Attempt;
      Reservations : Intel_GPU_GGTT_Reservations.Ledger;
      OK : Boolean;
      Address : Unsigned_64;
   begin
      Intel_GPU_GGTT_Reservations.Admit (Reservations, 2_097_152, 4096, 8 * 4096, OK);
      pragma Assert (OK);
      Protected_First := 4096; Protected_Bytes := 4 * 4096;
      Search_Reads := 0;
      Publish_Available (Object, Reservations, 16#100000#, 4 * 4096, 4096, Address, Status);
      pragma Assert (Status = Published and Address = 5 * 4096);
      pragma Assert (Search_Name (Not_Searched) = "NOT-SEARCHED" and
        Search_Name (Search_Rejected) = "REJECTED" and
        Search_Name (Search_Read_Failed) = "READ-FAILED" and
        Search_Name (Search_Exhausted) = "EXHAUSTED" and
        Search_Name (Search_Found) = "FOUND");
      pragma Assert (Search_Detail (Object).Outcome = Search_Found and
        Search_Detail (Object).Blocked = 4 and Search_Detail (Object).Reads = 4 and
        Search_Detail (Object).Nonzero = 0);
      pragma Assert (Reads = 8 and Writes = 4);
      for I in 1 .. 4 loop pragma Assert (Table (I) = 0); end loop;
   end;
   for Failed_Store in Boolean loop
      Reset;
      Search_Reads := 0;
      declare
         First_Attempt, Second_Attempt, No_Space : Attempt;
         Reservations : Intel_GPU_GGTT_Reservations.Ledger;
         OK : Boolean;
         Claim : Intel_GPU_GGTT_Reservations.Result;
         Address : Unsigned_64;
         Saved : Natural;
         use type Intel_GPU_GGTT_Reservations.Result;
      begin
         Intel_GPU_GGTT_Reservations.Admit (Reservations, 2_097_152, 4096, 31 * 4096, OK);
         pragma Assert (OK);
         Intel_GPU_GGTT_Reservations.Reserve (Reservations, 4096, 4 * 4096, Claim);
         pragma Assert (Claim = Intel_GPU_GGTT_Reservations.Reserved);
         if Failed_Store then Fail_Write := 1; end if;
         Publish_Available (First_Attempt, Reservations, 16#100000#, 4 * 4096,
                            8 * 4096, Address, Status);
         pragma Assert (Address = 8 * 4096);
         pragma Assert (Prepared_Start = Address and Prepared_Bytes = 4 * 4096);
         pragma Assert (Status = (if Failed_Store then Quarantined else Published));
         pragma Assert (Intel_GPU_GGTT_Reservations.Count (Reservations) = 2);
         Saved := Reads + Writes + Prepares + Flushes;
         Publish_Available (First_Attempt, Reservations, 16#200000#, 4 * 4096,
                            4096, Address, Status);
         pragma Assert (Status = Rejected and Saved = Reads + Writes + Prepares + Flushes);
         -- Even if every PTE is zero again, the retained ledger claim must
         -- exclude the previous address, including an ambiguous first store.
         Reset;
         Publish_Available (Second_Attempt, Reservations, 16#200000#, 4 * 4096,
                            8 * 4096, Address, Status);
         pragma Assert (Status = Published and Address = 16 * 4096);
         pragma Assert (Prepared_Start = Address and Prepared_Bytes = 4 * 4096);
         Saved := Reads + Writes + Prepares + Flushes;
         Publish_Available (No_Space, Reservations, 16#300000#, 32 * 4096,
                            4096, Address, Status);
         pragma Assert (Status = Reservation_Failed and Current (No_Space) = Consumed_No_Writes);
         Publish_Available (No_Space, Reservations, 16#300000#, 4 * 4096,
                            4096, Address, Status);
         pragma Assert (Status = Rejected and Saved = Reads + Writes + Prepares + Flushes);
      end;
   end loop;
   Reset;
   Search_Reads := 0;
   declare
      Object : Attempt;
      Reservations : Intel_GPU_GGTT_Reservations.Ledger;
      OK : Boolean;
      Address : Unsigned_64;
   begin
      Intel_GPU_GGTT_Reservations.Admit (Reservations, 2_097_152, 8 * 4096, 24 * 4096, OK);
      pragma Assert (OK);
      Table (8) := 1;
      Publish_Available (Object, Reservations, 16#100000#, 4 * 4096,
                         8 * 4096, Address, Status);
      pragma Assert (Status = Published and Address = 8 * 4096 and Table (8) = 16#100001#);
      pragma Assert (Search_Detail (Object).Nonzero = 1);
      pragma Assert (Intel_GPU_GGTT_Reservations.Count (Reservations) = 1);
   end;
   Search_Reads := 0;
   -- Search and final preflight failures have different retention outcomes.
   -- No failure is allowed to reach preparation, PTE writes or invalidation.
   for Failure in 1 .. 6 loop
      Reset;
      declare
         Object : Attempt;
         Reservations : Intel_GPU_GGTT_Reservations.Ledger;
         OK : Boolean;
         Address : Unsigned_64;
         Saved : Natural;
      begin
         Intel_GPU_GGTT_Reservations.Admit (Reservations, 2_097_152, 8 * 4096, 24 * 4096, OK);
         pragma Assert (OK);
         if Failure <= 4 then Fail_Read := Failure;
         elsif Failure = 5 then Table (8) := Unsigned_64'Last;
         end if;
         Publish_Available (Object, Reservations,
           (if Failure = 6 then 1 else 16#100000#), 4 * 4096,
           8 * 4096, Address, Status);
         pragma Assert (Status =
           (if Failure = 6 then Rejected else Read_Failed));
         pragma Assert (Prepares = 0 and Writes = 0 and Flushes = 0);
         pragma Assert (Current (Object) = Consumed_No_Writes);
         pragma Assert (Intel_GPU_GGTT_Reservations.Count (Reservations) =
           (if Failure < 6 then 1 else 0));
         pragma Assert (Address = (if Failure < 6 then 8 * 4096 else 0));
         pragma Assert (Reads =
           (if Failure <= 4 then Failure elsif Failure = 5 then 1 else 0));
         Saved := Reads;
         Publish_Available (Object, Reservations, 16#200000#, 4 * 4096,
                            4096, Address, Status);
         pragma Assert (Status = Rejected and Reads = Saved and Address = 0);
         pragma Assert (Prepares = 0 and Writes = 0 and Flushes = 0);
      end;
   end loop;
   Reset;
   declare
      Unowned, Outside : Attempt;
      Reservations : Intel_GPU_GGTT_Reservations.Ledger;
      OK : Boolean;
   begin
      Transaction.Publish (Unowned, Reservations, 4096, 16#100000#, 16384, Status);
      pragma Assert (Status = Rejected and Reads = 0 and Writes = 0 and Prepares = 0);
      Intel_GPU_GGTT_Reservations.Admit (Reservations, 4096, 0, 4096, OK);
      pragma Assert (OK);
      -- The table is large enough, but the owner's admitted aperture is not.
      Transaction.Publish (Outside, Reservations, 4096, 16#100000#, 16384, Status);
      pragma Assert (Status = Reservation_Failed and Reads = 0 and Writes = 0 and Prepares = 0);
      pragma Assert (Intel_GPU_GGTT_Reservations.Count (Reservations) = 0);
   end;
   Reset; Run;
   pragma Assert (Status = Published and Flushes = 1);
   pragma Assert (Table (0) = 0 and Table (5) = 0);
   for I in 1 .. 4 loop
      pragma Assert (Table (I) = 16#100001# + Unsigned_64 (I - 1) * 4096);
   end loop;
   for Position in 1 .. 4 loop
      Reset; Table (Position) := 16#AB25_AB25_AB25_AB25#; Run;
      pragma Assert (Status = Published and Writes = 4 and Prepares = 1);
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
