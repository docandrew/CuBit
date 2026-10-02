with Interfaces; use Interfaces;
with Intel_GPU_GGTT;
with Intel_GPU_GGTT_Publish;
with Intel_GPU_GGTT_Reservations;
with Intel_GPU_Submission_Backing;
with Intel_GPU_Submission_Image;
with Intel_GPU_Submission_Materialize;
procedure Submission_Publish_Tests is
   package Images renames Intel_GPU_Submission_Image;
   package Writer renames Intel_GPU_Submission_Materialize;
   package Reservations renames Intel_GPU_GGTT_Reservations;
   Allocation_DMA : constant Unsigned_64 := 16#1000000#;
   Context_DMA : constant Unsigned_64 := Allocation_DMA +
     Intel_GPU_Submission_Backing.First;
   Table : array (Unsigned_64 range 0 .. 511) of Unsigned_64;
   Buffer : Writer.Bytes (0 .. Writer.Byte_Count - 1);
   Prepared, Visible : Boolean;
   Writes, Flushes : Natural;
   Fail_Prepare, Fail_Store : Boolean;
   Engine_Page : Boolean := False;
   Engine_DMA : constant Unsigned_64 := Allocation_DMA +
     Intel_GPU_Submission_Backing.Offsets (Intel_GPU_Submission_Backing.Engine_Status_Page);
   function Allowed (First, Bytes : Unsigned_64) return Boolean is
     (First >= 8192 and then First < 64 * 4096 and then
      Bytes > 0 and then Bytes <= 64 * 4096 - First);
   procedure Prepare (First, Bytes : Unsigned_64; OK : out Boolean) is
      Image : constant Images.Image := Images.Build (Context_DMA, First);
   begin
      if Engine_Page then
         pragma Assert (Prepared and Visible and Writes = 20 and Bytes = 4096);
         OK := True; return;
      end if;
      pragma Assert (Writes = 0 and not Prepared and Bytes = Images.GGTT_Bytes);
      pragma Assert (Image.Valid and Image.Words (1024 + 51) =
        Unsigned_32 (Allocation_DMA + Intel_GPU_Submission_Backing.Offsets
          (Intel_GPU_Submission_Backing.PML4)));
      Writer.Write (Image, Buffer, OK);
      Prepared := OK;
      Visible := OK and not Fail_Prepare; -- model cache-maintenance failure
      OK := Visible;
   end Prepare;
   procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64;
                       OK : out Boolean) is
   begin
      Value := Table (Index); OK := True;
   end Read_PTE;
   procedure Write_PTE (Index, Value : Unsigned_64; OK : out Boolean) is
   begin
      pragma Assert (Prepared and Visible);
      if Engine_Page then
         pragma Assert (Index = 22 and Value = Intel_GPU_GGTT.Encode_System_Page (Engine_DMA));
      else
         pragma Assert (Index in 2 .. 21);
         pragma Assert (Value = Intel_GPU_GGTT.Encode_System_Page
                        (Context_DMA + (Index - 2) * 4096));
      end if;
      Writes := Writes + 1;
      Table (Index) := Value;
      OK := not Fail_Store; -- store can reach the GPU even on reported failure
   end Write_PTE;
   procedure Invalidate (OK : out Boolean) is
   begin
      pragma Assert (Writes = (if Engine_Page then 21 else 20) and Visible);
      Flushes := Flushes + 1; OK := True;
   end Invalidate;
   package Publication is new Intel_GPU_GGTT_Publish
     (Allowed, Prepare, Read_PTE, Write_PTE, Invalidate);
   use type Publication.Result;
begin
   for Scenario in 0 .. 2 loop
      declare
         Ledger : Reservations.Ledger;
         Attempt, Another, Engine_Attempt : Publication.Attempt;
         OK : Boolean;
         Selected : Unsigned_64;
         Status : Publication.Result;
         Saved_Writes : Natural;
      begin
         Table := [others => 0]; Buffer := [others => 16#A5#];
         Engine_Page := False;
         Prepared := False; Visible := False; Writes := 0; Flushes := 0;
         Fail_Prepare := Scenario = 1; Fail_Store := Scenario = 2;
         -- Hardware table geometry is 2MiB; the mock implements only the
         -- narrow admitted aperture, so any out-of-aperture read also fails.
         Reservations.Admit (Ledger, 2 * 1024 * 1024, 4096, 63 * 4096, OK);
         pragma Assert (OK);
         Publication.Publish_Available
           (Attempt, Ledger, Context_DMA, Images.GGTT_Bytes, 4096, Selected, Status);
         pragma Assert (Selected = 8192);
         pragma Assert (Status =
           (case Scenario is when 0 => Publication.Published,
                             when 1 => Publication.Prepare_Failed,
                             when others => Publication.Quarantined));
         pragma Assert (Writes = (case Scenario is when 0 => 20,
                                  when 1 => 0, when others => 1));
         pragma Assert (Flushes = (if Scenario = 0 then 1 else 0));
         -- No GGTT aliases for the table/batch/completion tail pages.
         for I in Unsigned_64 range 22 .. Table'Last loop
            pragma Assert (Table (I) = 0);
         end loop;
         pragma Assert (Table (0) = 0 and Table (1) = 0);
         Saved_Writes := Writes;
         Publication.Publish (Another, Ledger, Selected, Context_DMA,
                              Images.GGTT_Bytes, Status);
         pragma Assert (Status = Publication.Reservation_Failed and
                        Writes = Saved_Writes);
         if Scenario = 0 then
            Engine_Page := True;
            Publication.Publish_Available
              (Engine_Attempt, Ledger, Engine_DMA, 4096, 4096, Selected, Status);
            pragma Assert (Status = Publication.Published and Selected = 22 * 4096);
            pragma Assert (Writes = 21 and Flushes = 2);
            -- Exactly one additional alias, not the intervening private VM,
            -- batch or completion pages in the physical allocation.
            pragma Assert (for all I in Unsigned_64 range 23 .. Table'Last => Table (I) = 0);
         end if;
      end;
   end loop;
end Submission_Publish_Tests;
