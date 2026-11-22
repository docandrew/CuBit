with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Video_Segment; use Intel_GPU_ADLN_Video_Segment;
with Intel_GPU_ADLN_Barrier;
with Intel_GPU_ADLN_Batch_Start;
-- Exact-word tests for the VCS MI_FLUSH_DW segment builder, cross-checked
-- against encodings the RCS builders already emit. Encoding only: no claim
-- that hardware executes these words (the qword store is a NUC check).
procedure Video_Segment_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & Label);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;
   Value : constant Unsigned_64 := 16#0000_0001_8000_0002#;
   Batch : constant Unsigned_64 := 16#0000_7FFF_1234_5678#;
   GGTT_Slot : constant Timeline_Target := (GGTT_Address, 16#00A0_1040#);
   Barrier : constant Intel_GPU_ADLN_Barrier.Barrier_Words :=
     Intel_GPU_ADLN_Barrier.Flush_And_Invalidate;
   I0 : constant Invalidate_Words := Invalidate (VCS0);
   I2 : constant Invalidate_Words := Invalidate (VCS2);
   B : Breadcrumb_Words;
   type Address_List is array (1 .. 4) of Unsigned_64;
   Bad_Batches : constant Address_List := [0, 4, 2 ** 48, 2 ** 48 + 8];
   S : Segment;
begin
   -- Pre-batch invalidation, exact words for VCS0.
   Check (I0 = [16#0280_0101#, 16#1324_4082#, 16#D0#, 0, 0,
                16#1102_0001#, 16#4218#, 1,
                16#0E01_C003#, 0, 16#4218#, 0, 0,
                16#0280_0100#], "VCS0 invalidate words");
   Check (I2 (6) = 16#4298# and I2 (10) = 16#4298# and
          I2 (0 .. 5) = I0 (0 .. 5) and I2 (11 .. 13) = I0 (11 .. 13),
          "VCS2 differs only in its AUX register");
   -- Shared encodings agree with the RCS barrier and batch start.
   Check (I0 (0) = Barrier (6) and I0 (13) = Barrier (21), "pre-parser words");
   Check (I0 (5) = Barrier (13) and I0 (7) = Barrier (15) and
          I0 (8) = Barrier (16), "LRI and semaphore words");
   Check (Intel_GPU_ADLN_Batch_Start.Build_At (Batch).Words (1) = MI_BB_Start_PPGTT,
          "BB_START word");

   -- Breadcrumb to the PPHWSP timeline slot.
   B := Breadcrumb (PPHWSP_Timeline, Value);
   Check (B = [16#1300_0002#, 0, 0, 0,
               16#1320_4003#, 16#200#, 0, 16#8000_0002#, 1,
               16#0100_0000#, 16#0400_0001#, 16#0280_0000#, 0, 0],
          "PPHWSP breadcrumb words");
   -- Breadcrumb to a GGTT timeline page.
   B := Breadcrumb (GGTT_Slot, Value);
   Check (B (4) = 16#1300_4003# and B (5) = 16#00A0_1044# and B (6) = 0,
          "GGTT breadcrumb: USE_GTT, no STORE_INDEX");
   Check (Breadcrumb_Words'Length mod 2 = 0 and Command_Words'Length mod 2 = 0,
          "segments keep the ring tail qword aligned");

   -- Batch segment: invalidate, BB_START, breadcrumb.
   S := Build_Batch (VCS2, Batch, PPHWSP_Timeline, Value);
   Check (S.Valid, "batch valid");
   for I in I2'Range loop
      Check (S.Words (I) = I2 (I), "batch: invalidate prefix");
   end loop;
   Check (S.Words (14 .. 19) =
            [16#0400_0001#, 16#1880_0101#, 16#1234_5678#, 16#7FFF#,
             16#0400_0000#, 0], "batch: dispatch words");
   B := Breadcrumb (PPHWSP_Timeline, Value);
   for I in B'Range loop
      Check (S.Words (20 + I) = B (I), "batch: breadcrumb suffix");
   end loop;
   Check (S.Words (Batch_Value_Low) = 16#8000_0002# and
          S.Words (Batch_Value_High) = 1, "batch: value position");
   -- Exactly one store targets the timeline slot: the final breadcrumb.
   declare
      Stores : Natural := 0;
   begin
      for I in S.Words'First .. S.Words'Last - 1 loop
         if (S.Words (I) and 16#FF80_0000#) = MI_Flush_DW and then
           (S.Words (I) and Flush_Op_Store_DW) /= 0 and then
           S.Words (I + 1) = PPHWSP_Timeline_Offset
         then
            Stores := Stores + 1;
         end if;
      end loop;
      Check (Stores = 1, "batch: single timeline writer");
   end;

   -- Signal-only segment.
   Check (Build_Signal (GGTT_Slot, Value).Valid and
          Build_Signal (GGTT_Slot, Value).Words = Breadcrumb (GGTT_Slot, Value),
          "signal");

   -- Invalid inputs give all-zero, invalid segments.
   declare
      Bad_Targets : constant array (Positive range <>) of Timeline_Target :=
        [(PPHWSP_Index, 16#D0#),          -- barrier scratch
         (PPHWSP_Index, 16#220#),         -- bit 5 set
         (PPHWSP_Index, 16#204#),         -- not qword aligned
         (PPHWSP_Index, 16#1000#),        -- outside the PPHWSP
         (GGTT_Address, 0),
         (GGTT_Address, 16#1_0000_0000#), -- above 4 GiB
         (GGTT_Address, 16#00A0_1060#)];  -- bit 5 set
   begin
      for T of Bad_Targets loop
         Check (not Valid_Target (T), "invalid target");
         S := Build_Batch (VCS0, Batch, T, Value);
         Check (not S.Valid and (for all W of S.Words => W = 0), "bad target batch");
         Check (not Build_Signal (T, Value).Valid, "bad target signal");
      end loop;
   end;
   for Bad_Batch of Bad_Batches loop
      S := Build_Batch (VCS0, Bad_Batch, PPHWSP_Timeline, Value);
      Check (not S.Valid and (for all W of S.Words => W = 0), "bad batch address");
   end loop;
   S := Build_Batch (VCS0, Batch, PPHWSP_Timeline, 0);
   Check (not S.Valid, "zero value refused");
   Check (Build_Batch (VCS0, Batch, PPHWSP_Timeline, Unsigned_64'Last).Valid and
          Build_Batch (VCS0, Batch, PPHWSP_Timeline, Unsigned_64'Last).Words
            (Batch_Value_High) = 16#FFFF_FFFF#, "full 64-bit value");

   Put_Line ("video_segment_tests: PASS (" & Checks'Image & " checks)");
end Video_Segment_Tests;
