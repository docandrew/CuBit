with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Application_State;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Update_Storage;
with Intel_GPU_Table_Provenance;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Update_Test_Backend;
procedure Update_Storage_Tests is
   package S renames Intel_GPU_Application_State;
   package B renames Update_Test_Backend;
   package M is new Intel_GPU_Metadata_Arena (B.Reserve, B.Commit, B.Clear);
   package U is new Intel_GPU_Update_Storage (M, B.Ready);
   type RAM is array (0 .. 511) of Unsigned_64;
   Index_RAM : RAM := [others => 0] with Alignment => 4096;
   Object : U.Pool;
   OK : Boolean;
   Before : Unsigned_64;
   Saved : S.Update_Access;
   use type S.Update_Access;
   procedure Finish is
      Commits_Before : Natural;
   begin
      for Turn in 1 .. 500 loop
         Commits_Before := B.Commits;
         U.Step (Object);
         pragma Assert (B.Commits <= Commits_Before + 1);
         exit when not U.Pending (Object);
      end loop;
      pragma Assert (U.Ready (Object) and not U.Pending (Object));
      pragma Assert (U.Charged_Bytes (Object) = B.Committed);
   end Finish;
begin
   pragma Assert (not U.Ready (Object) and not U.Pending (Object));
   S.Extend_Update_Index (Unsigned_64 (To_Integer (Index_RAM'Address)), 4096, OK);
   pragma Assert (OK);
   U.Request (Object, 1, 4 * 1024 * 1024, OK, Tables => 6);
   pragma Assert (OK); Finish;
   pragma Assert (S.VM.Metadata_Capacity (S.Updates (1).Candidate) = 6);
   pragma Assert (U.Charged_Bytes (Object) < 65536);
   for I in 17 .. 18 loop
      U.Request (Object, I, 4 * 1024 * 1024, OK, Tables => 6);
      pragma Assert (OK); Finish;
      pragma Assert (S.VM.Metadata_Capacity (S.Updates (I).Candidate) = 6);
      S.Table_References.Put (S.Updates (I).Table_IDs, 1, 6, I + 1000, OK);
      pragma Assert (OK);
   end loop;
   Saved := S.Updates (17);
   Before := B.Committed;
   U.Request (Object, 17, 4 * 1024 * 1024, OK, Tables => 6);
   pragma Assert (OK); Finish;
   pragma Assert (B.Committed = Before and S.Updates (17) = Saved);
   U.Request (Object, 17, 4 * 1024 * 1024, OK); -- Default preserves full-table caller.
   pragma Assert (OK); Finish;
   pragma Assert (S.VM.Metadata_Capacity (S.Updates (17).Candidate) = S.Table_Pages);
   pragma Assert (S.Updates (17) = Saved and S.Table_References.Get (Saved.Table_IDs, 1, 6) = 1017);
   pragma Assert (S.VM.Metadata_Capacity (S.Updates (18).Candidate) = 6);
   pragma Assert (S.Table_References.Get (S.Updates (18).Table_IDs, 1, 6) = 1018);
   Before := B.Committed;
   U.Request (Object, 18, 4 * 1024 * 1024, OK, Tables => 6);
   pragma Assert (OK); Finish;
   pragma Assert (B.Committed = Before);
   U.Request (Object, 18, 8 * 1024 * 1024, OK);
   pragma Assert (not OK and B.Committed = Before);
   U.Request (Object, 18, 4 * 1024 * 1024, OK, Tables => 65);
   pragma Assert (not OK and B.Committed = Before);
   pragma Assert (not Saved.Tables.Ready and S.VM.Backed_Tables (Saved.Candidate) = 0);
   -- Exercise the actual offline VM builder with the demand-sized stores.
   -- These synthetic DMA identities test topology, not hardware ownership.
   S.VM.Initialize (S.Updates (1).Candidate,
     [4096, 8192, 12288, 16384, others => 0], OK, Backing_Count => 4);
   pragma Assert (OK);
   S.VM.Map_Page (S.Updates (1).Candidate, 4096, 16#100000#,
     Write_Back, Read_Write, OK);
   pragma Assert (OK);
   S.VM.Seal (S.Updates (1).Candidate, OK); pragma Assert (OK);
   S.VM.Prepare_Update (S.Updates (18).Candidate, S.Updates (1).Candidate,
     [16#10000#, 16#11000#, 16#12000#, 16#13000#, 16#14000#, 16#15000#,
      others => 0], OK, Backing_Count => 6);
   pragma Assert (OK and S.VM.Used (S.Updates (18).Candidate) = 4);
   S.VM.Map_Page (S.Updates (18).Candidate, 2 ** 30, 16#200000#,
     Write_Back, Read_Write, OK);
   pragma Assert (OK and S.VM.Used (S.Updates (18).Candidate) = 6);
   pragma Assert (S.VM.Lookup (S.Updates (18).Candidate, 4096) = 16#100003#);
   pragma Assert (S.VM.Lookup (S.Updates (18).Candidate, 2 ** 30) = 16#200003#);
   pragma Assert (S.VM.Lookup (S.Updates (1).Candidate, 2 ** 30) = 0);
   -- A generation change while growth is pending invalidates this operation,
   -- even if the old generation later returns. No allocation may be replayed.
   U.Request (Object, 18, 4 * 1024 * 1024, OK, Tables => 64);
   pragma Assert (OK and U.Pending (Object));
   S.Updates (18).Table_Generation := 2;
   U.Step (Object);
   pragma Assert (not U.Pending (Object) and not U.Ready (Object));
   pragma Assert (B.Committed = Before);
   S.Updates (18).Table_Generation := 1;
   U.Step (Object);
   U.Request (Object, 18, 4 * 1024 * 1024, OK, Tables => 64);
   pragma Assert (not OK and B.Committed = Before);
   Ada.Text_IO.Put_Line ("Update regions PASS: independent demand, stable17/18 switching,6->64 growth, bounded commits, shared charges, no GPU backing publication");
   Ada.Text_IO.Put_Line ("Update epoch PASS: pending generation change fails without commit or replay");
   Ada.Text_IO.Put_Line ("Update topology PASS: demand-sized six-table clone and map preserve source");
end Update_Storage_Tests;
