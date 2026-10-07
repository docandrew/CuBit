with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Metadata_Platform;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
with Intel_GPU_Application_State;
with Intel_GPU_Update_Storage;
procedure Update_Metadata_Check is
   package P renames Intel_GPU_Metadata_Platform;
   package S renames Intel_GPU_Application_State;
   Held : Boolean := True;
   Commits : Natural := 0;
   Committed : Unsigned_64 := 0;
   Ignore : Unsigned_64;
   function Owner return Boolean is (Held);
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
      OK : constant Boolean := P.Commit (Base, Offset, Bytes);
   begin
      Commits := Commits + 1;
      if OK then Committed := Committed + Bytes; end if;
      return OK;
   end Commit;
   package M is new Intel_GPU_Metadata_Arena (P.Reserve, Commit, Intel_GPU_Metadata_Initialize.Clear);
   package U is new Intel_GPU_Update_Storage (M, Owner);
   Object : U.Pool;
   Saved : S.Update_Access;
   use type S.Update_Access, S.Installation_Phase;
   OK : Boolean;
   Index_Base, Before : Unsigned_64;
   procedure Check (Condition : Boolean; Detail : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL native update metadata: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
   procedure Grow (Index, Tables : Positive) is
      Old_Commits : Natural;
   begin
      U.Request (Object, Index, 8 * 1024 * 1024, OK, Tables);
      Check (OK, "request");
      for Turn in 1 .. 500 loop
         Old_Commits := Commits;
         U.Step (Object);
         Check (Commits <= Old_Commits + 1, "bounded commit");
         exit when not U.Pending (Object);
      end loop;
      Check (U.Ready (Object) and not U.Pending (Object), "ready");
      Check (U.Charged_Bytes (Object) = Committed, "shared accounting");
      Check (not S.Updates (Index).Tables.Ready, "no GPU backing");
   end Grow;
begin
   debugPrint ("native update metadata: real owned-memory reservations (NO GPU/ISOLATION)" & ASCII.LF);
   Index_Base := P.Reserve (8192); Check (Index_Base /= 0, "index reserve");
   Check (P.Commit (Index_Base, 0, 8192), "index commit");
   S.Extend_Update_Index (Index_Base, 8192, OK); Check (OK, "index extension");
   Grow (17, 6); Grow (18, 6); Grow (900, 6);
   Saved := S.Updates (17);
   S.Table_References.Put (Saved.Table_IDs, 1, 6, 1017, OK); Check (OK, "sentinel write");
   Before := Committed; Grow (17, 6);
   Check (Before = Committed and S.Updates (17) = Saved, "stable revisit");
   Grow (17, 64);
   Check (S.Updates (17) = Saved and S.Table_References.Get (Saved.Table_IDs, 1, 6) = 1017,
     "growth preserves pointer and sentinel");
   Check (S.VM.Metadata_Capacity (Saved.Candidate) = 64 and
     S.VM.Metadata_Capacity (S.Updates (18).Candidate) = 6 and
     S.VM.Metadata_Capacity (S.Updates (900).Candidate) = 6, "independent images");
   Before := Committed; Grow (18, 6); Check (Before = Committed, "second revisit");
   declare Attempt : S.Installation; begin
      S.Begin_Fresh_Update (Attempt, 901, Unsigned_64 (To_Integer (Saved.all'Address)),
        S.Update_Storage_Bytes, OK);
      Check (OK, "overlap preflight");
      S.Step_Fresh_Update (Attempt);
      Check (S.State (Attempt) = S.Failed and S.Checked (Attempt) <= 64 and
        not S.Has_Update (901), "sparse range overlap");
   end;
   debugPrint ("native update metadata: sparse17/18/900 stable growth and overlap PASS" & ASCII.LF);
   U.Request (Object, 900, 8 * 1024 * 1024, OK, 64); Check (OK, "pending owner test");
   Held := False; U.Step (Object); Held := True;
   Check (not U.Ready (Object) and not U.Pending (Object), "owner loss");
   U.Request (Object, 900, 8 * 1024 * 1024, OK, 64);
   Check (not OK and Committed = Before and U.Charged_Bytes (Object) = Before,
     "no replay after callback owner loss");
   debugPrint ("TEST: PASS native update metadata independent demand stable ranges retained failure (NO GPU/ISOLATION)" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end Update_Metadata_Check;
