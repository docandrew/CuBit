with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Application_State;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Update_Storage;
with Update_Test_Backend;
procedure Update_Ledger_Failure_Tests is
   package S renames Intel_GPU_Application_State;
   package P renames Intel_GPU_Table_Provenance;
   package B renames Update_Test_Backend;
   package M is new Intel_GPU_Metadata_Arena (B.Reserve, B.Commit, B.Clear);
   package U is new Intel_GPU_Update_Storage (M, B.Ready);
   Object : U.Pool;
   OK : Boolean;
   Before : Natural;
begin
   U.Request (Object, 1, 262144, OK);
   pragma Assert (OK);
   for Turn in 1 .. 50 loop
      U.Step (Object);
      exit when B.Commits /= 0;
   end loop;
   pragma Assert (B.Commits = 1 and U.Pending (Object) and not U.Ready (Object));
   pragma Assert (P.Capacity (S.Updates (1).Table_Owners) = 16);
   B.Owner := False; U.Step (Object);
   pragma Assert (not U.Pending (Object) and not U.Ready (Object));
   B.Owner := True; Before := B.Commits;
   for Turn in 1 .. 10 loop U.Step (Object); end loop;
   U.Request (Object, 1, 262144, OK);
   pragma Assert (not OK and B.Commits = Before and U.Charged_Bytes (Object) = 4096);
   pragma Assert (P.Capacity (S.Updates (1).Table_Owners) = 16);
   Ada.Text_IO.Put_Line ("Update ledger failure PASS: owner loss before attachment, charge/backing retained, no replay");
end Update_Ledger_Failure_Tests;
