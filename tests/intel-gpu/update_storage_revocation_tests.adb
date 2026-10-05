with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Application_State;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Update_Storage;
with Update_Test_Backend;
procedure Update_Storage_Revocation_Tests is
   package S renames Intel_GPU_Application_State;
   package B renames Update_Test_Backend;
   type RAM is array (0 .. 511) of Unsigned_64;
   Index_RAM : RAM := [others => 0] with Alignment => 4096;
   OK : Boolean;
begin
   S.Extend_Update_Index (Unsigned_64 (To_Integer (Index_RAM'Address)), 4096, OK);
   pragma Assert (OK);
   for Mode in 1 .. 5 loop
      declare
         package M is new Intel_GPU_Metadata_Arena (B.Reserve, B.Commit, B.Clear);
         package U is new Intel_GPU_Update_Storage (M, B.Ready);
         Object : U.Pool;
         Before : Natural;
         Quota : constant Unsigned_64 := (if Mode = 5 then 4096 else 1048576);
      begin
         B.Reset;
         B.Lose_On_Reserve := Mode = 1; B.Lose_On_Commit := Mode = 2;
         B.Fail_Clear := Mode = 3; B.Fail_Commit := Mode = 4;
         U.Request (Object, 17, Quota, OK); pragma Assert (OK);
         for Turn in 1 .. 100 loop
            U.Step (Object);
            exit when not U.Pending (Object);
         end loop;
         pragma Assert (not U.Ready (Object) and not U.Pending (Object) and not S.Has_Update (17));
         B.Owner := True;
         Before := B.Reservations + B.Commits + B.Clears;
         for Turn in 1 .. 10 loop U.Step (Object); end loop;
         U.Request (Object, 17, Quota, OK);
         pragma Assert (not OK and B.Reservations + B.Commits + B.Clears = Before);
         pragma Assert (U.Charged_Bytes (Object) = (if Mode = 1 then 0 else 4096));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Update revocation PASS: reserve/commit ownership, clear/commit failure, aggregate exhaustion, retained charges and no replay");
end Update_Storage_Revocation_Tests;
