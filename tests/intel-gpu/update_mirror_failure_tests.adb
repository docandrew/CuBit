with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Application_State;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Update_Storage;
with Update_Test_Backend;
procedure Update_Mirror_Failure_Tests is
   package S renames Intel_GPU_Application_State;
   package B renames Update_Test_Backend;
   -- Each invocation is a fresh owner/address space; retained metadata from
   -- one failure is never overwritten to prepare the next injected failure.
   Fault_Step : constant Natural := (if Ada.Command_Line.Argument_Count < 1 then 0
     else Natural'Value (Ada.Command_Line.Argument (1)));
   Fault_Commit : constant Natural := (if Ada.Command_Line.Argument_Count < 2 then 0
     else Natural'Value (Ada.Command_Line.Argument (2)));
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
      OK : constant Boolean := B.Commit (Base, Offset, Bytes);
   begin
      if Fault_Commit /= 0 and then B.Commits = Fault_Commit then B.Owner := False; end if;
      return OK;
   end Commit;
   package M is new Intel_GPU_Metadata_Arena (B.Reserve, Commit, B.Clear);
   package U is new Intel_GPU_Update_Storage (M, B.Ready);
   type RAM is array (0 .. 511) of Unsigned_64;
   Index_RAM : RAM := [others => 0] with Alignment => 4096;
   Object : U.Pool;
   OK : Boolean;
   Steps, Before, Callbacks : Natural := 0;
   Charge : Unsigned_64;
begin
   S.Extend_Update_Index (Unsigned_64 (To_Integer (Index_RAM'Address)), 4096, OK);
   pragma Assert (OK);
   U.Request (Object, 17, 1048576, OK); pragma Assert (OK);
   while U.Pending (Object) loop
      Steps := Steps + 1; pragma Assert (Steps <= 500);
      if Fault_Step /= 0 and then Steps = Fault_Step then B.Owner := False; end if;
      Before := B.Commits;
      U.Step (Object);
      pragma Assert (B.Commits <= Before + 1);
      if not S.Has_Update (17) or else S.VM.Metadata_Capacity (S.Updates (17).Candidate) < 64 then
         pragma Assert (not U.Ready (Object));
      end if;
   end loop;
   if Fault_Step = 0 and Fault_Commit = 0 then
      pragma Assert (U.Ready (Object));
      pragma Assert (S.VM.Metadata_Capacity (S.Updates (17).Candidate) = 64);
      pragma Assert (not S.Updates (17).Tables.Ready);
      Ada.Text_IO.Put_Line ("MIRROR-BASELINE" & Natural'Image (Steps) & Natural'Image (B.Commits));
   else
      pragma Assert (not B.Owner and not U.Ready (Object)); -- Injection was reached.
      Charge := U.Charged_Bytes (Object);
      Callbacks := B.Reservations + B.Commits + B.Clears;
      B.Owner := True;
      for Turn in 1 .. 20 loop U.Step (Object); end loop;
      U.Request (Object, 17, 1048576, OK);
      pragma Assert (not OK and not U.Ready (Object) and not U.Pending (Object));
      pragma Assert (Callbacks = B.Reservations + B.Commits + B.Clears);
      pragma Assert (U.Charged_Bytes (Object) = Charge);
      Ada.Text_IO.Put_Line ("Mirror revocation PASS step=" & Natural'Image (Fault_Step) &
        " commit=" & Natural'Image (Fault_Commit) & " retained/no replay");
   end if;
end Update_Mirror_Failure_Tests;
