--  Hosted test of Handoff_Contexts: the boot admissions, the seal, the
--  switch to Translated, and that no sequence of operations after the seal
--  makes a requester pass through again (DMA_Remapping's xHCI
--  pass-through until devmgr's firmware handoff).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Handoff_Contexts; use Handoff_Contexts;

procedure Main is
   Checks : Natural := 0;
   T : Table := Empty;
   Ok : Boolean;
   --  The IdeaPad's two controllers, and a third behind a bridge.
   Thunderbolt_XHCI : constant Requester := 16#0068#;   -- 00:0d.0
   PCH_XHCI         : constant Requester := 16#00A0#;   -- 00:14.0
   Bridged_XHCI     : constant Requester := 16#0300#;   -- 03:00.0
   Other            : constant Requester := 16#0010#;   -- 00:02.0

   type Requester_List is array (Positive range <>) of Requester;
   Grants : constant Requester_List :=
     [PCH_XHCI, Thunderbolt_XHCI, PCH_XHCI, Bridged_XHCI, Thunderbolt_XHCI];

   procedure Check (Condition : Boolean) is
   begin
      pragma Assert (Condition);
      Checks := Checks + 1;
   end Check;
begin
   Check (not Sealed (T));
   for R in Requester loop
      pragma Assert (Phase_Of (T, R) = Untracked);
   end loop;
   Checks := Checks + 1;

   --  Setup: each xHCI admitted once.
   Admit (T, Thunderbolt_XHCI, Ok); Check (Ok);
   Admit (T, PCH_XHCI, Ok);         Check (Ok);
   Admit (T, Bridged_XHCI, Ok);     Check (Ok);
   Admit (T, PCH_XHCI, Ok);         Check (not Ok);   -- already passing through
   Check (Phase_Of (T, PCH_XHCI) = Passing_Through);
   Check (Phase_Of (T, Other) = Untracked);
   Seal (T);
   Check (Sealed (T));

   --  A requester never admitted has no pass-through to end.
   declare
      Before : constant Table := T;
   begin
      Switch (T, Other, Ok);
      Check (not Ok and then Phase_Of (T, Other) = Untracked);
      pragma Assert (Never_Reopened (Before, T));
   end;

   --  devmgr's grants after each handoff, in any order, repeated.
   for Who of Grants loop
      declare
         Before : constant Table := T;
      begin
         Switch (T, Who, Ok);
         Check (Ok = (Phase_Of (Before, Who) = Passing_Through));
         Check (Phase_Of (T, Who) = Translated);
         pragma Assert (Never_Reopened (Before, T));
         Check (Sealed (T));
      end;
   end loop;

   --  Everything translated or untracked: nothing passes through any more.
   for R in Requester loop
      pragma Assert (Phase_Of (T, R) /= Passing_Through);
   end loop;
   Checks := Checks + 1;

   Put_Line ("handoff-contexts:" & Natural'Image (Checks) & " checks PASS");
end Main;
