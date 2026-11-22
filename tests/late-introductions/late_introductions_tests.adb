--  Hosted tests of procmgr's late-introduction ledger (ABI-002).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Introduction_Ledgers; use CuBit.Introduction_Ledgers;
with CuBit.Late_Introduction;

procedure Late_Introductions_Tests is
   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   Logstore_Role : constant := 9;
   Desktop_Role  : constant := 6;
   A : constant Process_Word := 16#0100_0020#;
   B : constant Process_Word := 16#0100_0021#;
   L : Ledger := Empty;
   S, T : Slot_Count;
   D : constant Decision :=
     (Holder => A, Index => 3, Role => Logstore_Role, Rights => 3, Tag => 16#4C4F_5000_000F_0007#,
      Way => Saved_Reply, Waiting => False);
begin
   --  Keep, then Find gives back exactly that decision.
   Keep (L, D, S);
   Check (S /= 0 and then L (S) = D, "kept decision stored as given");
   Check (Find (L, A, 3) = S, "found by its holder and index");
   --  Another holder, the same index: nothing (no cross-holder introduction).
   Check (Find (L, B, 3) = 0, "another holder's index finds nothing");
   Check (Find (L, A, 4) = 0, "an index not kept finds nothing");
   --  The same holder and index twice: refused, ledger unchanged.
   declare
      Before : constant Ledger := L;
   begin
      Keep (L, D, T);
      Check (T = 0 and then L = Before, "a duplicate is not kept");
   end;
   --  Not waiting: a program decision is not deliverable until its call is kept.
   Check (Next_For (L, Logstore_Role, 1) = 0, "not delivered before its call waits");
   Set_Waiting (L, S);
   Check (Next_For (L, Logstore_Role, 1) = S, "delivered once its call waits");
   Check (Next_For (L, Desktop_Role, 1) = 0, "never for another role");
   --  Offers (devmgr) are always deliverable, and kept after delivery.
   Keep (L, (Holder => B, Index => 1, Role => Desktop_Role, Rights => 0, Tag => 0,
             Way => Offer, Waiting => False), T);
   Check (T /= 0 and then Next_For (L, Desktop_Role, 1) = T, "an offer is deliverable");
   --  Used once: removed after the introduction.
   Remove (L, S);
   Check (Find (L, A, 3) = 0 and then Next_For (L, Logstore_Role, 1) = 0, "used once");
   --  A holder that exits takes its decisions with it; others stay.
   Keep (L, D, S);
   Keep (L, (D with delta Index => 5), S);
   Forget (L, A);
   Check (Find (L, A, 3) = 0 and then Find (L, A, 5) = 0, "forgotten with its holder");
   Check (Find (L, B, 1) = T, "another holder's decision stays");
   Check (CuBit.Introduction_Ledgers.Count (L) = 1, "one decision left");
   --  The named ceiling: a full ledger keeps nothing more.
   L := Empty;
   for I in 1 .. Maximum_Decisions loop
      Keep (L, (D with delta Index => Unsigned_64 (I)), S);
   end loop;
   Check (CuBit.Introduction_Ledgers.Count (L) = Maximum_Decisions, "filled to the ceiling");
   Keep (L, (D with delta Index => Unsigned_64 (Maximum_Decisions + 1)), S);
   Check (S = 0, "nothing past the ceiling");
   --  The introducer's name fits a startup-table name.
   Check (CuBit.Late_Introduction.Binding'Length in 1 .. 64, "introducer name fits");
   Put_Line ("late-introductions:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      raise Program_Error;
   end if;
end Late_Introductions_Tests;
