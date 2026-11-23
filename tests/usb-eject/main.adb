--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Hosted tests for FS-034 (USB stick hot-plug and Eject): the unit badge
--  encoding, the disk presence report, xhci's per-slot report state, and
--  the eject answer codec and plan. Byte-exact fixtures plus exhaustive
--  sweeps over the small domains.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Block_Unit_Badges; use CuBit.Block_Unit_Badges;
with CuBit.Disk_Presence;
with CuBit.Volume_Ejection;
with Test_Reports; use Test_Reports;
with Devmgr_USB_Disks;

procedure Main is
   package DP renames CuBit.Disk_Presence;
   package VE renames CuBit.Volume_Ejection;
   use type DP.Presence, DP.Report, DP.Words, VE.Outcome, VE.Step, VE.Answer,
            VE.Open_Handle_Policy;

   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & What);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;

   --  CuBit.Block_Devices.Write_Authority_Tag ("BLKWRITE"), the whole-device grant.
   Whole_Device : constant Unsigned_64 := 16#424C_4B57_5249_5445#;

   procedure Badges is
   begin
      --  Byte-exact: "BLKU" then the unit.
      Check (Badge (1) = 16#424C_4B55_0000_0001#, "badge of unit 1");
      Check (Badge (Unit'Last) = 16#424C_4B55_FFFF_FFFF#, "badge of the last unit");
      Check (Unit_Of (16#424C_4B55_0000_0007#) = 7, "unit 7 decoded");
      Check (Unit_Of (0) = No_Unit, "untagged names no unit");
      Check (Unit_Of (Whole_Device) = No_Unit, "the whole-device grant names no unit");
      Check (Unit_Of (16#424C_4B55_0000_0000#) = No_Unit, "unit 0 is no unit");
      Check (Unit_Of (16#424C_4B56_0000_0001#) = No_Unit, "another prefix names no unit");
      Check (Unit_Of (16#0000_0000_0000_0001#) = No_Unit, "a bare number names no unit");
      for U in Unit range 1 .. 70_000 loop
         Check (Unit_Of (Badge (U)) = U and then Badge (U) /= Whole_Device, "round trip");
      end loop;
   end Badges;

   procedure Presence_Reports is
      Item : DP.Report;
      Valid : Boolean;
      W : DP.Words;
   begin
      --  devmgr's label beside OP_PUBLISH_XHCI_STATS (16#0243#).
      Check (DP.Report_Label = 16#0245# and then DP.Report_Words = 2, "report label and length");
      W := DP.Encode ((Unit => 5, State => DP.Present, Write_Protected => False));
      Check (W = [5, 1], "present report words");
      W := DP.Encode ((Unit => 5, State => DP.Present, Write_Protected => True));
      Check (W = [5, 16#101#], "write-protected present report words");
      W := DP.Encode ((Unit => 9, State => DP.Gone, Write_Protected => False));
      Check (W = [9, 2], "gone report words");
      DP.Decode (2, [9, 2], Item, Valid);
      Check (Valid and then Item = (Unit => 9, State => DP.Gone, Write_Protected => False), "gone decoded");
      DP.Decode (2, [5, 16#101#], Item, Valid);
      Check (Valid and then Item.Write_Protected and then Item.State = DP.Present, "wp decoded");
      --  Refused: wrong length, unit 0 or too large, unknown state, a
      --  write-protected Gone, stray bits.
      DP.Decode (1, [5, 1], Item, Valid);
      Check (not Valid, "short report refused");
      DP.Decode (3, [5, 1], Item, Valid);
      Check (not Valid, "long report refused");
      DP.Decode (2, [0, 1], Item, Valid);
      Check (not Valid, "unit 0 refused");
      DP.Decode (2, [2 ** 32, 1], Item, Valid);
      Check (not Valid, "unit past 32 bits refused");
      for S in Unsigned_64 range 0 .. 16#3FF# loop
         DP.Decode (2, [3, S], Item, Valid);
         Check (Valid = (S in 1 | 2 | 16#101#), "state word sweep");
         if Valid then
            Check (DP.Encode (Item) = [3, S], "canonical");
         end if;
      end loop;
      DP.Decode (2, [3, 16#1_0000_0001#], Item, Valid);
      Check (not Valid, "high state bits refused");
   end Presence_Reports;

   procedure Slot_Reports is
      R : Reports.State;
      Due : Boolean;
      Rep : DP.Report;
   begin
      Check (Reports.Valid (R), "fresh state valid");
      Reports.Next (R, 1, Due, Rep);
      Check (not Due, "nothing owed at first");
      --  A disk appears: Present owed until sent.
      Reports.Appear (R, 1, 10, False);
      Reports.Next (R, 1, Due, Rep);
      Check (Due and then Rep = (Unit => 10, State => DP.Present, Write_Protected => False), "present owed");
      Reports.Next (R, 1, Due, Rep);
      Check (Due and then Rep.Unit = 10, "still owed while not sent (queue full)");
      Reports.Sent (R, 1);
      Reports.Next (R, 1, Due, Rep);
      Check (not Due, "present delivered");
      --  It goes: Gone owed.
      Reports.Vanish (R, 1);
      Reports.Next (R, 1, Due, Rep);
      Check (Due and then Rep = (Unit => 10, State => DP.Gone, Write_Protected => False), "gone owed");
      --  A new disk in the same slot before the Gone went out: Gone first.
      Reports.Appear (R, 1, 11, True);
      Reports.Next (R, 1, Due, Rep);
      Check (Due and then Rep.State = DP.Gone and then Rep.Unit = 10, "old gone before new present");
      Reports.Sent (R, 1);
      Reports.Next (R, 1, Due, Rep);
      Check (Due and then Rep = (Unit => 11, State => DP.Present, Write_Protected => True), "then new present");
      --  A disk that comes and goes unannounced is never reported.
      Reports.Appear (R, 2, 12, False);
      Reports.Vanish (R, 2);
      Reports.Next (R, 2, Due, Rep);
      Check (not Due, "unannounced disk not reported");
      --  Other slots are untouched.
      Reports.Next (R, 1, Due, Rep);
      Check (Due and then Rep.Unit = 11, "slot 1 unaffected by slot 2");
      Reports.Sent (R, 1);
      Reports.Vanish (R, 1);
      Reports.Sent (R, 1);
      Reports.Next (R, 1, Due, Rep);
      Check (not Due, "all delivered");
      Check (Reports.Valid (R), "state stays valid");
   end Slot_Reports;

   procedure Ejection is
      A : VE.Answer;
      Valid : Boolean;
      P : VE.Open_Handle_Policy;
   begin
      Check (VE.Encode ((Result => VE.Safe_To_Remove, Open => 0)) = 1, "safe word");
      Check (VE.Encode ((Result => VE.Open_Handles, Open => 3)) = 16#3_0000_0002#, "open word");
      Check (VE.Encode ((Result => VE.Access_Denied, Open => 0)) = 6, "denied word");
      VE.Decode (16#3_0000_0002#, A, Valid);
      Check (Valid and then A = (Result => VE.Open_Handles, Open => 3), "open decoded");
      VE.Decode (2, A, Valid);
      Check (not Valid, "open handles without a count refused");
      VE.Decode (16#1_0000_0001#, A, Valid);
      Check (not Valid, "a count with safe refused");
      VE.Decode (0, A, Valid);
      Check (not Valid, "outcome 0 refused");
      VE.Decode (7, A, Valid);
      Check (not Valid, "outcome 7 refused");
      VE.Decode (16#101#, A, Valid);
      Check (not Valid, "stray bits refused");
      for Code in Unsigned_64 range 0 .. 16#FF# loop
         VE.Decode (Code, A, Valid);
         Check (Valid = (Code in 1 | 3 .. 6), "outcome sweep");
      end loop;
      VE.Decode_Policy (1, P, Valid);
      Check (Valid and then P = VE.Refuse_If_Open, "policy 1");
      VE.Decode_Policy (2, P, Valid);
      Check (Valid and then P = VE.Close_Open_Handles, "policy 2");
      VE.Decode_Policy (0, P, Valid);
      Check (not Valid, "policy 0 refused");
      VE.Decode_Policy (3, P, Valid);
      Check (not Valid, "policy 3 refused");
      --  The plan.
      Check (VE.Plan (False, True, 0, VE.Close_Open_Handles) = VE.Refuse_Not_Attached, "not attached");
      Check (VE.Plan (True, False, 0, VE.Close_Open_Handles) = VE.Refuse_Not_Removable, "fixed disk");
      Check (VE.Plan (True, True, 0, VE.Refuse_If_Open) = VE.Flush_Only, "idle stick");
      Check (VE.Plan (True, True, 2, VE.Refuse_If_Open) = VE.Refuse_Open, "open refused");
      Check (VE.Plan (True, True, 2, VE.Close_Open_Handles) = VE.Close_Then_Flush, "open closed");
      Check (VE.After_Flush (True) = (Result => VE.Safe_To_Remove, Open => 0), "flushed: safe");
      Check (VE.After_Flush (False) = (Result => VE.Flush_Failed, Open => 0), "not flushed: refused");
   end Ejection;
   procedure Devmgr_Table is
      package DU renames Devmgr_USB_Disks;
      use type DU.Phase, DU.Placement;
      T : DU.Table;
      R : DU.Placement;
      N : DU.Ordinal;
      Found : Boolean;
   begin
      Check (DU.Valid (T), "empty table valid");
      --  Driver 3's unit 1, then driver 4's unit 1: names 0 and 1.
      DU.Note_Present (T, 3, 1, False, R, N);
      Check (R = DU.Placed and then N = 0 and then T (0).State = DU.Attach_Owed, "first stick @usb:0");
      DU.Note_Present (T, 4, 1, True, R, N);
      Check (R = DU.Placed and then N = 1, "second controller's stick @usb:1");
      DU.Note_Present (T, 3, 1, False, R, N);
      Check (R = DU.Already_Known, "a repeated report changes nothing");
      DU.Next_Due (T, 0, Found, N);
      Check (Found and then N = 0, "attach of @usb:0 due");
      --  An attach that timed out is retried later and remembered as told.
      DU.Attach_Done (T, 0, DU.Not_Answered, 1000);
      Check (T (0).State = DU.Attach_Owed and then T (0).Told and then T (0).Retry_At = 1250,
             "unanswered attach retried");
      DU.Next_Due (T, 1100, Found, N);
      Check (Found and then N = 1, "the other attach meanwhile");
      DU.Attach_Done (T, 1, DU.Answered_OK, 1100);
      DU.Next_Due (T, 1100, Found, N);
      Check (not Found, "nothing due before the retry");
      Check (DU.Next_Wake (T) = 1250, "wake at the retry");
      DU.Attach_Done (T, 0, DU.Answered_OK, 1300);
      --  Gone: detach owed; the name stays held until acknowledged.
      DU.Note_Gone (T, 3, 1, Found, N);
      Check (Found and then N = 0 and then T (0).State = DU.Detach_Owed, "gone owes a detach");
      DU.Note_Present (T, 3, 2, False, R, N);
      Check (R = DU.Placed and then N = 2, "a new stick never takes a name being detached");
      DU.Detach_Done (T, 0, DU.Not_Answered, 2000);
      Check (T (0).State = DU.Detach_Owed and then T (0).Retry_At = 2250, "unanswered detach retried");
      DU.Detach_Done (T, 0, DU.Answered_OK, 2300);
      Check (T (0).State = DU.Free, "acknowledged detach frees the name");
      DU.Note_Present (T, 3, 3, False, R, N);
      Check (R = DU.Placed and then N = 0, "the freed name is the lowest again");
      --  Gone before the filesystem heard: no detach.
      DU.Note_Gone (T, 3, 3, Found, N);
      Check (Found and then T (0).State = DU.Free, "untold stick needs no detach");
      --  An unanswered attach then gone: detach owed (it may be known).
      DU.Attach_Done (T, 2, DU.Not_Answered, 3000);
      DU.Note_Gone (T, 3, 2, Found, N);
      Check (T (2).State = DU.Detach_Owed, "maybe-known stick is detached");
      --  A driver ends: its attached disks owe detaches, others untouched.
      DU.Driver_Ended (T, 4);
      Check (T (1).State = DU.Detach_Owed, "ended driver's disk detached");
      Check (T (2).State = DU.Detach_Owed, "other driver's disk untouched");
      DU.Note_Gone (T, 9, 9, Found, N);
      Check (not Found, "unknown unit gone: nothing");
      --  Every name used: capacity.
      T := [others => <>];
      for U in Unit range 1 .. DU.Maximum_Disks loop
         DU.Note_Present (T, 5, U, False, R, N);
         Check (R = DU.Placed, "fill");
      end loop;
      DU.Note_Present (T, 5, DU.Maximum_Disks + 1, False, R, N);
      Check (R = DU.No_Free_Name, "no free name");
      Check (DU.Valid (T), "valid throughout");
   end Devmgr_Table;
begin
   Badges;
   Devmgr_Table;
   Presence_Reports;
   Slot_Reports;
   Ejection;
   Put_Line ("usb-eject: PASS" & Natural'Image (Checks) & " checks");
end Main;
