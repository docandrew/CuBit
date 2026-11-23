------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  CCL.Growing_Tables' body is not proved: these check its contracts (the
--  postconditions are not checked at run time, see the spec) and its
--  sharing directly.
------------------------------------------------------------------------------
with Ada.Text_IO;
with GNAT.Source_Info;
with Integer_Tables; use Integer_Tables;

procedure Main is
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not Good then
         raise Program_Error with Site & ": growing table check" & Checks'Image;
      end if;
   end Check;

   --  Whether T holds exactly First, First + 1, ... (Length (T) of them).
   function Counts_From (T : Table; First : Integer) return Boolean is
     (for all P in 1 .. Length (T) => Get (T, P) = First + P - 1);

   function Same_Integer (Left, Right : Integer) return Boolean is (Left = Right);
   function Identity (Item : Integer) return Integer is (Item);
   function Find is new Search (Key => Integer, Key_Of => Identity, Same_Key => Same_Integer);

   procedure Append_Checked (T : in out Table; Value : Integer) is
      Before : constant Table := T;
      Done : Boolean;
   begin
      Append (T, Value, Done);
      Check (Done);
      Check (Length (T) = Length (Before) + 1 and then Get (T, Length (T)) = Value);
      Check (for all P in 1 .. Length (Before) => Get (T, P) = Get (Before, P));
   end Append_Checked;

   T, Copy, Other : Table;
   Empty : Table;
   Done : Boolean;
begin
   --  Empty; growth through the first block and every doubling to the
   --  ceiling; one more is refused, unchanged.
   Check (Length (T) = 0 and T = Copy);
   for Value in 1 .. Count'Last loop
      Append_Checked (T, Value);
   end loop;
   Check (Length (T) = Count'Last and Counts_From (T, 1));
   Copy := T;
   Append (T, 99, Done);
   Check (not Done and T = Copy and Length (T) = Count'Last);

   --  Search: the first match, or 0.
   Check (Find (T, 1) = 1 and Find (T, Count'Last) = Count'Last and Find (T, 0) = 0);

   --  A copy appends in place at the tip; the original still sees only its
   --  own elements, and its own append then takes a private copy.
   declare
      Base, Left, Right : Table;
   begin
      for Value in 1 .. 10 loop Append_Checked (Base, Value); end loop;
      Left := Base;
      Right := Base;
      Append_Checked (Left, 11);
      Check (Length (Base) = 10 and Counts_From (Base, 1));
      Append_Checked (Right, 111);
      Check (Length (Left) = 11 and Get (Left, 11) = 11 and Counts_From (Left, 1));
      Check (Length (Right) = 11 and Get (Right, 11) = 111);
      Check (Left /= Right and Base /= Left);
      Append_Checked (Base, 12);
      Check (Get (Base, 11) = 12 and Get (Left, 11) = 11 and Get (Right, 11) = 111);
      --  Equal content in different storage is equal.
      Other := Empty;
      for Value in 1 .. 11 loop Append_Checked (Other, Value); end loop;
      Check (Other = Left);
   end;

   --  Replace: in place when sole, a private copy when shared.
   declare
      Base, Shared : Table;
   begin
      for Value in 1 .. 5 loop Append_Checked (Base, Value); end loop;
      Shared := Base;
      Replace (Shared, 3, 30, Done);
      Check (Done and Get (Shared, 3) = 30 and Length (Shared) = 5);
      Check (Get (Base, 3) = 3 and Counts_From (Base, 1));
      Check (for all P in 1 .. 5 => P = 3 or else Get (Shared, P) = P);
      Replace (Base, 5, 50, Done);
      Check (Done and Get (Base, 5) = 50 and Get (Shared, 5) = 5);
      --  A holder shorter than its storage's last written item: appending
      --  after the other holder is gone reuses the storage.
      Shared := Base;
      Append_Checked (Shared, 6);
      Shared := Empty;
      Append_Checked (Base, 7);
      Check (Length (Base) = 6 and Get (Base, 6) = 7 and Get (Base, 5) = 50);
   end;

   --  Copies and appends on several tasks at once: the counts of holders
   --  and the claim of the tip are atomic.
   declare
      Base : Table;
      Workers : constant := 8;
      Rounds : constant := 20_000;
      Failures : array (1 .. Workers) of Natural := [others => 0];
      task type Worker (Id : Positive);
      task body Worker is
         Mine : Table;
         Ok : Boolean;
      begin
         for Round in 1 .. Rounds loop
            Mine := Base;
            Append (Mine, Id * 1_000 + Round mod 1_000, Ok);
            if not Ok or else Length (Mine) /= 11 or else
              Get (Mine, 11) /= Id * 1_000 + Round mod 1_000 or else
              (for some P in 1 .. 10 => Get (Mine, P) /= P) or else
              Length (Base) /= 10
            then
               Failures (Id) := Failures (Id) + 1;
            end if;
         end loop;
      end Worker;
   begin
      for Value in 1 .. 10 loop Append_Checked (Base, Value); end loop;
      declare
         W1 : Worker (1); W2 : Worker (2); W3 : Worker (3); W4 : Worker (4);
         W5 : Worker (5); W6 : Worker (6); W7 : Worker (7); W8 : Worker (8);
      begin
         null;
      end;
      Check (for all F of Failures => F = 0);
      Check (Length (Base) = 10 and Counts_From (Base, 1));
   end;

   Ada.Text_IO.Put_Line ("CCL growing tables: PASS" & Checks'Image & " checks");
end Main;
