--  CuBit.Identity_Tables (procmgr's process and launch tables), hosted with
--  run-time checks on. Regression: a key whose hash's top Capacity_Bits are
--  all zero has home slot 1; Home converted that 0 to Valid_Index before
--  adding one, so procmgr died (Constraint_Error, "cubit-identity_tables.adb
--  line 13") on the first such child, about one new process in 512
--  (seen in the device-build test, 2026-11-06).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Identity_Tables;
with CuBit.Process_IDs; use CuBit.Process_IDs;

procedure Main is
   Bits : constant := 9;
   package Table is new CuBit.Identity_Tables
     (Element => Natural, Empty => 0, Capacity_Bits => Bits);
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   function Top (P : Process_ID) return Unsigned_64 is (Shift_Right (Hash (P), 64 - Bits));

   --  Identities (slot, generation 1) with the given top hash bits.
   function With_Top (Wanted : Unsigned_64; Skip : Natural := 0) return Process_ID is
      Seen : Natural := 0;
   begin
      for Slot in Slot_Number range 1 .. Last_Slot loop
         if Top (To_Process_ID (Slot, 1)) = Wanted then
            if Seen = Skip then
               return To_Process_ID (Slot, 1);
            end if;
            Seen := Seen + 1;
         end if;
      end loop;
      return No_Process;
   end With_Top;

   First_Home : constant Process_ID := With_Top (0);
   Second_Home : constant Process_ID := With_Top (0, 1);
   Last_Home : constant Process_ID := With_Top (2 ** Bits - 1);
   At_Index : Table.Index;
begin
   Check (First_Home /= No_Process and then Second_Home /= No_Process and then Last_Home /= No_Process,
          "keys homed at both ends of the table exist");
   Table.Ensure (First_Home, At_Index);
   Check (At_Index = 1, "a key whose hash's top bits are zero lives at slot 1");
   Table.Values (At_Index) := 7;
   Check (Table.Find (First_Home) = 1 and then Table.Values (Table.Find (First_Home)) = 7, "and is found there");
   --  Wrapping: the last slot's run continues at slot 1.
   Table.Ensure (Last_Home, At_Index);
   Check (At_Index = Table.Capacity, "a key homed at the last slot lives there");
   Table.Ensure (Second_Home, At_Index);
   Check (At_Index = 2, "a second key homed at slot 1 probes on to slot 2");
   Table.Remove (First_Home);
   Check (Table.Find (First_Home) = 0 and then Table.Find (Second_Home) = 1,
          "removing slot 1 shifts its probe run back");
   Check (Table.Find (Last_Home) = Table.Capacity and then Table.Count = 2, "the rest stays");
   Table.Remove (Second_Home);
   Table.Remove (Last_Home);
   Check (Table.Count = 0, "empty again");
   if Failures = 0 then
      Put_Line ("identity-tables: PASS");
   else
      Put_Line ("identity-tables: FAIL" & Failures'Image);
      raise Program_Error;
   end if;
end Main;
