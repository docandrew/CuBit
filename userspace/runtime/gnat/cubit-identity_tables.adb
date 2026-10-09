------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package body CuBit.Identity_Tables is
   Keys : array (Valid_Index) of Process_ID := [others => No_Process];   --  free
   Used : Natural := 0;

   function Home (Key : Process_ID) return Valid_Index is
     (Valid_Index (Shift_Right (Hash (Key), 64 - Capacity_Bits)) + 1);

   function Next (I : Valid_Index) return Valid_Index is
     (if I = Capacity then 1 else I + 1);

   function Find (Key : Process_ID) return Index is
      I : Valid_Index := Home (Key);
   begin
      if Key = No_Process then
         return 0;
      end if;
      for Probe in 1 .. Capacity loop
         if Keys (I) = Key then
            return I;
         elsif Keys (I) = No_Process then
            return 0;
         end if;
         I := Next (I);
      end loop;
      return 0;
   end Find;

   procedure Ensure (Key : Process_ID; At_Index : out Index) is
      I : Valid_Index := Home (Key);
   begin
      At_Index := 0;
      if Key = No_Process then
         return;
      end if;
      for Probe in 1 .. Capacity loop
         if Keys (I) = Key then
            At_Index := I;
            return;
         elsif Keys (I) = No_Process then
            Keys (I) := Key;
            Values (I) := Empty;
            Used := Used + 1;
            At_Index := I;
            return;
         end if;
         I := Next (I);
      end loop;
   end Ensure;

   function Ensure (Key : Process_ID) return Index is
      Result : Index;
   begin
      Ensure (Key, Result);
      return Result;
   end Ensure;

   --  Backward-shift deletion: move later entries of the same probe run
   --  into the gap, so no lookup ever stops early at it.
   procedure Remove (Key : Process_ID) is
      Gap : Index := Find (Key);
      I : Valid_Index;
      H : Valid_Index;
      function Between (Low, X, High : Valid_Index) return Boolean is
        (if Low <= High then X > Low and then X <= High
         else X > Low or else X <= High);
   begin
      if Gap = 0 then
         return;
      end if;
      Keys (Gap) := No_Process;
      Values (Gap) := Empty;
      Used := Used - 1;
      I := Next (Gap);
      while Keys (I) /= No_Process loop
         H := Home (Keys (I));
         --  The entry at I may move to Gap unless its home lies in
         --  (Gap, I]: then it would no longer be reachable from home.
         if not Between (Gap, H, I) then
            Keys (Gap) := Keys (I);
            Values (Gap) := Values (I);
            Keys (I) := No_Process;
            Values (I) := Empty;
            Gap := I;
         end if;
         I := Next (I);
      end loop;
   end Remove;

   function Count return Natural is (Used);
end CuBit.Identity_Tables;
