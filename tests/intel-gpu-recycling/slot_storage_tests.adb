with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Slot_Storage;
with Intel_GPU_Session_Store;
with Intel_GPU_Session_Values;
-- Growable per-session storage (Linux-hosted, the production generics over
-- a model of owned reservations): pages are committed only as the table
-- grows, a refused commit grows nothing, new and reset slots hold the
-- element type's own defaults (never leftover bytes), and committed slots
-- never move.
procedure Slot_Storage_Tests is
   Page : constant := 4096;
   Reserved_Bytes : constant := 64 * Page;
   type Memory is array (1 .. Reserved_Bytes + Page) of Unsigned_8;
   type Memory_Access is access Memory;
   Arena : constant Memory_Access := new Memory'(others => 16#AA#);
   Base : Unsigned_64;
   Committed : Unsigned_64 := 0;
   Refuse_Commit : Boolean := False;
   Reservations : Natural := 0;
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
      Start : constant Unsigned_64 :=
        Unsigned_64 (To_Integer (Arena.all'Address));
   begin
      if Reservations > 0 or else Bytes > Reserved_Bytes then return 0; end if;
      Reservations := Reservations + 1;
      Base := Start + (Page - Start mod Page) mod Page;
      return Base;
   end Reserve;
   function Commit (Region_Base, Offset, Bytes : Unsigned_64) return Boolean is
   begin
      if Refuse_Commit or else Region_Base /= Base or else Offset /= Committed then
         return False;
      end if;
      -- Owned memory commits zero-filled pages.
      declare
         Pages : Storage_Array (1 .. Storage_Offset (Bytes))
           with Import, Address => To_Address (Integer_Address (Base + Offset));
      begin
         Pages := [others => 0];
      end;
      Committed := Committed + Bytes;
      return True;
   end Commit;
   package Storage is new Intel_GPU_Slot_Storage (Reserve, Commit, 2 * Page);
   subtype Slot is Positive range 1 .. 1_000;
   type Item is limited record
      Marker : Unsigned_32 := 7;
      Count : Natural := 3;
      Flag : Boolean := True;
      Pad : Storage_Array (1 .. 100) := [others => 1];
      -- No default: a reused slot must not keep its last session's value.
      Raw : Unsigned_32;
   end record;
   package Items is new Intel_GPU_Session_Store (Storage, Slot, Item);
   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1; Put_Line ("FAIL: " & What);
      end if;
   end Check;
   function Fresh (I : Slot) return Boolean is
     (Items.Items (I).Marker = 7 and then Items.Items (I).Count = 3 and then
      Items.Items (I).Flag and then (for all B of Items.Items (I).Pad => B = 1) and then
      Items.Items (I).Raw = 0);
   use type System.Address;
   OK : Boolean;
   First_Address : System.Address;
   Element_Bytes : constant Unsigned_64 := Unsigned_64 (Items.Element_Array'Component_Size / 8);
begin
   Check (Storage.Valid (Items.Region), "the region reserved address space");
   Check (Committed = 0 and Items.Grown = 0, "nothing committed before growth");
   Items.Grow (8, OK);
   Check (OK and Items.Grown = 8, "grows");
   Check (Committed >= 8 * Element_Bytes and Committed mod Page = 0 and Committed <= 2 * Page,
          "commits only what the grown slots need, in pages");
   for I in 1 .. 8 loop Check (Fresh (I), "a new slot has the type's defaults"); end loop;
   First_Address := Items.Items (1)'Address;
   Items.Items (3).Marker := 99; Items.Items (3).Pad (5) := 0; Items.Items (3).Flag := False;
   Items.Items (3).Raw := 5;
   Items.Reset (3);
   Check (Fresh (3), "reset restores the defaults");
   Items.Items (4).Count := 11;
   Refuse_Commit := True;
   Items.Grow (200, OK);
   Check (not OK and Items.Grown = 8, "a refused commit grows nothing");
   Refuse_Commit := False;
   Items.Grow (200, OK);
   Check (OK and Items.Grown = 200, "grows after memory frees up");
   Check (Items.Items (1)'Address = First_Address and Items.Items (4).Count = 11,
          "growth never moves or touches committed slots");
   for I in 9 .. 200 loop Check (Fresh (I), "slots committed later have the defaults"); end loop;
   Items.Grow (100, OK);
   Check (OK and Items.Grown = 200, "never shrinks");
   -- The reservation covers every slot the type can name.
   Items.Grow (Slot'Last, OK);
   Check (OK and Items.Grown = Slot'Last and Committed <= Items.Region.Reserved,
          "every nameable slot fits the reservation");
   Check (Fresh (Slot'Last) and Items.Items (4).Count = 11, "the last slot is fresh");
   if Failures = 0 then
      Put_Line ("Slot storage PASS:" & Natural'Image (Checks) & " checks: commit on demand, defaults, reset, refusal, stability");
   else
      Put_Line ("Slot storage FAIL:" & Natural'Image (Failures));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Slot_Storage_Tests;
