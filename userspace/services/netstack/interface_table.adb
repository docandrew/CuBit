------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;

package body Interface_Table is

   --  CuAlloc's entry points (userspace/allocator, as System.Memory uses).
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate_zeroed";
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_reallocate";
   procedure Release (Item : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";

   No_Block : constant Unsigned_64 := 0;
   Element_Alignment : constant := 64;   --  a cache line

   package Conversions is new System.Address_To_Access_Conversions (Element);

   type Entry_Record is record
      Item  : Unsigned_64 := No_Block;       --  the element's block, or none
      Epoch : NL.Interface_Epoch := NL.Interface_Epoch'First;
   end record;
   type Directory is array (Natural range 0 .. Natural'Last / 2) of Entry_Record;
   type Directory_Access is access all Directory;
   for Directory_Access'Storage_Size use 0;
   package Directory_Conversions is new System.Address_To_Access_Conversions (Directory);

   Entry_Bytes : constant Unsigned_64 := Entry_Record'Size / Storage_Unit;
   Minimum_Slots : constant := 4;

   Base     : Unsigned_64 := No_Block;   --  the directory's block
   Capacity : Natural := 0;              --  its slots
   Used     : Natural := 0;              --  slots 0 .. Used - 1 exist
   Occupied : Natural := 0;
   Next_Epoch : NL.Interface_Epoch := NL.Interface_Epoch'First;

   function Slots return Directory_Access is
     (Directory_Access (Directory_Conversions.To_Pointer
        (To_Address (Integer_Address (Base)))));

   function Slot_Count return Natural is (Used);
   function Count return Natural is (Occupied);

   function In_Use (Slot : Natural) return Boolean is
     (Slot < Used and then Slots (Slot).Item /= No_Block);

   function Get (Slot : Natural) return Element_Access is
     (Element_Access (Conversions.To_Pointer
        (To_Address (Integer_Address (Slots (Slot).Item)))));

   function Handle_Of (Slot : Natural) return NL.Interface_Handle is
     ((Slot => NL.Interface_Slot (Slot), Epoch => Slots (Slot).Epoch));

   --  Make room for New_Capacity slots; False if the heap refused.
   function Resize (New_Capacity : Natural) return Boolean is
      Grown : constant Unsigned_64 :=
        (if Base = No_Block
         then Allocate_Zeroed (Unsigned_64 (New_Capacity) * Entry_Bytes, Element_Alignment)
         else Reallocate (Base, Unsigned_64 (New_Capacity) * Entry_Bytes));
   begin
      if Grown = No_Block then
         return False;
      end if;
      Base := Grown;
      for S in Capacity .. New_Capacity - 1 loop
         Slots (S) := (others => <>);
      end loop;
      Capacity := New_Capacity;
      return True;
   end Resize;

   procedure Insert
     (Item : Element; Handle : out NL.Interface_Handle; Inserted : out Boolean)
   is
      Slot  : Natural := 0;
      Block : Unsigned_64;
   begin
      Handle := (others => <>);
      Inserted := False;
      --  The lowest free slot, else a new one.
      while Slot < Used and then Slots (Slot).Item /= No_Block loop
         Slot := Slot + 1;
      end loop;
      if Slot = Capacity and then
        not Resize (Natural'Max (Minimum_Slots, 2 * Capacity))
      then
         return;
      end if;
      Block := Allocate_Zeroed (Element'Size / Storage_Unit, Element_Alignment);
      if Block = No_Block then
         return;
      end if;
      Conversions.To_Pointer (To_Address (Integer_Address (Block))).all := Item;
      Slots (Slot) := (Item => Block, Epoch => Next_Epoch);
      Next_Epoch := (if Next_Epoch = NL.Interface_Epoch'Last then NL.Interface_Epoch'First
                     else Next_Epoch + 1);
      if Slot = Used then
         Used := Used + 1;
      end if;
      Occupied := Occupied + 1;
      Handle := Handle_Of (Slot);
      Inserted := True;
   end Insert;

   procedure Find (Handle : NL.Interface_Handle; Slot : out Natural; Found : out Boolean) is
   begin
      Slot := 0;
      Found := Handle.Slot < NL.Interface_Slot (Used) and then
        Slots (Natural (Handle.Slot)).Item /= No_Block and then
        Slots (Natural (Handle.Slot)).Epoch = Handle.Epoch;
      if Found then
         Slot := Natural (Handle.Slot);
      end if;
   end Find;

   procedure Remove (Slot : Natural) is
   begin
      if not In_Use (Slot) then
         return;
      end if;
      Release (Slots (Slot).Item);
      Slots (Slot).Item := No_Block;
      Occupied := Occupied - 1;
      while Used > 0 and then Slots (Used - 1).Item = No_Block loop
         Used := Used - 1;
      end loop;
      --  Give memory back once the directory is a quarter used.
      if Capacity > Minimum_Slots and then Used <= Capacity / 4 then
         declare
            Ignore : constant Boolean := Resize (Natural'Max (Minimum_Slots, Capacity / 2));
         begin
            null;   --  a refused shrink keeps the larger directory
         end;
      end if;
   end Remove;

end Interface_Table;
