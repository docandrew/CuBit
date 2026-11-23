------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

package body CCL.Growing_Tables with SPARK_Mode => Off is
   use type Interfaces.Unsigned_32;

   procedure Free is new Ada.Unchecked_Deallocation (Block, Block_Access);

   --  The storage a table's first element gets.
   FIRST_CAPACITY : constant := 16;
   --  GCC's __ATOMIC_SEQ_CST memory model.
   SEQUENTIALLY_CONSISTENT : constant := 5;

   function Add_Fetch
     (Target : not null access Interfaces.Unsigned_32; Value : Interfaces.Unsigned_32;
      Model : Integer) return Interfaces.Unsigned_32
     with Import, Convention => Intrinsic, External_Name => "__atomic_add_fetch_4";
   function Sub_Fetch
     (Target : not null access Interfaces.Unsigned_32; Value : Interfaces.Unsigned_32;
      Model : Integer) return Interfaces.Unsigned_32
     with Import, Convention => Intrinsic, External_Name => "__atomic_sub_fetch_4";
   function Load
     (Target : not null access Interfaces.Unsigned_32; Model : Integer)
      return Interfaces.Unsigned_32
     with Import, Convention => Intrinsic, External_Name => "__atomic_load_4";
   function Compare_Exchange
     (Target : not null access Interfaces.Unsigned_32;
      Expected : not null access Interfaces.Unsigned_32;
      Desired : Interfaces.Unsigned_32; Weak : Boolean;
      Success_Model, Failure_Model : Integer) return Boolean
     with Import, Convention => Intrinsic, External_Name => "__atomic_compare_exchange_4";
   procedure Store
     (Target : not null access Interfaces.Unsigned_32; Value : Interfaces.Unsigned_32;
      Model : Integer)
     with Import, Convention => Intrinsic, External_Name => "__atomic_store_4";

   --  Whether Item is the only holder of its block.
   function Sole (Item : Table) return Boolean is
     (Load (Item.Data.Holders'Access, SEQUENTIALLY_CONSISTENT) = 1);

   --  Drop this table's hold on its storage; the last holder frees it.
   procedure Release (Data : in out Block_Access) is
   begin
      if Data /= null then
         if Sub_Fetch (Data.Holders'Access, 1, SEQUENTIALLY_CONSISTENT) = 0 then
            Free (Data);
         end if;
         Data := null;
      end if;
   end Release;

   overriding procedure Adjust (Item : in out Table) is
      Ignored : Interfaces.Unsigned_32;
   begin
      if Item.Data /= null then
         Ignored := Add_Fetch (Item.Data.Holders'Access, 1, SEQUENTIALLY_CONSISTENT);
      end if;
   end Adjust;

   overriding procedure Finalize (Item : in out Table) is
   begin
      Release (Item.Data);
      Item.Used := 0;
   end Finalize;

   function Length (Item : Table) return Count is (Item.Used);

   function Get (Item : Table; Position : Index) return Element is
     (Item.Data.Items (Position));

   function Search (Item : Table; Wanted : Key) return Count is
   begin
      for P in 1 .. Item.Used loop
         if Same_Key (Key_Of (Item.Data.Items (P)), Wanted) then
            return P;
         end if;
      end loop;
      return 0;
   end Search;

   function "=" (Left, Right : Table) return Boolean is
   begin
      if Left.Used /= Right.Used then
         return False;
      elsif Left.Used = 0 or else Left.Data = Right.Data then
         return True;
      end if;
      return Left.Data.Items (1 .. Left.Used) = Right.Data.Items (1 .. Right.Used);
   end "=";

   --  Make Item's storage its own, with room for Room elements: as it is
   --  when no other table holds it and it is large enough, otherwise a new
   --  block (doubling, up to Ceiling) holding the elements in use.
   procedure Own (Item : in out Table; Room : Index; Done : out Boolean) is
      Fresh : Block_Access;
      Capacity : Index;
   begin
      Done := False;
      if Item.Data /= null and then Sole (Item) and then Item.Data.Capacity >= Room then
         Done := True;
         return;
      end if;
      if Item.Data = null then
         Capacity := Index'Min (Ceiling, Index'Max (Room, FIRST_CAPACITY));
      elsif Item.Data.Capacity >= Room then
         Capacity := Item.Data.Capacity;
      elsif Item.Data.Capacity > Ceiling / 2 then
         Capacity := Ceiling;
      else
         Capacity := Index'Max (Room, 2 * Item.Data.Capacity);
      end if;
      Fresh := new Block (Capacity);
      if Item.Used > 0 then
         Fresh.Items (1 .. Item.Used) := Item.Data.Items (1 .. Item.Used);
      end if;
      Fresh.Written := Interfaces.Unsigned_32 (Item.Used);
      Release (Item.Data);
      Item.Data := Fresh;
      Done := True;
   exception
      when Storage_Error =>
         Done := False;
   end Own;

   --  Claim item Item.Used + 1 of a shared block for Item: only when Item
   --  uses every item written so far and the block has room.
   function Claim_Next (Item : Table) return Boolean is
      Expected : aliased Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Item.Used);
   begin
      return Item.Data /= null and then Item.Used < Item.Data.Capacity and then
        Compare_Exchange
          (Item.Data.Written'Access, Expected'Access, Interfaces.Unsigned_32 (Item.Used + 1),
           False, SEQUENTIALLY_CONSISTENT, SEQUENTIALLY_CONSISTENT);
   end Claim_Next;

   procedure Append (Item : in out Table; Value : Element; Done : out Boolean) is
   begin
      Done := False;
      if Item.Used = Ceiling then
         return;
      end if;
      if Item.Data /= null and then Sole (Item) and then Item.Used < Item.Data.Capacity then
         Store (Item.Data.Written'Access, Interfaces.Unsigned_32 (Item.Used + 1), SEQUENTIALLY_CONSISTENT);
         Done := True;
      elsif Claim_Next (Item) then
         Done := True;
      else
         Own (Item, Item.Used + 1, Done);
         if Done then
            Store (Item.Data.Written'Access, Interfaces.Unsigned_32 (Item.Used + 1), SEQUENTIALLY_CONSISTENT);
         end if;
      end if;
      if Done then
         Item.Used := Item.Used + 1;
         Item.Data.Items (Item.Used) := Value;
      end if;
   end Append;

   procedure Replace
     (Item : in out Table; Position : Index; Value : Element; Done : out Boolean) is
   begin
      Own (Item, Item.Used, Done);
      if Done then
         --  Sole holder now: items past Item.Used are nobody's.
         Store (Item.Data.Written'Access, Interfaces.Unsigned_32 (Item.Used), SEQUENTIALLY_CONSISTENT);
         Item.Data.Items (Position) := Value;
      end if;
   end Replace;
end CCL.Growing_Tables;
