pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

package body Source_Identities is
   package LS renames CuBit.Log_Sources;
   use type LS.Entry_Kind;

   --  CuBit's process heap (userspace/allocator, as System.Memory uses it):
   --  zero when it refuses.
   function CuAlloc_Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
   with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free (Block : Unsigned_64)
   with Import, Convention => C, External_Name => "cualloc_free";

   --  The first block's entries: an allocation hint, never a limit.
   Initial_Capacity : constant := 32;
   Entry_Bytes : constant Unsigned_64 := Known_Array'Component_Size / System.Storage_Unit;
   Entry_Alignment : constant Unsigned_64 := Known'Alignment;

   function Address_Of (Block : Unsigned_64) return System.Address is
     (To_Address (Integer_Address (Block)));

   function Find (Item : Table; Source : Unsigned_64) return Natural is
      Data : constant Known_Array (1 .. Item.Capacity)
        with Import, Address => Address_Of (Item.Block);
   begin
      for I in 1 .. Item.Used loop
         if Data (I).Listed.Who.Source = Source then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   procedure Remove (Item : in out Table; Position : Positive) is
      Data : Known_Array (1 .. Item.Capacity) with Import, Address => Address_Of (Item.Block);
   begin
      Data (Position .. Item.Used - 1) := Data (Position + 1 .. Item.Used);
      Item.Used := Item.Used - 1;
   end Remove;

   --  Room for one more entry: the block doubles (at least Initial_Capacity),
   --  its entries copied over. Grown False when the heap refuses; the table
   --  is then as it was.
   procedure Make_Room (Item : in out Table; Grown : out Boolean) is
      Wanted : Natural;
      Larger : Unsigned_64;
   begin
      Grown := Item.Used < Item.Capacity;
      if Grown or else Item.Capacity > Natural'Last / 2 then
         return;
      end if;
      Wanted := Natural'Max (Initial_Capacity, Item.Capacity * 2);
      Larger := CuAlloc_Allocate (Unsigned_64 (Wanted) * Entry_Bytes, Entry_Alignment);
      if Larger = No_Block then
         return;
      end if;
      declare
         Old_Data : constant Known_Array (1 .. Item.Capacity)
           with Import, Address => Address_Of (Item.Block);
         New_Data : Known_Array (1 .. Wanted) with Import, Address => Address_Of (Larger);
      begin
         New_Data (1 .. Item.Used) := Old_Data (1 .. Item.Used);
      end;
      if Item.Block /= No_Block then
         CuAlloc_Free (Item.Block);
      end if;
      Item.Block := Larger;
      Item.Capacity := Wanted;
      Grown := True;
   end Make_Room;

   procedure Started (Item : in out Table; Who : LS.Source_Identity; Now_Ms : Unsigned_64) is
      Earlier : constant Natural := Find (Item, Who.Source);
      Grown : Boolean;
   begin
      if Earlier /= 0 then
         Remove (Item, Earlier);
      end if;
      Make_Room (Item, Grown);
      if not Grown then
         if Item.Given_Up < Unsigned_64'Last then
            Item.Given_Up := Item.Given_Up + 1;
         end if;
         return;
      end if;
      declare
         Data : Known_Array (1 .. Item.Capacity) with Import, Address => Address_Of (Item.Block);
      begin
         Item.Used := Item.Used + 1;
         Data (Item.Used) := (Listed => (Kind => LS.Started, Who => Who, Seen_Ms => Now_Ms),
                              Exited_Ms => 0);
      end;
   end Started;

   procedure Exited (Item : in out Table; Source : Unsigned_64; Now_Ms : Unsigned_64) is
      Position : constant Natural := Find (Item, Source);
      Data : Known_Array (1 .. Item.Capacity) with Import, Address => Address_Of (Item.Block);
   begin
      if Position /= 0 and then Data (Position).Listed.Kind = LS.Started then
         Data (Position).Listed.Kind := LS.Exited;
         Data (Position).Exited_Ms := Now_Ms;
      end if;
   end Exited;

   procedure Prune (Item : in out Table; Store : Log_Fanout.Broker; Now_Ms : Unsigned_64) is
      Position : Positive := 1;
      Data : constant Known_Array (1 .. Item.Capacity)
        with Import, Address => Address_Of (Item.Block);
   begin
      while Position <= Item.Used loop
         if Data (Position).Listed.Kind = LS.Exited
           and then Now_Ms >= Data (Position).Exited_Ms
           and then Now_Ms - Data (Position).Exited_Ms >= Exit_Grace_Ms
           and then not Log_Fanout.Holds_History (Store, Data (Position).Listed.Who.Source)
         then
            Remove (Item, Position);
         else
            Position := Position + 1;
         end if;
      end loop;
   end Prune;

   function Count (Item : Table) return Natural is (Item.Used);

   function Newest (Item : Table; Position : Positive) return LS.Source_Entry is
      Data : constant Known_Array (1 .. Item.Capacity)
        with Import, Address => Address_Of (Item.Block);
   begin
      return Data (Item.Used - Position + 1).Listed;
   end Newest;

   function Lost (Item : Table) return Unsigned_64 is (Item.Given_Up);
end Source_Identities;
