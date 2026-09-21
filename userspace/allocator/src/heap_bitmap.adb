package body Heap_Bitmap with SPARK_Mode is
   type Slot_Number is mod 2 ** 32;
   function Word_Of (Item : Slot) return Word_Index is
   begin
      return Word_Index (Slot_Number (Item - 1) / Bits_Per_Word);
   end Word_Of;
   function Bit_Of (Item : Slot) return Bit_Index is
   begin
      return Bit_Index (Slot_Number (Item - 1) mod Bits_Per_Word);
   end Bit_Of;

   function Live (State : Pool; Item : Slot) return Boolean is (Bit (State.Bits, Item));

   procedure Prove_Uniform (Bits : Bitmap; Prefix : Count; Value : Boolean) with Ghost,
     Pre => (for all I in 1 .. Prefix => Bit (Bits, I) = Value),
     Post => Population (Bits, Prefix) = (if Value then Prefix else 0),
     Subprogram_Variant => (Decreases => Prefix)
   is
   begin
      if Prefix > 0 then Prove_Uniform (Bits, Prefix - 1, Value); end if;
   end Prove_Uniform;

   procedure Prove_Change (Before, After : Bitmap; Item : Slot; Prefix : Count) with Ghost,
     Pre => (for all I in Slot => (if I /= Item then Bit (Before, I) = Bit (After, I))),
     Post => Integer (Population (After, Prefix)) = Integer (Population (Before, Prefix)) +
       (if Item <= Prefix then (if Bit (After, Item) then 1 else 0) -
          (if Bit (Before, Item) then 1 else 0) else 0),
     Subprogram_Variant => (Decreases => Prefix)
   is
   begin
      if Prefix > 0 then Prove_Change (Before, After, Item, Prefix - 1); end if;
   end Prove_Change;

   procedure Prove_Bit (Bits : Bitmap; Item : Slot; Prefix : Count) with Ghost,
     Pre => Item <= Prefix,
     Post => (if Bit (Bits, Item) then Population (Bits, Prefix) > 0
              else Population (Bits, Prefix) < Prefix),
     Subprogram_Variant => (Decreases => Prefix)
   is
   begin
      if Item < Prefix then Prove_Bit (Bits, Item, Prefix - 1); end if;
   end Prove_Bit;

   procedure Initialize (State : out Pool; Limit : Slot) is
   begin
      State := (Bits => [others => [others => False]], Limit => Limit,
                Occupied => 0, Cached => 0, Free => [others => 1]);
      Prove_Uniform (State.Bits, Limit, False);
   end Initialize;

   -- Refill only an empty cache, so no cached slot can be inserted twice.
   -- Slots omitted from a full cache remain free in the membership bitmap.
   procedure Refill (State : in out Pool) with No_Inline,
     Pre => Valid (State) and then State.Cached = 0 and then State.Occupied < State.Limit,
     Post => Valid (State) and then State.Cached > 0 and then
       State.Bits = State.Bits'Old and then State.Limit = State.Limit'Old and then
       State.Occupied = State.Occupied'Old
   is
   begin
      for I in 1 .. State.Limit loop
         if not Bit (State.Bits, I) then
            State.Cached := State.Cached + 1;
            State.Free (State.Cached) := Stored_Slot (I);
            if State.Cached = Cache_Size then return; end if;
         end if;
         pragma Loop_Invariant (Valid (State));
         pragma Loop_Invariant (State.Cached < Cache_Size);
         pragma Loop_Invariant (for all J in 1 .. State.Cached => State.Free (J) <= Stored_Slot (I));
         pragma Loop_Invariant
           (if State.Cached = 0 then (for all J in 1 .. I => Bit (State.Bits, J)));
      end loop;
      if State.Cached = 0 then Prove_Uniform (State.Bits, State.Limit, True); end if;
   end Refill;

   procedure Allocate (State : in out Pool; Item : out Slot; Success : out Boolean) is
      Before : constant Bitmap := State.Bits with Ghost;
   begin
      Item := Slot'First;
      Success := State.Occupied < State.Limit;
      if Success then
         if State.Cached = 0 then Refill (State); end if;
         Item := Slot (State.Free (State.Cached));
         State.Cached := State.Cached - 1;
         State.Bits (Word_Of (Item)) (Bit_Of (Item)) := True;
         Prove_Change (Before, State.Bits, Item, State.Limit);
         State.Occupied := State.Occupied + 1;
      end if;
   end Allocate;

   procedure Release (State : in out Pool; Item : Slot; Success : out Boolean) is
      Before : constant Bitmap := State.Bits with Ghost;
   begin
      Success := Bit (State.Bits, Item);
      if Success then
         State.Bits (Word_Of (Item)) (Bit_Of (Item)) := False;
         Prove_Change (Before, State.Bits, Item, State.Limit);
         State.Occupied := State.Occupied - 1;
         -- Prefer the latest return even when full. The displaced cached slot
         -- stays free in Bits and will be found by a later refill.
         if State.Cached < Cache_Size then State.Cached := State.Cached + 1; end if;
         State.Free (State.Cached) := Stored_Slot (Item);
      end if;
   end Release;

   procedure Reconfigure (State : in out Pool; Limit : Slot; Success : out Boolean) is
   begin
      Success := State.Occupied = 0;
      if Success then
         for I in 1 .. State.Limit loop
            if Bit (State.Bits, I) then Prove_Bit (State.Bits, I, State.Limit); end if;
            pragma Loop_Invariant (for all J in 1 .. I => not Bit (State.Bits, J));
         end loop;
         Initialize (State, Limit);
      end if;
   end Reconfigure;
end Heap_Bitmap;
