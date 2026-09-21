-- Single-owner slab slots: compact membership, exact accounting, no payload links.
package Heap_Bitmap with SPARK_Mode, Pure is
   Max_Slots : constant := 4_096;
   subtype Slot is Positive range 1 .. Max_Slots;
   subtype Count is Natural range 0 .. Max_Slots;
   type Pool is private;
   function Valid (State : Pool) return Boolean with Ghost;
   function Capacity (State : Pool) return Slot;
   function Used (State : Pool) return Count;
   function Live (State : Pool; Item : Slot) return Boolean with Inline_Always;

   procedure Initialize (State : out Pool; Limit : Slot) with
     Post => Valid (State) and then Capacity (State) = Limit and then Used (State) = 0
       and then (for all I in Slot => not Live (State, I));
   procedure Allocate (State : in out Pool; Item : out Slot; Success : out Boolean) with Inline_Always,
     Pre => Valid (State),
     Post => Valid (State) and then Capacity (State) = Capacity (State'Old) and then
       Success = (Used (State'Old) < Capacity (State'Old)) and then
       (if Success then Item <= Capacity (State) and then
          Used (State) = Used (State'Old) + 1 and then
          not Live (State'Old, Item) and then Live (State, Item) and then
          (for all I in Slot => (if I /= Item then Live (State, I) = Live (State'Old, I)))
        else State = State'Old);
   procedure Release (State : in out Pool; Item : Slot; Success : out Boolean) with Inline_Always,
     Pre => Valid (State),
     Post => Valid (State) and then Capacity (State) = Capacity (State'Old) and then
       Success = Live (State'Old, Item) and then
       (if Success then Used (State) = Used (State'Old) - 1 and then
          not Live (State, Item) and then
          (for all I in Slot => (if I /= Item then Live (State, I) = Live (State'Old, I)))
        else State = State'Old);

   -- The lifecycle gate: no capacity change while even one live slot remains.
   procedure Reconfigure (State : in out Pool; Limit : Slot; Success : out Boolean) with
     Pre => Valid (State),
     Post => Valid (State) and then Success = (Used (State'Old) = 0) and then
       (if Success then Capacity (State) = Limit and then Used (State) = 0 and then
          (for all I in Slot => not Live (State'Old, I) and then not Live (State, I))
        else State = State'Old);
private
   Bits_Per_Word : constant := 64;
   subtype Word_Index is Natural range 0 .. Max_Slots / Bits_Per_Word - 1;
   subtype Bit_Index is Natural range 0 .. Bits_Per_Word - 1;
   type Word_Bits is array (Bit_Index) of Boolean with Pack, Size => 64;
   type Bitmap is array (Word_Index) of Word_Bits;
   -- Conversion is bounded by Slot; unsigned division/remainder avoids
   -- signed negative-index correction in unchecked release code.
   function Word_Of (Item : Slot) return Word_Index with Inline_Always,
     Post => Word_Of'Result = (Item - 1) / Bits_Per_Word;
   function Bit_Of (Item : Slot) return Bit_Index with Inline_Always,
     Post => Bit_Of'Result = (Item - 1) mod Bits_Per_Word;
   function Bit (Bits : Bitmap; Item : Slot) return Boolean is
     (Bits (Word_Of (Item)) (Bit_Of (Item))) with Inline_Always;
   type Stored_Slot is range 1 .. Max_Slots with Size => 16;
   Cache_Size : constant := 64;
   subtype Cache_Count is Natural range 0 .. Cache_Size;
   subtype Cache_Index is Positive range 1 .. Cache_Size;
   type Free_Stack is array (Cache_Index) of Stored_Slot with Component_Size => 16;
   -- Keep counters apart to avoid costly paired SIMD updates on the tested
   -- compiler. A power-of-two object stride simplifies slab address arithmetic;
   -- padding is deliberate, not additional usable cache capacity.
   type Pool is record
      Bits : Bitmap;
      Limit : Slot;
      Occupied : Count;
      Free : Free_Stack;
      Cached : Cache_Count;
   end record with Object_Size => 1_024 * 8;
   function Population (Bits : Bitmap; Prefix : Count) return Count is
     (if Prefix = 0 then 0 else Population (Bits, Prefix - 1) +
        (if Bit (Bits, Prefix) then 1 else 0)) with Ghost,
     Post => Population'Result <= Prefix,
     Subprogram_Variant => (Decreases => Prefix);
   function Valid (State : Pool) return Boolean is
     (State.Occupied <= State.Limit and then
      State.Occupied = Population (State.Bits, State.Limit) and then
      (for all I in Slot => (if I > State.Limit then not Bit (State.Bits, I))) and then
      (for all I in 1 .. State.Cached =>
         Slot (State.Free (I)) <= State.Limit and then
         not Bit (State.Bits, Slot (State.Free (I))) and then
         (for all J in 1 .. I - 1 => State.Free (I) /= State.Free (J))));
   function Capacity (State : Pool) return Slot is (State.Limit);
   function Used (State : Pool) return Count is (State.Occupied);
end Heap_Bitmap;
