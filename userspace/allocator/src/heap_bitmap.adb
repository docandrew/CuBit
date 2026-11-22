package body Heap_Bitmap with SPARK_Mode is
   type Granule_Number is mod 2 ** 32;
   function Word_Of (G : Granule) return Word_Index is
   begin
      return Word_Index (Granule_Number (G - 1) / Bits_Per_Word);
   end Word_Of;
   function Bit_Of (G : Granule) return Bit_Index is
   begin
      return Bit_Index (Granule_Number (G - 1) mod Bits_Per_Word);
   end Bit_Of;
   -- Position's granule (unsigned: no sign correction in the code).
   function Granule_Of (Position : Local) return Granule with Inline_Always,
     Post => Granule_Of'Result = Position / Granule_Bytes + 1
   is
   begin
      return Granule (Granule_Number (Position) / Granule_Bytes + 1);
   end Granule_Of;

   procedure Prove_Uniform (Bits : Bitmap; C : Size_Class; Prefix : Count; Value : Boolean) with Ghost,
     Pre => Prefix <= Slots (C) and then (for all I in 1 .. Prefix => Bit (Bits, Grain (C, I)) = Value),
     Post => Population (Bits, C, Prefix) = (if Value then Prefix else 0),
     Subprogram_Variant => (Decreases => Prefix)
   is
   begin
      if Prefix > 0 then Prove_Uniform (Bits, C, Prefix - 1, Value); end if;
   end Prove_Uniform;

   -- Distinct blocks have distinct first granules.
   procedure Prove_Apart (C : Size_Class; I, J : Slot) with Ghost,
     Pre => I <= Slots (C) and then J <= Slots (C) and then I /= J,
     Post => Grain (C, I) /= Grain (C, J)
   is
   begin
      if I < J then Prove_Order (I - 1, J - 1, C); else Prove_Order (J - 1, I - 1, C); end if;
   end Prove_Apart;

   -- No other block shares Item's first granule.
   procedure Prove_Unique (C : Size_Class; Item : Slot) with Ghost,
     Pre => Item <= Slots (C),
     Post => (for all I in 1 .. Slots (C) => (if I /= Item then Grain (C, I) /= Grain (C, Item)))
   is
   begin
      for I in 1 .. Slots (C) loop
         if I /= Item then Prove_Apart (C, I, Item); end if;
         pragma Loop_Invariant (for all J in 1 .. I => (if J /= Item then Grain (C, J) /= Grain (C, Item)));
      end loop;
   end Prove_Unique;

   procedure Prove_Change (Before, After : Bitmap; C : Size_Class; Item : Slot; Prefix : Count) with Ghost,
     Pre => Item <= Slots (C) and then Prefix <= Slots (C) and then
       (for all G in Granule => (if G /= Grain (C, Item) then Bit (Before, G) = Bit (After, G))),
     Post => Integer (Population (After, C, Prefix)) = Integer (Population (Before, C, Prefix)) +
       (if Item <= Prefix then (if Bit (After, Grain (C, Item)) then 1 else 0) -
          (if Bit (Before, Grain (C, Item)) then 1 else 0) else 0),
     Subprogram_Variant => (Decreases => Prefix)
   is
   begin
      if Prefix > 0 then
         Prove_Change (Before, After, C, Item, Prefix - 1);
         if Prefix /= Item then Prove_Apart (C, Prefix, Item); end if;
      end if;
   end Prove_Change;

   procedure Prove_Bit (Bits : Bitmap; C : Size_Class; Item : Slot; Prefix : Count) with Ghost,
     Pre => Prefix <= Slots (C) and then Item <= Prefix,
     Post => (if Bit (Bits, Grain (C, Item)) then Population (Bits, C, Prefix) > 0
              else Population (Bits, C, Prefix) < Prefix),
     Subprogram_Variant => (Decreases => Prefix)
   is
   begin
      if Item < Prefix then Prove_Bit (Bits, C, Item, Prefix - 1); end if;
   end Prove_Bit;

   procedure Prove_Empty (Bits : Bitmap; C : Size_Class; Prefix : Count) with Ghost,
     Pre => Prefix <= Slots (C) and then Population (Bits, C, Prefix) = 0,
     Post => (for all I in 1 .. Prefix => not Bit (Bits, Grain (C, I))),
     Subprogram_Variant => (Decreases => Prefix)
   is
   begin
      if Prefix > 0 then Prove_Empty (Bits, C, Prefix - 1); end if;
   end Prove_Empty;

   -- No block live, and only blocks' first granules set: no bit set.
   procedure Prove_Clear (Bits : Bitmap; C : Size_Class) with Ghost,
     Pre => (for all I in 1 .. Slots (C) => not Bit (Bits, Grain (C, I))) and then
       (for all G in Granule =>
          (if Bit (Bits, G) then ((G - 1) * Granule_Bytes) mod Stride (C) = 0 and then
             ((G - 1) * Granule_Bytes) / Stride (C) < Slots (C))),
     Post => (for all G in Granule => not Bit (Bits, G))
   is
   begin
      for G in Granule loop
         if Bit (Bits, G) then
            Prove_Start ((G - 1) * Granule_Bytes, C);
            pragma Assert (Grain (C, ((G - 1) * Granule_Bytes) / Stride (C) + 1) = G);
         end if;
         pragma Loop_Invariant (for all H in 1 .. G => not Bit (Bits, H));
      end loop;
   end Prove_Clear;

   -- Position, where a block starts, names that block's first granule.
   procedure Prove_Grain (State : Pool; Position : Local) with Ghost,
     Pre => Starts (State, Position),
     Post => Position mod Granule_Bytes = 0 and then Item_Of (State, Position) <= Slots (State.Kind)
       and then Grain (State.Kind, Item_Of (State, Position)) = Position / Granule_Bytes + 1
       and then Position + Stride (State.Kind) <= Slab_Bytes
   is
   begin
      Prove_Start (Position, State.Kind);
   end Prove_Grain;

   procedure Initialize (State : out Pool; Kind : Size_Class) is
   begin
      State := (Occupied => 0, Cached => 0, Handed => 0, Kind => Kind, Clean => 0,
                Free => [others => 0], Bits => [others => [others => False]]);
      Prove_Uniform (State.Bits, Kind, Limit_Of (State), False);
   end Initialize;

   -- Refill only an empty cache, so no cached block can be inserted twice.
   -- Blocks omitted from a full cache remain free in the bitmap.
   procedure Refill (State : in out Pool) with No_Inline,
     Pre => Valid (State) and then State.Cached = 0 and then State.Occupied < Limit_Of (State)
       and then State.Handed = Limit_Of (State),
     Post => Valid (State) and then State.Cached > 0 and then
       State.Bits = State.Bits'Old and then State.Kind = State.Kind'Old
       and then State.Occupied = State.Occupied'Old and then State.Handed = State.Handed'Old
   is
   begin
      for I in 1 .. Limit_Of (State) loop
         if not Bit (State.Bits, Grain (State.Kind, I)) then
            Prove_Scale (I - 1, State.Kind);
            State.Cached := State.Cached + 1;
            State.Free (State.Cached) := Stored_Offset (Scale (I - 1, State.Kind));
            if State.Cached = Cache_Size then return; end if;
         end if;
         pragma Loop_Invariant (Valid (State));
         pragma Loop_Invariant (State.Cached < Cache_Size);
         pragma Loop_Invariant
           (for all J in 1 .. State.Cached => Natural (State.Free (J)) / Stride (State.Kind) + 1 <= I);
         pragma Loop_Invariant
           (if State.Cached = 0 then (for all J in 1 .. I => Bit (State.Bits, Grain (State.Kind, J))));
      end loop;
      if State.Cached = 0 then Prove_Uniform (State.Bits, State.Kind, Limit_Of (State), True); end if;
   end Refill;

   -- Mark the free block at Position live (the cache's top, just popped,
   -- or the first fresh one, just passed).
   procedure Mark (State : in out Pool; Position : Local) with Inline_Always,
     Pre => State.Handed <= Limit_Of (State) and then
       State.Occupied = Population (State.Bits, State.Kind, Limit_Of (State)) and then
       Starts (State, Position) and then Item_Of (State, Position) < Fresh_Of (State) and then
       not Bit (State.Bits, Position / Granule_Bytes + 1) and then Position mod Granule_Bytes = 0 and then
       Grain (State.Kind, Item_Of (State, Position)) = Position / Granule_Bytes + 1 and then
       (for all G in Granule =>
          (if Bit (State.Bits, G) then
             ((G - 1) * Granule_Bytes) mod Stride (State.Kind) = 0 and then
             ((G - 1) * Granule_Bytes) / Stride (State.Kind) + 1 < Fresh_Of (State))) and then
       (for all I in 1 .. State.Cached =>
          Natural (State.Free (I)) mod Stride (State.Kind) = 0 and then
          Natural (State.Free (I)) mod Granule_Bytes = 0 and then
          Natural (State.Free (I)) / Stride (State.Kind) + 1 < Fresh_Of (State) and then
          not Bit (State.Bits, Natural (State.Free (I)) / Granule_Bytes + 1) and then
          Natural (State.Free (I)) /= Position and then
          (for all J in 1 .. I - 1 => State.Free (I) /= State.Free (J))),
     Post => Valid (State) and then State.Kind = State.Kind'Old
       and then State.Occupied = State.Occupied'Old + 1
       and then State.Cached = State.Cached'Old and then State.Handed = State.Handed'Old
       and then Bit (State.Bits, Position / Granule_Bytes + 1)
       and then (for all I in 1 .. Slots (State.Kind) =>
                   (if I /= Item_Of (State, Position) then
                      Bit (State.Bits, Grain (State.Kind, I)) = Bit (State.Bits'Old, Grain (State.Kind, I))))
   is
      G : constant Granule := Granule_Of (Position);
      Before : constant Bitmap := State.Bits with Ghost;
   begin
      Prove_Bit (State.Bits, State.Kind, Item_Of (State, Position), Limit_Of (State));
      State.Bits (Word_Of (G)) (Bit_Of (G)) := True;
      pragma Assert (for all H in Granule => (if H /= G then Bit (State.Bits, H) = Bit (Before, H)));
      Prove_Change (Before, State.Bits, State.Kind, Item_Of (State, Position), Limit_Of (State));
      State.Occupied := State.Occupied + 1;
      Prove_Unique (State.Kind, Item_Of (State, Position));
      pragma Assert (for all I in 1 .. Slots (State.Kind) =>
                       (if I /= Item_Of (State, Position) then
                          Bit (State.Bits, Grain (State.Kind, I)) = Bit (Before, Grain (State.Kind, I))));
   end Mark;

   -- Take, its frame stated on the bits.
   procedure Take_Block (State : in out Pool; Position : out Local; Success, Untouched : out Boolean) with
     Inline_Always,
     Pre => Valid (State),
     Post => Valid (State) and then State.Kind = State.Kind'Old and then Success = Ready (State'Old) and then
       (if Success then State.Occupied'Old < Limit_Of (State'Old) and then
          Starts (State, Position) and then Position mod Granule_Bytes = 0 and then
          Position + Stride (State.Kind) <= Slab_Bytes and then
          State.Occupied = State.Occupied'Old + 1 and then
          not Live (State'Old, Item_Of (State, Position)) and then Live (State, Item_Of (State, Position)) and then
          (for all I in 1 .. Slots (State.Kind) =>
             (if I /= Item_Of (State, Position) then
                Bit (State.Bits, Grain (State.Kind, I)) = Bit (State.Bits'Old, Grain (State.Kind, I))))
        else State = State'Old)
   is
      Before : constant Pool := State with Ghost;
   begin
      Untouched := False;
      if State.Cached > 0 then
         Position := Local (State.Free (State.Cached));
         State.Cached := State.Cached - 1;
      elsif State.Handed < Limit_Of (State) then
         Prove_Scale (State.Handed, State.Kind);
         Position := Scale (State.Handed, State.Kind);
         State.Handed := State.Handed + 1;
         Untouched := Position >= State.Clean;
         State.Clean := Natural'Max (State.Clean, Position + Stride (State.Kind));
      else
         Position := 0;
         Success := False;
         return;
      end if;
      Success := True;
      Prove_Grain (State, Position);
      -- Taking the block from the cache or the fresh ones changed no bit.
      pragma Assert (State.Bits = Before.Bits and then State.Kind = Before.Kind);
      Mark (State, Position);
   end Take_Block;

   procedure Take (State : in out Pool; Position : out Local; Success, Untouched : out Boolean) is
   begin
      Take_Block (State, Position, Success, Untouched);
   end Take;

   procedure Allocate (State : in out Pool; Position : out Local; Success : out Boolean) is
      Untouched : Boolean;
      Before : constant Pool := State with Ghost;
   begin
      Position := 0;
      Success := State.Occupied < Limit_Of (State);
      if Success then
         -- Neither cached nor fresh blocks: some released block was dropped
         -- from a full cache; find it (and up to a cache full more).
         if State.Cached = 0 and then State.Handed = Limit_Of (State) then Refill (State); end if;
         pragma Assert (State.Bits = Before.Bits and then State.Kind = Before.Kind);
         Take_Block (State, Position, Success, Untouched);
         pragma Assert (Success);
      end if;
   end Allocate;

   function Is_Live (State : Pool; Position : Local) return Boolean is
   begin
      if Starts (State, Position) then Prove_Grain (State, Position); end if;
      if Position mod Granule_Bytes = 0 and then Bit (State.Bits, Granule_Of (Position)) then
         Prove_Start (Position, State.Kind);
      end if;
      return Position mod Granule_Bytes = 0 and then Bit (State.Bits, Granule_Of (Position));
   end Is_Live;

   procedure Release (State : in out Pool; Position : Local; Success : out Boolean) is
      G : constant Granule := Granule_Of (Position);
      Before : constant Bitmap := State.Bits with Ghost;
   begin
      if Starts (State, Position) then Prove_Grain (State, Position); end if;
      Success := Position mod Granule_Bytes = 0 and then Bit (State.Bits, G);
      if Success then
         -- The bit is a block's first granule (Valid): Position starts it.
         pragma Assert ((G - 1) * Granule_Bytes = Position);
         Prove_Grain (State, Position);
         State.Bits (Word_Of (G)) (Bit_Of (G)) := False;
         pragma Assert (for all H in Granule => (if H /= G then Bit (State.Bits, H) = Bit (Before, H)));
         Prove_Change (Before, State.Bits, State.Kind, Item_Of (State, Position), Limit_Of (State));
         State.Occupied := State.Occupied - 1;
         Prove_Unique (State.Kind, Item_Of (State, Position));
         pragma Assert (for all I in 1 .. Slots (State.Kind) =>
                          (if I /= Item_Of (State, Position) then
                             Bit (State.Bits, Grain (State.Kind, I)) = Bit (Before, Grain (State.Kind, I))));
         if State.Occupied = 0 then
            -- Empty: every block is free again, so hand them out in order
            -- (no cache, no refill scan).
            Prove_Empty (State.Bits, State.Kind, Limit_Of (State));
            Prove_Clear (State.Bits, State.Kind);
            State.Cached := 0;
            State.Handed := 0;
         else
            -- Prefer the latest return even when full. The displaced cached
            -- block stays free in Bits and will be found by a later refill.
            if State.Cached < Cache_Size then State.Cached := State.Cached + 1; end if;
            State.Free (State.Cached) := Stored_Offset (Position);
         end if;
      end if;
   end Release;

   procedure Reconfigure (State : in out Pool; Kind : Size_Class; Success : out Boolean) is
   begin
      Success := State.Occupied = 0;
      if Success then
         -- No block live: no bit set (Valid), so the bits serve any class
         -- as they are; the clean mark stays (the slab's bytes below it
         -- were written).
         Prove_Empty (State.Bits, State.Kind, Limit_Of (State));
         Prove_Clear (State.Bits, State.Kind);
         State.Kind := Kind;
         State.Cached := 0;
         State.Handed := 0;
         Prove_Uniform (State.Bits, Kind, Limit_Of (State), False);
      end if;
   end Reconfigure;
end Heap_Bitmap;
