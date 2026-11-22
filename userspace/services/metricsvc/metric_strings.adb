pragma Ada_2022;
with Heap_Growth;
package body Metric_Strings with SPARK_Mode => Off is
   Initial_Bytes : constant := 4_096;
   Initial_Strings : constant := 256;
   --  The index holds at most half its slots.
   Load_Divisor : constant := 2;

   function Add (A, B : Unsigned_64) return Unsigned_64 is
     (if B > Unsigned_64'Last - A then Unsigned_64'Last else A + B);

   function Hash (Text : String; Seed : Unsigned_64 := Offset_Basis) return Unsigned_64 is
      H : Unsigned_64 := Seed;
   begin
      for C of Text loop
         H := (H xor Unsigned_64 (Character'Pos (C))) * Prime;
      end loop;
      return H;
   end Hash;

   function Matches (T : Table; Id : String_Id; Text : String; H : Unsigned_64) return Boolean;
   function Matches (T : Table; Id : String_Id; Text : String; H : Unsigned_64) return Boolean is
      E : constant Entry_Record := Entry_Blocks.Get (T.Entries, Id - 1);
   begin
      if E.Hash /= H or else E.Length /= Text'Length then
         return False;
      end if;
      for I in 0 .. E.Length - 1 loop
         if Text_Blocks.Get (T.Text, E.Offset + I) /= Text (Text'First + I) then
            return False;
         end if;
      end loop;
      return True;
   end Matches;

   --  The slot holding Text, or the empty slot where it would go.
   function Slot_For (T : Table; Text : String; H : Unsigned_64) return Natural;
   function Slot_For (T : Table; Text : String; H : Unsigned_64) return Natural is
      Size : constant Natural := Slot_Blocks.Capacity (T.Slots);
      Mask : constant Unsigned_64 := Unsigned_64 (Size) - 1;
      Slot : Natural := Natural (H and Mask);
      Held : String_Id;
   begin
      loop
         Held := Slot_Blocks.Get (T.Slots, Slot);
         exit when Held = No_String or else Matches (T, Held, Text, H);
         Slot := Natural ((Unsigned_64 (Slot) + 1) and Mask);
      end loop;
      return Slot;
   end Slot_For;

   function Find (T : Table; Text : String) return String_Id is
      H : constant Unsigned_64 := Hash (Text);
   begin
      if Slot_Blocks.Capacity (T.Slots) = 0 or else Text'Length = 0 then
         return No_String;
      end if;
      return Slot_Blocks.Get (T.Slots, Slot_For (T, Text, H));
   end Find;

   --  A new, larger index holding every string.
   procedure Grow_Index (T : in out Table; Grown : out Boolean);
   procedure Grow_Index (T : in out Table; Grown : out Boolean) is
      Size : constant Natural :=
        (if Slot_Blocks.Capacity (T.Slots) = 0 then Initial_Strings * Load_Divisor
         else 2 * Slot_Blocks.Capacity (T.Slots));
      Fresh : Slot_Blocks.Block;
      Old : Slot_Blocks.Block := T.Slots;
   begin
      Grown := False;
      Slot_Blocks.Allocate (Fresh, Size);
      if Slot_Blocks.Capacity (Fresh) = 0 then
         return;
      end if;
      for I in 0 .. Size - 1 loop
         Slot_Blocks.Set (Fresh, I, No_String);
      end loop;
      T.Slots := Fresh;
      for Id in 1 .. T.Used loop
         declare
            E : constant Entry_Record := Entry_Blocks.Get (T.Entries, Id - 1);
            Mask : constant Unsigned_64 := Unsigned_64 (Size) - 1;
            Slot : Natural := Natural (E.Hash and Mask);
         begin
            while Slot_Blocks.Get (T.Slots, Slot) /= No_String loop
               Slot := Natural ((Unsigned_64 (Slot) + 1) and Mask);
            end loop;
            Slot_Blocks.Set (T.Slots, Slot, Id);
         end;
      end loop;
      Slot_Blocks.Release (Old);
      Grown := True;
   end Grow_Index;

   procedure Grow_Text is new Heap_Growth (Text_Blocks);
   procedure Grow_Entries is new Heap_Growth (Entry_Blocks);

   procedure Intern (T : in out Table; Text : String; Id : out String_Id) is
      H : constant Unsigned_64 := Hash (Text);
      Grown : Boolean;
      Slot : Natural;
   begin
      Id := No_String;
      if Text'Length = 0 or else Text'Length > Maximum_Length then
         T.Refusals := Add (T.Refusals, 1);
         return;
      end if;
      Id := Find (T, Text);
      if Id /= No_String then
         return;
      end if;
      if T.Used = Maximum_Strings or else T.Used_Bytes > Maximum_Bytes - Text'Length then
         T.Refusals := Add (T.Refusals, 1);
         return;
      end if;
      if (T.Used + 1) * Load_Divisor > Slot_Blocks.Capacity (T.Slots) then
         Grow_Index (T, Grown);
         if not Grown then
            T.Refusals := Add (T.Refusals, 1);
            return;
         end if;
      end if;
      Grow_Text (T.Text, T.Used_Bytes, Initial_Bytes, Maximum_Bytes, T.Used_Bytes + Text'Length, Grown);
      if Grown then
         Grow_Entries (T.Entries, T.Used, Initial_Strings, Maximum_Strings, T.Used + 1, Grown);
      end if;
      if not Grown then
         T.Refusals := Add (T.Refusals, 1);
         return;
      end if;
      for I in 0 .. Text'Length - 1 loop
         Text_Blocks.Set (T.Text, T.Used_Bytes + I, Text (Text'First + I));
      end loop;
      Entry_Blocks.Set (T.Entries, T.Used, (Offset => T.Used_Bytes, Length => Text'Length, Hash => H));
      T.Used_Bytes := T.Used_Bytes + Text'Length;
      T.Used := T.Used + 1;
      Id := T.Used;
      Slot := Slot_For (T, Text, H);
      Slot_Blocks.Set (T.Slots, Slot, Id);
   end Intern;

   function Text_Of (T : Table; Id : String_Id) return String is
   begin
      if Id = No_String or else Id > T.Used then
         return "";
      end if;
      declare
         E : constant Entry_Record := Entry_Blocks.Get (T.Entries, Id - 1);
         Result : String (1 .. E.Length);
      begin
         for I in Result'Range loop
            Result (I) := Text_Blocks.Get (T.Text, E.Offset + I - 1);
         end loop;
         return Result;
      end;
   end Text_Of;

   function Count (T : Table) return Natural is (T.Used);
   function Bytes_Held (T : Table) return Unsigned_64 is
     (Unsigned_64 (Text_Blocks.Capacity (T.Text)) +
      Unsigned_64 (Entry_Blocks.Capacity (T.Entries)) * (Entry_Record'Size / 8) +
      Unsigned_64 (Slot_Blocks.Capacity (T.Slots)) * (String_Id'Size / 8 + 1));
   function Refused (T : Table) return Unsigned_64 is (T.Refusals);
end Metric_Strings;
