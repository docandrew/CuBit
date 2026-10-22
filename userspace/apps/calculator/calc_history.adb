with Ada.Unchecked_Deallocation;
with Calc_Numbers;

package body Calc_History is
   use type Calc_Engine.Number_Domain;
   use type Calc_Words.Word_Size;

   --  The ring starts small and doubles as entries arrive, up to Limit.
   FIRST_SLOTS : constant := 16;
   HEXADECIMAL_BASE : constant Calc_Format.Display_Base := 16;
   OCTAL_BASE : constant Calc_Format.Display_Base := 8;
   BINARY_BASE : constant Calc_Format.Display_Base := 2;
   --  An unbounded value's bit field: its low 64 bits, a pattern of at
   --  most three limbs (with its denominator).
   BIT_FIELD_WORD : constant Calc_Words.Word_Size := Calc_Words.Bits_64;
   BIT_FIELD_LIMBS : constant := 4;

   procedure Release_Text is new Ada.Unchecked_Deallocation (String, Text_Access);
   procedure Release is new Ada.Unchecked_Deallocation (Calc_Syntax.Term_Array, Terms_Access);
   procedure Release is new Ada.Unchecked_Deallocation (Item, Item_Access);
   procedure Release is new Ada.Unchecked_Deallocation (Ring, Ring_Access);

   procedure Release (Item : in out Texts) is
   begin
      Release_Text (Item.Fraction);
      Release_Text (Item.Decimal);
      Release_Text (Item.Bound);
      Release_Text (Item.Hexadecimal);
      Release_Text (Item.Octal);
      Release_Text (Item.Binary);
      Release_Text (Item.Bits);
   end Release;

   procedure Describe
     (X : Calc_Balls.Ball; Mode : Calc_Engine.Options; Wrapped : Boolean; Places : Calc_Format.Places_Count;
      Into : in out Texts)
   is
      Mid : Calc_Numbers.Number;
      Programmer : constant Boolean := Mode.Domain = Calc_Engine.Integers;
   begin
      Release (Into);
      Calc_Balls.Midpoint (X, Mid);
      declare
         Exact : constant Boolean := Calc_Balls.Is_Exact (X);
         Fraction : String (1 .. (if Exact then Calc_Format.Fraction_Bound (Mid) else 0));
         Decimal : String (1 .. Calc_Format.Ball_Bound (X, Places));
         Bound : String (1 .. Calc_Format.BOUND_TEXT);
         Based : String (1 .. (if Programmer then Calc_Format.Base_Bound (Mid) else 0));
         Fraction_Last, Decimal_Last, Bound_Last, Based_Last : Natural := 0;
      begin
         if Exact then
            Calc_Format.Fraction_Text (Mid, Fraction, Fraction_Last);
         end if;
         Calc_Format.Ball_Text (X, Places, Decimal, Decimal_Last, Bound, Bound_Last, Into.Form);
         Into.Fraction := new String'(Fraction (1 .. Fraction_Last));
         Into.Decimal := new String'(Decimal (1 .. Decimal_Last));
         Into.Bound := new String'(Bound (1 .. Bound_Last));
         Into.Exact := Exact;
         Into.Whole := Calc_Balls.Is_Integer (X);
         Into.Programmer := Programmer and then Into.Whole;
         Into.Word := Mode.Word;
         Into.Wrapped := Wrapped;
         if Into.Programmer then
            Calc_Format.Base_Text (Mid, HEXADECIMAL_BASE, Mode.Word, Based, Based_Last);
            Into.Hexadecimal := new String'(Based (1 .. Based_Last));
            Calc_Format.Base_Text (Mid, OCTAL_BASE, Mode.Word, Based, Based_Last);
            Into.Octal := new String'(Based (1 .. Based_Last));
            Calc_Format.Base_Text (Mid, BINARY_BASE, Mode.Word, Based, Based_Last);
            Into.Binary := new String'(Based (1 .. Based_Last));
            if Mode.Word /= Calc_Words.Unbounded then
               Into.Bits := new String'(Based (1 .. Based_Last));
            else
               declare
                  Low : Calc_Numbers.Number;
                  State : Calc_Numbers.Status;
                  Budget : constant Calc_Numbers.Size_Budget :=
                    Calc_Numbers.Size_Budget'Max (Calc_Numbers.Size (Mid), BIT_FIELD_LIMBS);
               begin
                  Calc_Words.Pattern (Mid, BIT_FIELD_WORD, Budget, Low, State);
                  if Calc_Numbers."=" (State, Calc_Numbers.Done) then
                     declare
                        Bits : String (1 .. Calc_Format.Base_Bound (Low));
                        Bits_Last : Natural;
                     begin
                        Calc_Format.Base_Text (Low, BINARY_BASE, BIT_FIELD_WORD, Bits, Bits_Last);
                        Into.Bits := new String'(Bits (1 .. Bits_Last));
                     end;
                  end if;
                  Calc_Numbers.Free (Low);
               end;
            end if;
         end if;
      end;
      Calc_Numbers.Free (Mid);
   end Describe;

   procedure Free (X : in out Item_Access) is
   begin
      if X /= null then
         Release_Text (X.Source);
         Release (X.Terms);
         Release (X.Shown);
         Calc_Balls.Free (X.Value);
         Release (X);
      end if;
   end Free;

   procedure Create (H : out History; Capacity : Positive) is
   begin
      H.Items := new Ring'(0 .. Natural'Min (FIRST_SLOTS, Capacity) - 1 => null);
      H.First := 0;
      H.Used := 0;
      H.Limit := Capacity;
   end Create;

   procedure Clear (H : in out History) is
   begin
      if H.Items /= null then
         for X of H.Items.all loop
            Free (X);
         end loop;
      end if;
      H.First := 0;
      H.Used := 0;
   end Clear;

   procedure Destroy (H : in out History) is
   begin
      Clear (H);
      Release (H.Items);
   end Destroy;

   function Count (H : History) return Entry_Count is (H.Used);
   function Capacity (H : History) return Positive is (H.Limit);

   --  The slot of entry Index (1 = newest).
   function Slot (H : History; Index : Entry_Index) return Natural is
     ((H.First + H.Used - Index) mod H.Items'Length);

   function At_Index (H : History; Index : Entry_Index) return Item_Access is (H.Items (Slot (H, Index)));

   function Source (H : History; Index : Entry_Index) return Text_View is
     (Text_View (At_Index (H, Index).Source));
   function Shown (H : History; Index : Entry_Index) return Texts is (At_Index (H, Index).Shown);
   function Term_Count (H : History; Index : Entry_Index) return Natural is (At_Index (H, Index).Terms'Length);

   procedure Add
     (H : in out History; Text : String; Terms : Calc_Syntax.Term_Array; Result : in out Calc_Balls.Ball;
      Mode : Calc_Engine.Options; Wrapped : Boolean; Places : Calc_Format.Places_Count)
   is
      X : constant Item_Access := new Item;
   begin
      X.Source := new String'(Text);
      X.Terms := new Calc_Syntax.Term_Array'(Terms);
      X.Mode := Mode;
      X.Wrapped := Wrapped;
      Calc_Balls.Move (Result, X.Value);
      Describe (X.Value, Mode, Wrapped, Places, X.Shown);
      if H.Used = H.Limit then
         --  Full: the oldest entry goes.
         Free (H.Items (H.First));
         H.Items (H.First) := X;
         H.First := (H.First + 1) mod H.Items'Length;
         return;
      end if;
      if H.Used = H.Items'Length then
         declare
            Grown : constant Ring_Access :=
              new Ring'(0 .. Natural'Min (H.Limit, 2 * H.Items'Length) - 1 => null);
         begin
            for K in 0 .. H.Used - 1 loop
               Grown (K) := H.Items ((H.First + K) mod H.Items'Length);
            end loop;
            Release (H.Items);
            H.Items := Grown;
            H.First := 0;
         end;
      end if;
      H.Items ((H.First + H.Used) mod H.Items'Length) := X;
      H.Used := H.Used + 1;
   end Add;

   procedure Copy_Latest (H : History; Into : in out Calc_Balls.Ball) is
   begin
      Calc_Balls.Copy (At_Index (H, 1).Value, Into);
   end Copy_Latest;

   procedure Reformat (H : in out History; Places : Calc_Format.Places_Count) is
   begin
      for Index in 1 .. H.Used loop
         declare
            X : Item renames At_Index (H, Index).all;
         begin
            Describe (X.Value, X.Mode, X.Wrapped, Places, X.Shown);
         end;
      end loop;
   end Reformat;

   procedure Evaluate
     (H : History; Text : String; Bounds : Calc_Engine.Limits; Mode : Calc_Engine.Options;
      Result : in out Calc_Balls.Ball; Wrapped : out Boolean; Failure : out Calc_Engine.Failure_Kind;
      Position : out Calc_Syntax.Column)
   is
      Based : constant String (1 .. Text'Length) := Text;
      Nothing : Calc_Balls.Ball;
   begin
      if H.Used > 0 then
         Calc_Engine.Evaluate (Based, At_Index (H, 1).Value, True, Bounds, Mode, Result, Wrapped, Failure, Position);
      else
         Calc_Engine.Evaluate (Based, Nothing, False, Bounds, Mode, Result, Wrapped, Failure, Position);
      end if;
   end Evaluate;
end Calc_History;
