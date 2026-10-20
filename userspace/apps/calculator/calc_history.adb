with Ada.Unchecked_Deallocation;

package body Calc_History is

   --  The ring starts small and doubles as entries arrive, up to Limit.
   FIRST_SLOTS : constant := 16;

   procedure Release is new Ada.Unchecked_Deallocation (String, Text_Access);
   procedure Release is new Ada.Unchecked_Deallocation (Calc_Syntax.Term_Array, Terms_Access);
   procedure Release is new Ada.Unchecked_Deallocation (Item, Item_Access);
   procedure Release is new Ada.Unchecked_Deallocation (Ring, Ring_Access);

   procedure Free (X : in out Item_Access) is
   begin
      if X /= null then
         Release (X.Source);
         Release (X.Fraction);
         Release (X.Decimal);
         Release (X.Terms);
         Calc_Numbers.Free (X.Value);
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
   function Fraction (H : History; Index : Entry_Index) return Text_View is
     (Text_View (At_Index (H, Index).Fraction));
   function Decimal (H : History; Index : Entry_Index) return Text_View is
     (Text_View (At_Index (H, Index).Decimal));
   function Form (H : History; Index : Entry_Index) return Calc_Format.Decimal_Form is (At_Index (H, Index).Form);
   function Kind (H : History; Index : Entry_Index) return Calc_Numbers.Exactness is
     (Calc_Numbers.Kind (At_Index (H, Index).Value));
   function Is_Integer (H : History; Index : Entry_Index) return Boolean is
     (Calc_Numbers.Is_Integer (At_Index (H, Index).Value));
   function Term_Count (H : History; Index : Entry_Index) return Natural is (At_Index (H, Index).Terms'Length);

   procedure Format (X : in out Item; Places : Calc_Format.Places_Count) is
      Fraction_Text : String (1 .. Calc_Format.Fraction_Bound (X.Value));
      Decimal_Text : String (1 .. Calc_Format.Decimal_Bound (X.Value, Places));
      Fraction_Last, Decimal_Last : Natural;
   begin
      Calc_Format.Fraction_Text (X.Value, Fraction_Text, Fraction_Last);
      Calc_Format.Decimal_Text (X.Value, Places, Decimal_Text, Decimal_Last, X.Form);
      Release (X.Fraction);
      Release (X.Decimal);
      X.Fraction := new String'(Fraction_Text (1 .. Fraction_Last));
      X.Decimal := new String'(Decimal_Text (1 .. Decimal_Last));
   end Format;

   procedure Add
     (H : in out History; Text : String; Terms : Calc_Syntax.Term_Array;
      Result : in out Calc_Numbers.Number; Places : Calc_Format.Places_Count)
   is
      X : constant Item_Access := new Item;
   begin
      X.Source := new String'(Text);
      X.Terms := new Calc_Syntax.Term_Array'(Terms);
      Calc_Numbers.Move (Result, X.Value);
      Format (X.all, Places);
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

   procedure Copy_Latest (H : History; Into : in out Calc_Numbers.Number) is
   begin
      Calc_Numbers.Copy (At_Index (H, 1).Value, Into);
   end Copy_Latest;

   procedure Reformat (H : in out History; Places : Calc_Format.Places_Count) is
   begin
      for Index in 1 .. H.Used loop
         Format (At_Index (H, Index).all, Places);
      end loop;
   end Reformat;

   procedure Evaluate
     (H : History; Text : String; Bounds : Calc_Engine.Limits;
      Result : in out Calc_Numbers.Number; Failure : out Calc_Engine.Failure_Kind;
      Position : out Calc_Syntax.Column)
   is
      Based : constant String (1 .. Text'Length) := Text;
      Nothing : Calc_Numbers.Number;
   begin
      if H.Used > 0 then
         Calc_Engine.Evaluate (Based, At_Index (H, 1).Value, True, Bounds, Result, Failure, Position);
      else
         Calc_Engine.Evaluate (Based, Nothing, False, Bounds, Result, Failure, Position);
      end if;
   end Evaluate;
end Calc_History;
