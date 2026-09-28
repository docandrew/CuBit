with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
package body Nested_Fixture is
   use CCL.Objects;
   procedure Define (Shifted : Boolean; Contract : out Binding; Success : out Boolean) is
      Types : Registry;
      Payload, Mode, Root, Unrelated : Type_Reference;
      Defined_As : Definition_Result;
      Unbound : Binding;
   begin
      Contract := Unbound;
      Success := False;
      if Shifted then
         CCL.Types.Define (Types, (Identifier => Named ("Unrelated"), Form => Product,
           others => <>), Unrelated, Defined_As);
         if Defined_As /= Defined then return; end if;
      end if;
      CCL.Types.Define (Types,
        (Identifier => Named ("ActiveData"), Form => Product, Count => 2,
         Parts => [1 => (Named ("enabled"), Boolean_Type),
                   2 => (Named ("note"), String_Type), others => <>]), Payload, Defined_As);
      if Defined_As /= Defined then return; end if;
      CCL.Types.Define (Types,
        (Identifier => Named ("Mode"), Form => Sum, Count => 2,
         Parts => [1 => (Named ("Inactive"), Unit_Type),
                   2 => (Named ("Active"), Payload), others => <>]), Mode, Defined_As);
      if Defined_As /= Defined then return; end if;
      CCL.Types.Define (Types,
        (Identifier => Named ("Preferences"), Form => Product, Count => 3,
         Parts => [1 => (Named ("name"), String_Type),
                   2 => (Named ("mode"), Mode),
                   3 => (Named ("score"), Integer_Type), others => <>]), Root, Defined_As);
      if Defined_As /= Defined then return; end if;
      Bind (Types, Root, [5, 6, 7, 8], Contract, Success);
   end Define;

   procedure Values
     (Contract : Binding; First, Second : out CCL.Objects.Image; Success : out Boolean)
   is
      Text : String (1 .. Maximum_Text_Bytes);
      Good : Boolean := True;
      procedure Add (Object : in out CCL.Objects.Image; Item : Cell) is
         Result : Build_Result;
      begin
         Append (Object, Item, Result);
         Good := Good and Result = Added;
      end Add;
      procedure Add_Text (Object : in out CCL.Objects.Image; Item : String) is
         Result : Build_Result;
      begin
         Append_Text (Object, Item, Result);
         Good := Good and Result = Added;
      end Add_Text;
   begin
      for I in Text'Range loop Text (I) := Character'Val (32 + (I - 1) mod 95); end loop;
      First := Empty (Contract);
      Add (First, Product_Cell (3));
      Add_Text (First, "Cubie");
      Add (First, Variant_Cell (2));
      Add (First, Product_Cell (2));
      Add (First, Boolean_Cell (True));
      Add_Text (First, Text (1 .. 5000));
      Add (First, Integer_Cell (Integer_64'First));
      Second := Empty (Contract);
      Add (Second, Product_Cell (3));
      Add_Text (Second, Text);
      Add (Second, Variant_Cell (1));
      Add (Second, Unit_Cell);
      Add (Second, Integer_Cell (Integer_64'Last));
      Success := Good and Validate (First, Contract) and Validate (Second, Contract);
   end Values;
end Nested_Fixture;
