with Ada.Text_IO; use Ada.Text_IO;
with CCL.Types; use CCL.Types;

procedure Registry_Tests is
   R : Registry;
   D : Description;
   Ref, Point, Outcome : Type_Reference;
   Status : Definition_Result;
   procedure Reject (Expected : Definition_Result) is
      Before : constant Registry := R;
   begin
      Define (R, D, Ref, Status);
      pragma Assert (Status = Expected and Ref = Invalid_Type and R = Before);
   end Reject;
begin
   pragma Assert (Known (R, Integer_Type) and not Known (R, Invalid_Type));
   pragma Assert (not Known (R, Declared_Type'First));
   pragma Assert (Find (R, Named ("String")) = String_Type);
   pragma Assert (Cells (R, Invalid_Type) = 0);
   pragma Assert (not Valid_Name (Named ("")) and not Valid_Name (Named ("1a")));
   pragma Assert (not Valid_Name (Named ("a.b")));
   pragma Assert (not Valid_Name (Named (String'(1 .. 33 => 'a'))));
   D := (Identifier => Named ("Point"), Form => Product, Count => 2,
         Parts => [1 => (Named ("x"), Integer_Type),
                   2 => (Named ("y"), Integer_Type), others => <>]);
   Define (R, D, Point, Status);
   pragma Assert (Status = Defined and Cells (R, Point) = 3);
   pragma Assert (Describe (R, Point) = D);
   Reject (Duplicate_Name);
   D := (Identifier => Named ("Outcome"), Form => Sum, Count => 2,
         Parts => [1 => (Named ("Some"), Point),
                   2 => (Named ("None"), Unit_Type), others => <>]);
   Define (R, D, Outcome, Status);
   pragma Assert (Status = Defined and Cells (R, Outcome) = 4);
   pragma Assert (not Is_Enumeration (R, Outcome));
   D := (Identifier => Named ("Color"), Form => Sum, Count => 2,
         Parts => [1 => (Named ("Red"), Unit_Type),
                   2 => (Named ("Blue"), Unit_Type), others => <>]);
   Define (R, D, Ref, Status);
   pragma Assert (Status = Defined and Is_Enumeration (R, Ref));
   D.Identifier := Named ("Bad");
   D.Parts (2).Identifier := Named ("Red");
   Reject (Duplicate_Name);
   D.Parts (2).Identifier := Named ("Blue");
   D.Parts (2).Payload := Last (R) + 1;
   Reject (Invalid_Reference);
   D.Parts (2).Payload := Invalid_Type;
   Reject (Invalid_Reference);
   D.Count := 0;
   Reject (Invalid_Shape);
   D.Form := Primitive;
   Reject (Invalid_Shape);
   D.Form := Product;
   D.Identifier := Named ("Integer");
   Reject (Duplicate_Name);
   D.Identifier := Named ("No.Dot");
   Reject (Invalid_Name);
   --  1 + 16 * 3 = 49 cells, then 1 + 5 * 49 = 246; reject overflow.
   D := (Identifier => Named ("Wide"), Form => Product, Count => 16, others => <>);
   for I in 1 .. D.Count loop
      D.Parts (I) := (Named ("field" & Character'Val (Character'Pos ('A') + I - 1)), Point);
   end loop;
   Define (R, D, Ref, Status);
   pragma Assert (Status = Defined and Cells (R, Ref) = 49);
   D.Identifier := Named ("TooWide");
   for I in 1 .. D.Count loop D.Parts (I).Payload := Ref; end loop;
   Reject (Layout_Too_Large);
   D.Count := 5;
   Define (R, D, Ref, Status);
   pragma Assert (Status = Defined and Cells (R, Ref) = 246);
   --  Exhaust registry without reusing identities or changing earlier types.
   while Last (R) < Type_Reference'Last loop
      D := (Form => Product, others => <>);
      D.Identifier := Named ("Empty" & Character'Val
        (Character'Pos ('A') + Integer (Last (R) - Unit_Type) mod 26) &
        Character'Val (Character'Pos ('A') + Integer (Last (R) - Unit_Type) / 26));
      Define (R, D, Ref, Status);
      pragma Assert (Status = Defined);
   end loop;
   D.Identifier := Named ("Full"); Reject (Registry_Full);
   pragma Assert (Cells (R, Point) = 3 and Cells (R, Outcome) = 4);
   Put_Line ("CCL type registry: products, sums, bounds, atomic rejection PASS");
end Registry_Tests;
