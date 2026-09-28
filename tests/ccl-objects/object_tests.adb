with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with Config_Objects;
with Config_Fixture;

procedure Object_Tests is
   Schema : constant Schema_Key := [16#1234#, 16#5678#, 1, 9];
   Wrong_Schema : constant Schema_Key := [16#1234#, 16#5678#, 2, 9];
   R, R2 : Registry;
   Point, Color, Settings, Other : Type_Reference;
   Contract, Moved_Contract, Rejected : Binding;
   Good, Bad, Moved : CCL.Objects.Image;
   Ok : Boolean;
   Built : Build_Result;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean) is
   begin
      pragma Assert (Condition);
      Checks := Checks + 1;
   end Check;
   procedure Define_Types
     (Types : in out Registry; Point, Color, Settings : out Type_Reference)
   is
      D : Description;
      Result : Definition_Result;
   begin
      D := (Identifier => Named ("Point"), Form => Product, Count => 2,
            Parts => [1 => (Named ("x"), Integer_Type),
                      2 => (Named ("y"), Integer_Type), others => <>]);
      Define (Types, D, Point, Result); Check (Result = Defined);
      D := (Identifier => Named ("Color"), Form => Sum, Count => 2,
            Parts => [1 => (Named ("Red"), Unit_Type),
                      2 => (Named ("Blue"), Unit_Type), others => <>]);
      Define (Types, D, Color, Result); Check (Result = Defined);
      D := (Identifier => Named ("Settings"), Form => Product, Count => 3,
            Parts => [1 => (Named ("name"), String_Type),
                      2 => (Named ("position"), Point),
                      3 => (Named ("color"), Color), others => <>]);
      Define (Types, D, Settings, Result); Check (Result = Defined);
   end Define_Types;
   procedure Add (C : Cell) is
   begin
      Append (Good, C, Built); Check (Built = Added);
   end Add;
begin
   Check (CCL.Objects.Image'Size = Native_Image_Bytes * 8 and CCL.Objects.Image'Alignment = 4096);
   Define_Types (R, Point, Color, Settings);
   Bind (R, Settings, Schema, Contract, Ok); Check (Ok);
   Good := Empty (Contract);
   Check (not Validate (Good, Contract));
   Add (Product_Cell (3));
   Append_Text (Good, "Cubie", Built); Check (Built = Added);
   Add (Product_Cell (2));
   Add (Integer_Cell (Integer_64'First));
   Add (Integer_Cell (Integer_64'Last));
   Add (Variant_Cell (2));
   Add (Unit_Cell);
   Check (Validate (Good, Contract));
   Check (Integer_Of (Good.Cells (4)) = Integer_64'First);
   Check (Integer_Of (Good.Cells (5)) = Integer_64'Last);
   for N in Integer_64 range -10 .. 10 loop Check (Integer_Of (Integer_Cell (N)) = N); end loop;
   Check (Good.Used_Cells = 7 and Good.Used_Bytes = 5);
   Moved := Good;
   Check (Validate (Moved, Contract));
   declare
      Result : Definition_Result;
   begin
      Define (R2, (Identifier => Named ("Unrelated"), Form => Product, others => <>), Other, Result);
      Check (Result = Defined);
      Define_Types (R2, Point, Color, Other);
      Check (Other /= Settings);
      Bind (R2, Other, Schema, Moved_Contract, Ok); Check (Ok);
      --  The trusted schema is the same, despite different module-local IDs.
      Check (Validate (Moved, Moved_Contract));
   end;
   for Mutation in 1 .. 19 loop
      Bad := Good;
      case Mutation is
         when 1 => Bad.Version := 2;
         when 2 => Bad.Schema := Wrong_Schema;
         when 3 => Bad.Reserved := 1;
         when 4 => Bad.Used_Cells := 0;
         when 5 => Bad.Used_Cells := Unsigned_32'Last;
         when 6 => Bad.Used_Cells := 6;
         when 7 => Bad.Used_Cells := 8;
         when 8 => Bad.Used_Bytes := Unsigned_32'Last;
         when 9 => Bad.Used_Bytes := 4;
         when 10 => Bad.Used_Bytes := 6;
         when 11 => Bad.Cells (1).First := 2;
         when 12 => Bad.Cells (2).First := Unsigned_64'Last;
         when 13 => Bad.Cells (2).Second := Unsigned_64'Last;
         when 14 => Bad.Cells (3).First := 3;
         when 15 => Bad.Cells (4).Second := 1;
         when 16 => Bad.Cells (6).First := 3;
         when 17 => Bad.Cells (8).First := 1;
         when 18 => Bad.Text (8192) := 'x';
         when others => Bad.Padding (4048) := 1;
      end case;
      Check (not Validate (Bad, Contract));
   end loop;
   declare
      Scalar : Binding;
      B : CCL.Objects.Image;
   begin
      -- Every byte of each unused region matters, not just its endpoints.
      Bind (R, Integer_Type, Schema, Scalar, Ok); Check (Ok);
      B := Empty (Scalar); Append (B, Integer_Cell (42), Built); Check (Built = Added);
      for I in B.Text'Range loop
         B.Text (I) := Character'Val (1 + (I mod 255));
         Check (not Validate (B, Scalar));
         B.Text (I) := Character'Val (0);
      end loop;
      for I in B.Padding'Range loop
         B.Padding (I) := Unsigned_8 (1 + (I mod 255));
         Check (not Validate (B, Scalar));
         B.Padding (I) := 0;
      end loop;
      for I in 2 .. Maximum_Cells loop
         B.Cells (I).First := 1; Check (not Validate (B, Scalar));
         B.Cells (I).First := 0;
         B.Cells (I).Second := Unsigned_64'Last; Check (not Validate (B, Scalar));
         B.Cells (I).Second := 0;
      end loop;
      Check (Validate (B, Scalar));
      Bind (R, String_Type, Schema, Scalar, Ok); Check (Ok);
      -- Every tail offset, including full capacity's empty slice. Nonzero
      -- bytes inside the string are data; the first byte after it is not.
      B := Empty (Scalar); Append_Text (B, "", Built); Check (Built = Added);
      for Length in 0 .. Maximum_Text_Bytes loop
         B.Cells (1).Second := Unsigned_64 (Length);
         B.Used_Bytes := Unsigned_32 (Length);
         Check (Validate (B, Scalar));
         if Length < Maximum_Text_Bytes then
            B.Text (Length + 1) := Character'Val (255);
            Check (not Validate (B, Scalar));
            -- Becomes legitimate payload on the following iteration.
         end if;
      end loop;
   end;
   declare
      D : Description;
      Ref : Type_Reference;
      Result : Definition_Result;
   begin
      D := (Identifier => Named ("HiddenHandler"), Form => Sum, Count => 2,
            Parts => [1 => (Named ("Nothing"), Unit_Type),
                      2 => (Named ("Callback"), Handler_Type), others => <>]);
      Define (R, D, Ref, Result); Check (Result = Defined);
      Bind (R, Ref, Schema, Rejected, Ok); Check (not Ok);
      D := (Identifier => Named ("NestedHandler"), Form => Product, Count => 1,
            Parts => [1 => (Named ("action"), Ref), others => <>]);
      Define (R, D, Ref, Result); Check (Result = Defined);
      Bind (R, Ref, Schema, Rejected, Ok); Check (not Ok);
      Bind (R, Handler_Type, Schema, Rejected, Ok); Check (not Ok);
      Bind (R, Invalid_Type, Schema, Rejected, Ok); Check (not Ok);
      Bind (R, Integer_Type, No_Schema, Rejected, Ok); Check (not Ok);
   end;
   declare
      Scalar : Binding;
      B : CCL.Objects.Image;
      Huge : constant String (1 .. Maximum_Text_Bytes) := [others => Character'Val (255)];
      High : constant String (Integer'Last .. Integer'Last) := "x";
   begin
      Bind (R, String_Type, Schema, Scalar, Ok); Check (Ok);
      B := Empty (Scalar);
      Append_Text (B, Huge, Built); Check (Built = Added and Validate (B, Scalar));
      declare Before : constant CCL.Objects.Image := B; begin
         Append_Text (B, "x", Built); Check (Built = Full and B = Before);
      end;
      B := Empty (Scalar);
      Append_Text (B, High, Built); Check (Built = Added and Validate (B, Scalar));
      B := Empty (Scalar);
      Append_Text (B, "", Built); Check (Built = Added and Validate (B, Scalar));
      Bind (R, Boolean_Type, Schema, Scalar, Ok); Check (Ok);
      B := Empty (Scalar); Append (B, Boolean_Cell (True), Built);
      Check (Built = Added and Validate (B, Scalar));
      B.Cells (1).First := 2; Check (not Validate (B, Scalar));
      Bind (R, Character_Type, Schema, Scalar, Ok); Check (Ok);
      B := Empty (Scalar); Append (B, Character_Cell (Character'Last), Built);
      Check (Built = Added and Validate (B, Scalar));
      B.Cells (1).First := 256; Check (not Validate (B, Scalar));
      B.Used_Cells := Unsigned_32'Last;
      declare Before : constant CCL.Objects.Image := B; begin
         Append (B, Unit_Cell, Built); Check (Built = Invalid_Image and B = Before);
      end;
   end;
   declare
      Object : Config_Objects.State;
      Loaded : CCL.Objects.Image;
      Revision : Unsigned_64;
      Stored : Config_Objects.Outcome;
      Read : Config_Objects.Read_Result;
      use type Config_Objects.Outcome;
      use type Config_Objects.Read_Result;
   begin
      Config_Fixture.Commit (Object, Good, 0, Stored); Check (Stored = Config_Objects.Not_Bound);
      Config_Objects.Initialize (Object, Contract, Ok); Check (Ok);
      Config_Objects.Initialize (Object, Moved_Contract, Ok); Check (not Ok);
      Config_Fixture.Load_Empty (Object);
      Config_Objects.Read (Object, Schema, Loaded, Revision, Read); Check (Read = Config_Objects.Missing);
      Config_Fixture.Commit (Object, Good, 0, Stored); Check (Stored = Config_Objects.Published);
      Config_Objects.Read (Object, Schema, Loaded, Revision, Read);
      Check (Read = Config_Objects.Found and Revision = 1 and Loaded = Good);
      Config_Fixture.Commit (Object, Good, 0, Stored); Check (Stored = Config_Objects.Revision_Conflict);
      Bad := Good; Bad.Cells (6).First := 3;
      Config_Fixture.Commit (Object, Bad, 1, Stored); Check (Stored = Config_Objects.Invalid_Value);
      Config_Objects.Read (Object, Schema, Loaded, Revision, Read);
      Check (Read = Config_Objects.Found and Revision = 1 and Loaded = Good);
      Config_Objects.Read (Object, Wrong_Schema, Loaded, Revision, Read);
      Check (Read = Config_Objects.Schema_Mismatch and Revision = 0 and Loaded.Schema = No_Schema);
      Bad := Good; Bad.Text (1 .. 5) := "Alloy";
      Config_Fixture.Commit (Object, Bad, 1, Stored); Check (Stored = Config_Objects.Published);
      Bad.Text (1 .. 5) := "CHANG";
      Config_Objects.Read (Object, Schema, Loaded, Revision, Read);
      Check (Read = Config_Objects.Found and Revision = 2 and Loaded.Text (1 .. 5) = "Alloy");
      Check (Validate (Loaded, Moved_Contract));
   end;
   declare
      D : Description := (Identifier => Named ("Wide"), Form => Product, Count => 16, others => <>);
      Wide, Big : Type_Reference;
      Result : Definition_Result;
      Large : CCL.Objects.Image;
      Large_Binding : Binding;
      procedure Put (Value : Cell) is
      begin
         Append (Large, Value, Built); Check (Built = Added);
      end Put;
   begin
      for I in 1 .. D.Count loop
         D.Parts (I) := (Named ("f" & Character'Val (Character'Pos ('A') + I)), Find (R, Named ("Point")));
      end loop;
      Define (R, D, Wide, Result); Check (Result = Defined);
      D.Identifier := Named ("Big"); D.Count := 5;
      for I in 1 .. D.Count loop D.Parts (I).Payload := Wide; end loop;
      Define (R, D, Big, Result); Check (Result = Defined and Cells (R, Big) = 246);
      Bind (R, Big, Schema, Large_Binding, Ok); Check (Ok);
      Large := Empty (Large_Binding);
      Put (Product_Cell (5));
      for W in 1 .. 5 loop
         Put (Product_Cell (16));
         for P in 1 .. 16 loop
            Put (Product_Cell (2)); Put (Integer_Cell (Integer_64 (W))); Put (Integer_Cell (Integer_64 (P)));
         end loop;
      end loop;
      Check (Validate (Large, Large_Binding) and Large.Used_Cells = 246);
      --  Bounded builders remain safe even before a schema-valid object exists.
      while Large.Used_Cells < Maximum_Cells loop Put (Unit_Cell); end loop;
      declare Before : constant CCL.Objects.Image := Large; begin
         Append (Large, Unit_Cell, Built); Check (Built = Full and Large = Before);
      end;
      Check (not Validate (Large, Large_Binding));
   end;
   Ada.Text_IO.Put_Line ("CCL native typed objects + Config: PASS" & Checks'Image & " checks");
end Object_Tests;
