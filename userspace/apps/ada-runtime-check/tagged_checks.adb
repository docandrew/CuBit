pragma Ada_2022;
with Ada.Tags; use Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Unchecked_Deallocation;
with Check_Results; use Check_Results;
with Shapes; use Shapes;

package body Tagged_Checks is

   type Shape_Access is access Shape'Class;
   type Named_Access is access Named'Class;

   procedure Free is new Ada.Unchecked_Deallocation (Named'Class, Named_Access);

   --  A class-wide result: built on the secondary stack.
   function Make (Which : Character) return Shape'Class is
     (case Which is
        when 'S' => Square'(Id => 1, Side => 3),
        when 'C' => Cube'(Id => 2, Side => 2, Depth => 5),
        when others => Polygon'(Points => 3, Id => 3, Lengths => [1, 2, 3], Scale => 2));

   --  Ada.Tags.Generic_Dispatching_Constructor: build an object from a tag.
   type Seed is record
      Size : Natural;
   end record;

   package Builders is
      type Built is abstract tagged record
         Size : Natural := 0;
      end record;
      function Create (From : not null access Seed) return Built is abstract;
      type Small is new Built with null record;
      overriding function Create (From : not null access Seed) return Small;
      type Large is new Built with null record;
      overriding function Create (From : not null access Seed) return Large;
   end Builders;

   package body Builders is
      overriding function Create (From : not null access Seed) return Small is
        (Size => From.Size);
      overriding function Create (From : not null access Seed) return Large is
        (Size => 1_000 * From.Size);
   end Builders;

   function Construct is new Ada.Tags.Generic_Dispatching_Constructor
     (T => Builders.Built, Parameters => Seed, Constructor => Builders.Create);

   procedure Library_Level is
      Sq : constant Square := (Id => 1, Side => 4);
      Cu : constant Cube := (Id => 2, Side => 2, Depth => 3);
      Po : Polygon := (Points => 4, Id => 3, Lengths => [1, 2, 3, 4], Scale => 1);
      pragma Warnings (Off, Po);  --  changed through its Scaled'Class view
      La : constant Label := (Length => 7);

      Items : constant array (1 .. 3) of Shape_Access :=
        [new Square'(Sq), new Cube'(Cu), new Polygon'(Po)];
      Areas : Natural := 0;
   begin
      for Item of Items loop
         Areas := Areas + Item.Area;
      end loop;
      Check (Areas = 16 + 12 + 10, "dispatching through Shape'Class (heap objects)");
      Check (Twice_Area (Cu) = 24 and then Kind (Shape'Class (Cu)) = 'S'
               and then Kind (Shape'Class (Po)) = '?',
             "class-wide calls, inherited and overridden primitives");

      Check (Items (1).all in Square'Class and then Items (2).all in Square'Class
               and then Items (3).all not in Square'Class
               and then Items (2).all not in Polygon,
             "'Class membership");
      Check (Items (3).all in Named'Class and then Items (3).all in Scaled'Class
               and then Items (1).all not in Named'Class,
             "interface membership from a class-wide object");

      declare
         N : Named'Class renames Named'Class (Po);
         S : Scaled'Class renames Scaled'Class (Po);
      begin
         Check (N.Name_Length = 4 and then Named'Class (La).Name_Length = 7,
                "dispatching through an interface");
         S.Grow (3);
         Check (Po.Scale = 3 and then S.Factor = 3 and then Po.Area = 30,
                "in out dispatching through a second interface");
         Check (Shape'Class (N).Area = 30 and then Shape'Class (N) in Polygon,
                "interface view converted back to the class-wide type");
      end;

      declare
         Discriminated : Named_Access :=
           new Polygon'(Points => 6, Id => 9, Lengths => [others => 2], Scale => 1);
         Plain : Named_Access := new Label'(Length => 5);
      begin
         Check (Discriminated.Name_Length = 6 and then Plain.Name_Length = 5
                  and then Shape'Class (Discriminated.all).Area = 12,
                "heap objects through access-to-interface");
         Free (Discriminated);
         Free (Plain);
         Check (Discriminated = null and then Plain = null,
                "Unchecked_Deallocation through access-to-interface");
      end;

      declare
         A : constant Shape'Class := Make ('S');
         B : constant Shape'Class := Make ('S');
         C : constant Shape'Class := Make ('C');
         P : constant Shape'Class := Make ('P');
      begin
         Check (A = B and then A /= C and then A'Tag = Square'Tag
                  and then C.Area = 20 and then P.Area = 12,
                "class-wide function results and dispatching equality");
      end;

      Check (Expanded_Name (Polygon'Tag) = "SHAPES.POLYGON"
               and then Expanded_Name (Builders.Small'Tag) = "TAGGED_CHECKS.BUILDERS.SMALL",
             "Expanded_Name");
      Check (External_Tag (Cube'Tag) = "SHAPES.CUBE"
               and then Internal_Tag ("SHAPES.CUBE") = Cube'Tag
               and then Internal_Tag (External_Tag (Polygon'Tag)) = Polygon'Tag,
             "External_Tag and Internal_Tag (registered library-level tags)");
      Check (Parent_Tag (Cube'Tag) = Square'Tag and then Parent_Tag (Square'Tag) = Shape'Tag
               and then Parent_Tag (Shape'Tag) = No_Tag,
             "Parent_Tag");
      Check (Is_Descendant_At_Same_Level (Cube'Tag, Shape'Tag)
               and then not Is_Descendant_At_Same_Level (Shape'Tag, Cube'Tag)
               and then Descendant_Tag ("SHAPES.CUBE", Shape'Tag) = Cube'Tag,
             "Is_Descendant_At_Same_Level and Descendant_Tag");
      Check (Interface_Ancestor_Tags (Polygon'Tag)'Length = 2
               and then Interface_Ancestor_Tags (Cube'Tag)'Length = 0
               and then Is_Abstract (Shape'Tag) and then not Is_Abstract (Cube'Tag),
             "Interface_Ancestor_Tags and Is_Abstract");

      declare
         Parameters : aliased Seed := (Size => 7);
         Small : constant Builders.Built'Class := Construct (Builders.Small'Tag, Parameters'Access);
         Large : constant Builders.Built'Class := Construct (Builders.Large'Tag, Parameters'Access);
      begin
         Check (Small.Size = 7 and then Large.Size = 7_000 and then Large in Builders.Large,
                "Generic_Dispatching_Constructor");
      end;
   end Library_Level;

   --  Tagged types declared inside a subprogram: their tags are not
   --  library-level, so their external tags carry the tag's address. Their
   --  primitives read the enclosing frame (Depth): the dispatch table holds
   --  a GCC function descriptor for them, never a stack trampoline
   --  (No_Implicit_Dynamic_Code stays; Always_Compatible_Rep is False).
   procedure Nested (Depth : Natural) is
      type Local_Square is new Square with record
         Weight : Natural := Depth;
      end record;
      overriding function Area (L : Local_Square) return Natural is (L.Side * L.Weight + Depth);

      type Local_Label is new Named with null record;
      overriding function Name_Length (L : Local_Label) return Natural is (Depth);

      L : constant Local_Square := (Id => 5, Side => 3, Weight => Depth);
      W : constant Shape'Class := L;
      N : constant Named'Class := Local_Label'(null record);
      Tag_Text : constant String := External_Tag (Local_Square'Tag);
   begin
      Check (W.Area = 4 * Depth and then Twice_Area (W) = 8 * Depth and then W in Square'Class
               and then W in Local_Square and then N.Name_Length = Depth,
             "nested tagged types dispatch to primitives reading the enclosing frame");
      Check (not Is_Descendant_At_Same_Level (Local_Square'Tag, Square'Tag)
               and then Parent_Tag (Local_Square'Tag) = Square'Tag,
             "nested tagged type is at a deeper level");
      Check (Expanded_Name (Local_Square'Tag) = "TAGGED_CHECKS.NESTED.LOCAL_SQUARE"
               and then Internal_Tag (Tag_Text) = Local_Square'Tag,
             "nested tagged type External_Tag round trip");
   end Nested;

   procedure Run is
   begin
      Library_Level;
      Nested (4);
   end Run;

end Tagged_Checks;
