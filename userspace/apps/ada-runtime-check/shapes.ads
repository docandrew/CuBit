------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Library-level tagged types for ada-runtime-check: an abstract root, two
--  interfaces, a discriminated extension and a type with both interfaces.
------------------------------------------------------------------------------
pragma Ada_2022;
package Shapes is

   type Shape is abstract tagged record
      Id : Natural := 0;
   end record;
   function Area (S : Shape) return Natural is abstract;
   function Kind (S : Shape) return Character;
   --  Class-wide caller: dispatches to Area.
   function Twice_Area (S : Shape'Class) return Natural;

   type Named is interface;
   function Name_Length (N : Named) return Natural is abstract;

   type Scaled is interface;
   function Factor (S : Scaled) return Natural is abstract;
   procedure Grow (S : in out Scaled; By : Natural) is abstract;

   type Square is new Shape with record
      Side : Natural := 0;
   end record;
   overriding function Area (S : Square) return Natural;
   overriding function Kind (S : Square) return Character;

   Max_Points : constant := 8;
   subtype Point_Count is Natural range 0 .. Max_Points;
   type Point_Array is array (Point_Count range <>) of Natural;

   --  A discriminated extension with a variable-size component, and both
   --  interfaces: its interface parts sit at an offset that depends on the
   --  object, the case Offset_To_Top/Base_Address handle.
   type Polygon (Points : Point_Count) is new Shape and Named and Scaled with record
      Lengths : Point_Array (1 .. Points) := [others => 0];
      Scale   : Natural := 1;
   end record;
   overriding function Area (P : Polygon) return Natural;
   overriding function Name_Length (P : Polygon) return Natural;
   overriding function Factor (P : Polygon) return Natural;
   overriding procedure Grow (P : in out Polygon; By : Natural);

   --  A grandchild through a non-abstract parent.
   type Cube is new Square with record
      Depth : Natural := 0;
   end record;
   overriding function Area (C : Cube) return Natural;

   --  Only Named.
   type Label is new Named with record
      Length : Natural := 0;
   end record;
   overriding function Name_Length (L : Label) return Natural;

end Shapes;
