--  Nominal, bounded type descriptions. Products and sums refer only to
--  previously published types: no recursive layouts or forward references.
package CCL.Types with SPARK_Mode is
   Maximum_Name_Length : constant := 32;
   Maximum_Declarations : constant := 32;
   Maximum_Components : constant := 16;
   Maximum_Value_Cells : constant := 256;

   type Builtin is (Invalid, Integer_Kind, Boolean_Kind, String_Kind,
                    Character_Kind, Handler_Kind, Unit_Kind);
   type Type_Reference is range 0 .. Builtin'Pos (Unit_Kind) + Maximum_Declarations;
   Invalid_Type : constant Type_Reference := Type_Reference (Builtin'Pos (Invalid));
   Integer_Type : constant Type_Reference := Type_Reference (Builtin'Pos (Integer_Kind));
   Boolean_Type : constant Type_Reference := Type_Reference (Builtin'Pos (Boolean_Kind));
   String_Type : constant Type_Reference := Type_Reference (Builtin'Pos (String_Kind));
   Character_Type : constant Type_Reference := Type_Reference (Builtin'Pos (Character_Kind));
   Handler_Type : constant Type_Reference := Type_Reference (Builtin'Pos (Handler_Kind));
   Unit_Type : constant Type_Reference := Type_Reference (Builtin'Pos (Unit_Kind));
   subtype Declared_Type is Type_Reference range Unit_Type + 1 .. Type_Reference'Last;
   subtype Registry_Bound is Type_Reference range Unit_Type .. Type_Reference'Last;
   subtype Component_Count is Natural range 0 .. Maximum_Components;
   subtype Component_Index is Positive range 1 .. Maximum_Components;
   subtype Cell_Count is Natural range 0 .. Maximum_Value_Cells;
   type Name is record
      Length : Natural range 0 .. Maximum_Name_Length := 0;
      Data : String (1 .. Maximum_Name_Length) := [others => ' '];
   end record;
   function Named (Text : String) return Name;
   function Image (Item : Name) return String is (Item.Data (1 .. Item.Length));
   function Same (Left, Right : Name) return Boolean is
     (Image (Left) = Image (Right));
   function Valid_Name (Item : Name) return Boolean;

   type Shape is (Primitive, Product, Sum, Resource);
   --  In a product these are fields; in a sum they are alternatives whose
   --  payload type may itself be a product. Unit is the empty product.
   --  Resource is an opaque live reference, not a constructible record or an
   --  integer. Its parts describe named type parameters (for example Value:T
   --  for a collection), not stored fields. Describing it grants no authority.
   type Component is record
      Identifier : Name;
      Payload : Type_Reference := Invalid_Type;
   end record;
   type Component_Array is array (Component_Index) of Component;
   type Description is record
      Identifier : Name;
      Form : Shape := Primitive;
      Count : Component_Count := 0;
      Parts : Component_Array := [others => (others => <>)];
   end record;
   type Registry is private;
   type Definition_Result is
     (Defined, Invalid_Name, Duplicate_Name, Invalid_Shape,
      Invalid_Reference, Layout_Too_Large, Registry_Full);
   function Last (Item : Registry) return Registry_Bound;
   function Known (Item : Registry; Ref : Type_Reference) return Boolean is
     (Ref /= Invalid_Type and then Ref <= Last (Item));
   function Find (Item : Registry; Identifier : Name) return Type_Reference;
   function Describe (Item : Registry; Ref : Type_Reference) return Description;
   function Cells (Item : Registry; Ref : Type_Reference) return Cell_Count;
   function Is_Enumeration (Item : Registry; Ref : Type_Reference) return Boolean;
   function Is_Scalar_Sum (Item : Registry; Ref : Type_Reference) return Boolean;
   function Alternative (Item : Registry; Ref : Type_Reference; Identifier : Name)
     return Component_Count;
   procedure Resolve_Alternative
     (Item : Registry; Qualified : Name; Ref : out Type_Reference;
      Choice : out Component_Count);
   procedure Define
     (Item : in out Registry; Definition : Description;
      Ref : out Type_Reference; Result : out Definition_Result)
   with Post =>
     (if Result = Defined then Ref = Last (Item) and
        Last (Item) = Last (Item'Old) + 1
      else Ref = Invalid_Type and Item = Item'Old);

   --  Materialize a nominal unary resource type from an already known value
   --  type. For example ConfigCollection with Value and Preferences becomes
   --  ConfigCollection-Preferences. The identifier-safe spelling is internal;
   --  presentation may render ConfigCollection<Preferences>. This describes a
   --  resource family only, never an authority or a live resource instance.
   type Unary_Resource_Result is
     (Resource_Specialized,
      Resource_Already_Specialized,
      Invalid_Resource_Family,
      Invalid_Resource_Parameter,
      Resource_Name_Too_Long,
      Resource_Definition_Conflict,
      Resource_Registry_Full);
   procedure Specialize_Unary_Resource
     (Item : in out Registry; Family, Parameter_Label : Name;
      Parameter : Type_Reference; Ref : out Type_Reference;
      Result : out Unary_Resource_Result)
   with Global => null,
     Post =>
       (if Result = Resource_Specialized then Ref = Last (Item) and
          Last (Item) = Last (Item'Old) + 1
        elsif Result = Resource_Already_Specialized then
          Ref /= Invalid_Type and Item = Item'Old
        else Ref = Invalid_Type and Item = Item'Old);

   type Import_Result is
     (Imported, Invalid_Root, Conflicting_Definition, Import_Full);
   --  Import the named root and its transitive dependencies only. Existing
   --  names must have identical definitions after local-reference translation.
   --  No partial publication on failure, replacement, constructors or effects.
   procedure Import_Definition
     (Source : Registry; Root : Type_Reference; Target : in out Registry;
      Ref : out Type_Reference; Result : out Import_Result)
   with Global => null, Post =>
     (if Result /= Imported then Ref = Invalid_Type and Target = Target'Old);
private
   type Definition_Array is array (Declared_Type) of Description;
   type Layout_Array is array (Declared_Type) of Cell_Count;
   type Registry is record
      Used : Registry_Bound := Unit_Type;
      Definitions : Definition_Array := [others => (others => <>)];
      Layouts : Layout_Array := [others => 0];
   end record;
   function Last (Item : Registry) return Registry_Bound is (Item.Used);
end CCL.Types;
