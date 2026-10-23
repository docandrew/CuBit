------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The setting catalog (CAP-003, docs/declaration-checking.md section 1),
--  read by the manifest tool: userspace/ccl/interfaces/settings/*.ccl, each
--  one CCL expression of type Setting_Catalog (interfaces/
--  setting-catalog.ccl), checked by the type system and read by field
--  name, never by a keyword reader.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Failures;

package Setting_Catalogs is

   Maximum_Settings : constant := 64;
   Maximum_Text     : constant := 96;

   subtype Text_Length is Natural range 0 .. Maximum_Text;
   type Bounded_Text is record
      Length : Text_Length := 0;
      Data   : String (1 .. Maximum_Text) := [others => ' '];
   end record;
   function Image (Item : Bounded_Text) return String is (Item.Data (1 .. Item.Length));

   --  The catalog's Shape, Value and Class alternatives.
   type Shape is (Single, Family);
   type Value_Kind is (Typed, Text, Address, Count, Collection);
   type Class is (Declaration_Managed, Application_State);

   type Setting is record
      Key    : Bounded_Text;
      Form   : Shape := Single;
      Value  : Value_Kind := Text;
      --  Typed: the Ada constant holding the CCL declarations, and the root
      --  type's name.
      Source : Bounded_Text;
      Root   : Bounded_Text;
      Kind   : Class := Declaration_Managed;
      --  The Ada codec package; empty for the built-in ones.
      Codec  : Bounded_Text;
      --  Which catalog file declared it (for messages).
      File   : Bounded_Text;
   end record;

   subtype Setting_Count is Natural range 0 .. Maximum_Settings;
   type Setting_Array is array (1 .. Maximum_Settings) of Setting;
   type Catalog is record
      Count    : Setting_Count := 0;
      Settings : Setting_Array;
   end record;

   --  Add the settings of one catalog file (Source, named File) to Item,
   --  checked against Schema (setting-catalog.ccl). Keys are 1 .. 64
   --  characters of [a-z0-9._-] with no two separators together, a
   --  family's ends with '.', and no key is
   --  declared twice or covers another.
   procedure Add
     (Item : in out Catalog; File, Source, Schema : String;
      Success : out Boolean; Why : out CuBit.Failures.Failure);

   --  Whether a Config scope (a manifest's prefix, Config_Inspection.
   --  Contains semantics) reaches setting Index: a key it contains, or a
   --  family it contains or lies in.
   function Covers (Item : Catalog; Scope : String; Index : Positive) return Boolean
   with Pre => Index <= Item.Count;

   --  The setting a full key (a member of a family included) belongs to, or 0.
   function Find (Item : Catalog; Key : String) return Setting_Count;

end Setting_Catalogs;
