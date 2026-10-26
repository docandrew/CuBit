------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ada types and a canonical-source reader for one CCL type, generated at
--  build time (the typed-setting decoders, Setting_Codecs, and the typed
--  program-parameter bindings, Parameter_Bindings).
--
--  The type and every type it reaches become Ada types (records,
--  enumerations, variant records as a Choice and one payload component per
--  alternative, range types, Boolean, Integer_64 and
--  CuBit.Canonical_Readers.Text for String) and one reader each: a fixed
--  sequence of CuBit.Canonical_Readers calls that accepts exactly the
--  canonical source CCL renders for a value (every field named, in
--  declaration order). The declarations are analysed here by the CCL
--  analyser; the generated code needs no CCL at run time.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Strings.Unbounded;

package Type_Codecs is
   use Ada.Strings.Unbounded;

   Readers : constant String := "CuBit.Canonical_Readers";

   type Generated is record
      --  The Ada type declarations, one per line, indented for a package
      --  spec; and the readers (Read_<Type>), for its body, which renames
      --  Readers as R.
      Spec, Bodies : Unbounded_String;
      --  Whether a declaration uses Interfaces (an Integer field), and
      --  whether a reader does (an Integer or range read).
      Integer_Field, Integer_Read : Boolean := False;
      --  The root's Ada type, its default value, and its reader.
      Root_Type, Root_Default, Root_Reader : Unbounded_String;
   end record;

   --  Declarations: CCL (type ...) forms declaring Root_Name. Detail says
   --  why when not Success.
   procedure Generate
     (Declarations, Root_Name : String;
      Item : out Generated; Success : out Boolean; Detail : out Unbounded_String);
end Type_Codecs;
