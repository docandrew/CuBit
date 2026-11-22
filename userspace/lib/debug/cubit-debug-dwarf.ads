------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A focused DWARF 4/5 reader (DBG-001 preview): units and abbreviations,
--  DIEs and their attributes, line tables, location and range lists, and
--  the indexes the engine needs (functions by PC, globals by name).
--
--  @description
--  The debug file is untrusted input: every read is bounded
--  (CuBit.Debug.Bytes), every table has a named ceiling, and a malformed
--  part is skipped or reported, never trusted.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Debug.Bytes;
with CuBit.Debug.Elf;

package CuBit.Debug.Dwarf is
   package B renames CuBit.Debug.Bytes;

   ---------------------------------------------------------------------------
   --  DWARF constants (DWARF 5, section 7)
   ---------------------------------------------------------------------------
   TAG_Array_Type          : constant := 16#01#;
   TAG_Enumeration_Type    : constant := 16#04#;
   TAG_Formal_Parameter    : constant := 16#05#;
   TAG_Lexical_Block       : constant := 16#0B#;
   TAG_Member              : constant := 16#0D#;
   TAG_Pointer_Type        : constant := 16#0F#;
   TAG_Reference_Type      : constant := 16#10#;
   TAG_Compile_Unit        : constant := 16#11#;
   TAG_Structure_Type      : constant := 16#13#;
   TAG_Subroutine_Type     : constant := 16#15#;
   TAG_Typedef             : constant := 16#16#;
   TAG_Union_Type          : constant := 16#17#;
   TAG_Variant             : constant := 16#19#;
   TAG_Inlined_Subroutine  : constant := 16#1D#;
   TAG_Subrange_Type       : constant := 16#21#;
   TAG_Base_Type           : constant := 16#24#;
   TAG_Const_Type          : constant := 16#26#;
   TAG_Enumerator          : constant := 16#28#;
   TAG_Subprogram          : constant := 16#2E#;
   TAG_Variant_Part        : constant := 16#33#;
   TAG_Variable            : constant := 16#34#;
   TAG_Volatile_Type       : constant := 16#35#;
   TAG_Restrict_Type       : constant := 16#37#;
   TAG_Unspecified_Type    : constant := 16#3B#;
   TAG_Rvalue_Reference    : constant := 16#42#;
   TAG_Atomic_Type         : constant := 16#47#;
   TAG_Call_Site           : constant := 16#48#;
   TAG_Call_Site_Parameter : constant := 16#49#;

   AT_Sibling              : constant := 16#01#;
   AT_Location             : constant := 16#02#;
   AT_Name                 : constant := 16#03#;
   AT_Byte_Size            : constant := 16#0B#;
   AT_Stmt_List            : constant := 16#10#;
   AT_Low_PC               : constant := 16#11#;
   AT_High_PC              : constant := 16#12#;
   AT_Language             : constant := 16#13#;
   AT_Discr                : constant := 16#15#;
   AT_Discr_Value          : constant := 16#16#;
   AT_Comp_Dir             : constant := 16#1B#;
   AT_Const_Value          : constant := 16#1C#;
   AT_Lower_Bound          : constant := 16#22#;
   AT_Upper_Bound          : constant := 16#2F#;
   AT_Abstract_Origin      : constant := 16#31#;
   AT_Count                : constant := 16#37#;
   AT_Data_Member_Location : constant := 16#38#;
   AT_Decl_File            : constant := 16#3A#;
   AT_Decl_Line            : constant := 16#3B#;
   AT_Declaration          : constant := 16#3C#;
   AT_Discr_List           : constant := 16#3D#;
   AT_Encoding             : constant := 16#3E#;
   AT_External             : constant := 16#3F#;
   AT_Frame_Base           : constant := 16#40#;
   AT_Specification        : constant := 16#47#;
   AT_Type                 : constant := 16#49#;
   AT_Ranges               : constant := 16#55#;
   AT_Data_Bit_Offset      : constant := 16#6B#;
   AT_Linkage_Name         : constant := 16#6E#;
   AT_Str_Offsets_Base     : constant := 16#72#;
   AT_Addr_Base            : constant := 16#73#;
   AT_Rnglists_Base        : constant := 16#74#;
   AT_Call_Return_PC       : constant := 16#7D#;
   AT_Call_Value           : constant := 16#7E#;
   AT_Call_Origin          : constant := 16#7F#;
   AT_Loclists_Base        : constant := 16#8C#;

   ATE_Address       : constant := 16#01#;
   ATE_Boolean       : constant := 16#02#;
   ATE_Float         : constant := 16#04#;
   ATE_Signed        : constant := 16#05#;
   ATE_Signed_Char   : constant := 16#06#;
   ATE_Unsigned      : constant := 16#07#;
   ATE_Unsigned_Char : constant := 16#08#;
   ATE_UTF           : constant := 16#10#;

   ---------------------------------------------------------------------------
   --  Ceilings
   ---------------------------------------------------------------------------
   Unit_Ceiling        : constant := 4096;
   Attribute_Ceiling   : constant := 48;
   Function_Ceiling    : constant := 65_536;
   Global_Ceiling      : constant := 32_768;
   Row_Ceiling         : constant := 2_000_000;
   File_Ceiling        : constant := 65_536;
   Abbrev_Ceiling      : constant := 262_144;
   Spec_Ceiling        : constant := 2_097_152;
   Name_Ceiling        : constant := 256;
   --  The deepest DIE nesting the walks follow.
   Depth_Ceiling       : constant := 64;

   ---------------------------------------------------------------------------
   --  Units
   ---------------------------------------------------------------------------
   type Unit is record
      Offset        : B.Offset := 0;   --  its header in .debug_info
      First_Die     : B.Offset := 0;
      Limit         : B.Offset := 0;   --  past its last byte
      Version       : Natural := 0;
      Address_Size  : Natural := 8;
      First_Abbrev  : Natural := 0;    --  in Abbrevs
      Abbrev_Count  : Natural := 0;
      Base_Address  : Unsigned_64 := 0;
      Str_Offsets_Base, Addr_Base, Loclists_Base, Rnglists_Base : B.Offset := 0;
      Line_Offset   : B.Offset := 0;
      Has_Lines     : Boolean := False;
      First_File    : Natural := 0;    --  its line program's files in Files
      File_Count    : Natural := 0;
      Language      : Unsigned_64 := 0;
      Name_At       : B.Offset := 0;     --  its DW_AT_name (a string's offset)
      Has_Name      : Boolean := False;
   end record;
   type Unit_Array is array (Natural range <>) of Unit;
   type Unit_Array_Access is access Unit_Array;

   type Attribute_Spec is record
      Name, Form : Unsigned_64 := 0;
      Implicit   : Integer_64 := 0;
   end record;
   type Spec_Array is array (Natural range <>) of Attribute_Spec;
   type Spec_Array_Access is access Spec_Array;
   type Abbrev is record
      Code, Tag  : Unsigned_64 := 0;
      Children   : Boolean := False;
      First_Spec : Natural := 0;
      Spec_Count : Natural := 0;
   end record;
   type Abbrev_Array is array (Natural range <>) of Abbrev;
   type Abbrev_Array_Access is access Abbrev_Array;

   ---------------------------------------------------------------------------
   --  DIEs
   ---------------------------------------------------------------------------
   type Value_Class is
     (No_Value, Constant_Value, Signed_Value, Address_Value, Reference_Value,
      String_Value,    --  At: the string's offset in its section (Str_Section)
      Block_Value,     --  At, Size: an expression or block in .debug_info
      Flag_Value, Section_Offset, Loclist_Index, Rnglist_Index);
   type String_Section is (In_Info, In_Str, In_Line_Str);

   type Attribute is record
      Name   : Unsigned_64 := 0;
      Form   : Unsigned_64 := 0;
      Class  : Value_Class := No_Value;
      Value  : Unsigned_64 := 0;
      Signed : Integer_64 := 0;
      At_Byte, Size : B.Offset := 0;
      Str_Section : String_Section := In_Info;
   end record;
   subtype Attribute_Count is Natural range 0 .. Attribute_Ceiling;
   type Attribute_Array is array (1 .. Attribute_Ceiling) of Attribute;

   type Die is record
      Offset   : B.Offset := 0;
      Unit_Index : Natural := 0;
      Tag      : Unsigned_64 := 0;     --  0: a null entry (end of children)
      Children : Boolean := False;
      Attrs    : Attribute_Array;
      Count    : Attribute_Count := 0;
      Next     : B.Offset := 0;        --  first child, or the next sibling
   end record;

   ---------------------------------------------------------------------------
   --  Indexes
   ---------------------------------------------------------------------------
   type Function_Entry is record
      Low, High : Unsigned_64 := 0;    --  [Low, High)
      Die_Offset : B.Offset := 0;
      Unit_Index : Natural := 0;
   end record;
   type Function_Array is array (Natural range <>) of Function_Entry;
   type Function_Array_Access is access Function_Array;

   type Global_Entry is record
      Die_Offset : B.Offset := 0;
      Unit_Index : Natural := 0;
      Address    : Unsigned_64 := 0;
   end record;
   type Global_Array is array (Natural range <>) of Global_Entry;
   type Global_Array_Access is access Global_Array;

   type Row is record
      Address : Unsigned_64 := 0;
      File    : Natural := 0;          --  in Files
      Line    : Natural := 0;
      Column  : Natural := 0;
      End_Sequence : Boolean := False;
   end record;
   type Row_Array is array (Natural range <>) of Row;
   type Row_Array_Access is access Row_Array;

   type File_Entry is record
      Section : String_Section := In_Info;  --  where its name is
      Name_At : B.Offset := 0;
      Data_Section : Boolean := True;        --  False: name in .debug_line
   end record;
   type File_Array is array (Natural range <>) of File_Entry;
   type File_Array_Access is access File_Array;

   type Image is record
      Data       : B.Buffer_Access;
      Sections   : CuBit.Debug.Elf.Section_Table;
      Units      : Unit_Array_Access;
      Unit_Count : Natural := 0;
      Abbrevs    : Abbrev_Array_Access;
      Abbrev_Count : Natural := 0;
      Specs      : Spec_Array_Access;
      Spec_Count : Natural := 0;
      Functions  : Function_Array_Access;
      Function_Count : Natural := 0;
      Globals    : Global_Array_Access;
      Global_Count : Natural := 0;
      Rows       : Row_Array_Access;
      Row_Count  : Natural := 0;
      Files      : File_Array_Access;
      File_Count : Natural := 0;
   end record;
   type Image_Access is access Image;

   type Load_Result is (Loaded, Not_Elf, No_Debug_Info, Compressed_Sections, Malformed_Info);

   --  Index Data (a whole debug file or unstripped executable, Data'First = 0).
   --  Img owns Data afterwards.
   procedure Load (Data : B.Buffer_Access; Img : out Image_Access; Result : out Load_Result);
   procedure Free (Img : in out Image_Access);

   --  The section's bytes as a cursor (Ok False when absent).
   function Section_Cursor (Img : Image; S : CuBit.Debug.Elf.Section_Name) return B.Cursor;

   function Unit_Of (Img : Image; Die_Offset : B.Offset) return Natural;
   --  A unit's source file name as its unit DIE gives it, or "".
   function Unit_Name (Img : Image; Unit_Index : Natural) return String;
   procedure Read_Die (Img : Image; Offset : B.Offset; D : out Die; Ok : out Boolean);
   --  The attribute's index in D, or 0.
   function Find (D : Die; Name : Unsigned_64) return Attribute_Count;
   --  The offset after D and all its children.
   function Sibling_Of (Img : Image; D : Die) return B.Offset;

   --  The string an attribute names, or "".
   function Text_Of (Img : Image; A : Attribute) return String;
   --  D's DW_AT_name, following DW_AT_abstract_origin and DW_AT_specification.
   function Name_Of (Img : Image; D : Die) return String;
   --  The DIE an attribute refers to (DW_AT_type, abstract_origin ...).
   function Reference_Of (D : Die; Name : Unsigned_64; Target : out B.Offset) return Boolean;
   --  D's attribute Name, or that of the DIE it is an instance or
   --  completion of (abstract_origin, specification), one level deep.
   procedure Find_Inherited
     (Img : Image; D : Die; Name : Unsigned_64; A : out Attribute; Found : out Boolean);
   --  A constant attribute's value (signed); False when not a constant.
   function Constant_Of (A : Attribute; Value : out Integer_64) return Boolean;
   --  Resolve an address attribute (addr or addrx forms).
   function Address_Of (Img : Image; Unit_Index : Natural; A : Attribute) return Unsigned_64;

   --  Whether PC lies in D's ranges (low/high or DW_AT_ranges).
   function Contains (Img : Image; D : Die; PC : Unsigned_64) return Boolean;

   --  The function whose range holds PC: an index in Functions, or -1.
   function Function_At (Img : Image; PC : Unsigned_64) return Integer;

   --  The line-table row for PC (the row starting at or before it), or
   --  Found False.
   procedure Line_At (Img : Image; PC : Unsigned_64; R : out Row; Found : out Boolean);
   function File_Name (Img : Image; File : Natural) return String;

   --  The location expression in force at PC for a location attribute:
   --  an exprloc directly, or the matching entry of its location list.
   procedure Location_At
     (Img : Image; Unit_Index : Natural; A : Attribute; PC : Unsigned_64;
      Expr_At, Expr_Size : out B.Offset; Found : out Boolean);

end CuBit.Debug.Dwarf;
