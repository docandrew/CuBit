------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;

package body CuBit.Debug.Dwarf is
   package E renames CuBit.Debug.Elf;
   use type E.Image_Result;

   --  Forms (DWARF 5, 7.5.6).
   FORM_Addr           : constant := 16#01#;
   FORM_Block2         : constant := 16#03#;
   FORM_Block4         : constant := 16#04#;
   FORM_Data2          : constant := 16#05#;
   FORM_Data4          : constant := 16#06#;
   FORM_Data8          : constant := 16#07#;
   FORM_String         : constant := 16#08#;
   FORM_Block          : constant := 16#09#;
   FORM_Block1         : constant := 16#0A#;
   FORM_Data1          : constant := 16#0B#;
   FORM_Flag           : constant := 16#0C#;
   FORM_Sdata          : constant := 16#0D#;
   FORM_Strp           : constant := 16#0E#;
   FORM_Udata          : constant := 16#0F#;
   FORM_Ref_Addr       : constant := 16#10#;
   FORM_Ref1           : constant := 16#11#;
   FORM_Ref2           : constant := 16#12#;
   FORM_Ref4           : constant := 16#13#;
   FORM_Ref8           : constant := 16#14#;
   FORM_Ref_Udata      : constant := 16#15#;
   FORM_Indirect       : constant := 16#16#;
   FORM_Sec_Offset     : constant := 16#17#;
   FORM_Exprloc        : constant := 16#18#;
   FORM_Flag_Present   : constant := 16#19#;
   FORM_Strx           : constant := 16#1A#;
   FORM_Addrx          : constant := 16#1B#;
   FORM_Ref_Sup4       : constant := 16#1C#;
   FORM_Strp_Sup       : constant := 16#1D#;
   FORM_Data16         : constant := 16#1E#;
   FORM_Line_Strp      : constant := 16#1F#;
   FORM_Ref_Sig8       : constant := 16#20#;
   FORM_Implicit_Const : constant := 16#21#;
   FORM_Loclistx       : constant := 16#22#;
   FORM_Rnglistx       : constant := 16#23#;
   FORM_Ref_Sup8       : constant := 16#24#;
   FORM_Strx1          : constant := 16#25#;
   FORM_Strx2          : constant := 16#26#;
   FORM_Strx3          : constant := 16#27#;
   FORM_Strx4          : constant := 16#28#;
   FORM_Addrx1         : constant := 16#29#;
   FORM_Addrx2         : constant := 16#2A#;
   FORM_Addrx3         : constant := 16#2B#;
   FORM_Addrx4         : constant := 16#2C#;

   UT_Compile  : constant := 16#01#;
   UT_Partial  : constant := 16#03#;
   DWARF_64_Escape : constant := 16#FFFF_FFFF#;
   Offset_Bytes : constant := 4;    --  32-bit DWARF only
   Data16_Bytes : constant := 16;
   Strx3_Bytes  : constant := 3;
   Lowest_Version : constant := 2;
   Highest_Version : constant := 5;
   Version_5 : constant := 5;

   --  Range and location list entries (DWARF 5, 7.25 and 7.29).
   RLE_End_Of_List   : constant := 16#00#;
   RLE_Base_Addressx : constant := 16#01#;
   RLE_Startx_Endx   : constant := 16#02#;
   RLE_Startx_Length : constant := 16#03#;
   RLE_Offset_Pair   : constant := 16#04#;
   RLE_Base_Address  : constant := 16#05#;
   RLE_Start_End     : constant := 16#06#;
   RLE_Start_Length  : constant := 16#07#;
   LLE_End_Of_List   : constant := 16#00#;
   LLE_Base_Addressx : constant := 16#01#;
   LLE_Startx_Endx   : constant := 16#02#;
   LLE_Startx_Length : constant := 16#03#;
   LLE_Offset_Pair   : constant := 16#04#;
   LLE_Default       : constant := 16#05#;
   LLE_Base_Address  : constant := 16#06#;
   LLE_Start_End     : constant := 16#07#;
   LLE_Start_Length  : constant := 16#08#;
   LLE_GNU_View_Pair : constant := 16#09#;
   List_Entry_Ceiling : constant := 65_536;
   --  Rows searched back from a line-0 row for its line.
   Line_Zero_Lookback : constant := 16;

   --  Line programs (DWARF 5, 6.2).
   LNS_Copy             : constant := 1;
   LNS_Advance_PC       : constant := 2;
   LNS_Advance_Line     : constant := 3;
   LNS_Set_File         : constant := 4;
   LNS_Set_Column       : constant := 5;
   LNS_Negate_Stmt      : constant := 6;
   LNS_Set_Basic_Block  : constant := 7;
   LNS_Const_Add_PC     : constant := 8;
   LNS_Fixed_Advance_PC : constant := 9;
   LNS_Set_Prologue_End : constant := 10;
   LNS_Set_Epilogue_Begin : constant := 11;
   LNS_Set_ISA          : constant := 12;
   LNE_End_Sequence     : constant := 1;
   LNE_Set_Address      : constant := 2;
   LNE_Define_File      : constant := 3;
   LNCT_Path            : constant := 1;
   LNCT_Directory_Index : constant := 2;
   Special_Opcode_Max   : constant := 255;
   Entry_Format_Ceiling : constant := 16;

   ---------------------------------------------------------------------------
   function Section_Cursor (Img : Image; S : E.Section_Name) return B.Cursor is
      Sec : constant E.Section := Img.Sections (S);
   begin
      if not Sec.In_File then
         return (At_Byte => 0, Limit => 0, Ok => False);
      end if;
      return B.Make (Sec.File_Offset, Sec.File_Offset + Sec.Size);
   end Section_Cursor;

   function Base_Of (Img : Image; S : E.Section_Name) return B.Offset is
     (Img.Sections (S).File_Offset);

   ---------------------------------------------------------------------------
   --  Attribute values
   ---------------------------------------------------------------------------
   procedure Read_Value
     (Img : Image; U : Unit; Form_In : Unsigned_64; Implicit : Integer_64;
      C : in out B.Cursor; A : in out Attribute)
   is
      Data : B.Byte_Buffer renames Img.Data.all;
      Form : Unsigned_64 := Form_In;
      V : Unsigned_64;
      S : Integer_64;
      Info_Base : constant B.Offset := Base_Of (Img, E.Debug_Info);

      procedure Block (Size_Bytes : Natural) is
         Size : Unsigned_64;
      begin
         if Size_Bytes = 0 then
            B.ULEB (Data, C, Size);
         else
            B.Sized (Data, C, Size_Bytes, Size);
         end if;
         A.Class := Block_Value;
         A.At_Byte := C.At_Byte;
         A.Size := Size;
         B.Skip (C, Size);
      end Block;

      procedure Str_Index (Index : Unsigned_64) is
         SC : B.Cursor := Section_Cursor (Img, E.Debug_Str_Offsets);
         Off : Unsigned_64;
      begin
         A.Class := String_Value;
         A.Str_Section := In_Str;
         A.Value := 0;
         if not SC.Ok then
            A.Class := No_Value;
            return;
         end if;
         B.Skip (SC, U.Str_Offsets_Base + Index * Offset_Bytes);
         B.U32 (Data, SC, Off);
         A.At_Byte := Base_Of (Img, E.Debug_Str) + Off;
         if not SC.Ok then
            A.Class := No_Value;
         end if;
      end Str_Index;
   begin
      if Form = FORM_Indirect then
         B.ULEB (Data, C, Form);
      end if;
      A.Form := Form;
      case Form is
         when FORM_Addr =>
            B.Sized (Data, C, U.Address_Size, V);
            A.Class := Address_Value;
            A.Value := V;
         when FORM_Addrx | FORM_Addrx1 | FORM_Addrx2 | FORM_Addrx3 | FORM_Addrx4 =>
            case Form is
               when FORM_Addrx  => B.ULEB (Data, C, V);
               when FORM_Addrx1 => B.U8 (Data, C, V);
               when FORM_Addrx2 => B.U16 (Data, C, V);
               when FORM_Addrx3 => B.Sized (Data, C, Strx3_Bytes, V);
               when others      => B.U32 (Data, C, V);
            end case;
            A.Class := Address_Value;
            A.Value := V;
            A.Signed := 1;   --  an index: Address_Of resolves it
         when FORM_Block1 => Block (1);
         when FORM_Block2 => Block (2);
         when FORM_Block4 => Block (4);
         when FORM_Block | FORM_Exprloc => Block (0);
         when FORM_Data1 | FORM_Data2 | FORM_Data4 | FORM_Data8 =>
            B.Sized (Data, C, (case Form is when FORM_Data1 => 1, when FORM_Data2 => 2,
                                             when FORM_Data4 => 4, when others => 8), V);
            A.Class := Constant_Value;
            A.Value := V;
            A.Signed := (case Form is when FORM_Data1 => B.Sign_Extend (V, 8),
                                      when FORM_Data2 => B.Sign_Extend (V, 16),
                                      when FORM_Data4 => B.Sign_Extend (V, 32),
                                      when others => B.To_Signed (V));
         when FORM_Data16 =>
            A.Class := Block_Value;
            A.At_Byte := C.At_Byte;
            A.Size := Data16_Bytes;
            B.Skip (C, Data16_Bytes);
         when FORM_Sdata =>
            B.SLEB (Data, C, S);
            A.Class := Signed_Value;
            A.Signed := S;
            A.Value := B.To_Unsigned (S);
         when FORM_Udata =>
            B.ULEB (Data, C, V);
            A.Class := Constant_Value;
            A.Value := V;
            A.Signed := B.To_Signed (V);
         when FORM_Implicit_Const =>
            A.Class := Signed_Value;
            A.Signed := Implicit;
            A.Value := B.To_Unsigned (Implicit);
         when FORM_String =>
            A.Class := String_Value;
            A.Str_Section := In_Info;
            A.At_Byte := C.At_Byte;
            B.Skip_String (Data, C);
         when FORM_Strp | FORM_Line_Strp | FORM_Strp_Sup =>
            B.U32 (Data, C, V);
            A.Class := String_Value;
            if Form = FORM_Line_Strp then
               A.Str_Section := In_Line_Str;
               A.At_Byte := Base_Of (Img, E.Debug_Line_Str) + V;
            else
               A.Str_Section := In_Str;
               A.At_Byte := Base_Of (Img, E.Debug_Str) + V;
            end if;
         when FORM_Strx  => B.ULEB (Data, C, V); Str_Index (V);
         when FORM_Strx1 => B.U8 (Data, C, V); Str_Index (V);
         when FORM_Strx2 => B.U16 (Data, C, V); Str_Index (V);
         when FORM_Strx3 => B.Sized (Data, C, Strx3_Bytes, V); Str_Index (V);
         when FORM_Strx4 => B.U32 (Data, C, V); Str_Index (V);
         when FORM_Flag =>
            B.U8 (Data, C, V);
            A.Class := Flag_Value;
            A.Value := (if V /= 0 then 1 else 0);
         when FORM_Flag_Present =>
            A.Class := Flag_Value;
            A.Value := 1;
         when FORM_Ref1 | FORM_Ref2 | FORM_Ref4 | FORM_Ref8 | FORM_Ref_Udata =>
            case Form is
               when FORM_Ref1 => B.U8 (Data, C, V);
               when FORM_Ref2 => B.U16 (Data, C, V);
               when FORM_Ref4 => B.U32 (Data, C, V);
               when FORM_Ref8 => B.U64 (Data, C, V);
               when others    => B.ULEB (Data, C, V);
            end case;
            A.Class := Reference_Value;
            A.Value := U.Offset + V;      --  section-relative
         when FORM_Ref_Addr =>
            if U.Version <= 2 then
               B.Sized (Data, C, U.Address_Size, V);
            else
               B.U32 (Data, C, V);
            end if;
            A.Class := Reference_Value;
            A.Value := V;
         when FORM_Ref_Sup4 =>
            B.U32 (Data, C, V);
            A.Class := No_Value;
         when FORM_Ref_Sup8 | FORM_Ref_Sig8 =>
            B.U64 (Data, C, V);
            A.Class := No_Value;
         when FORM_Sec_Offset =>
            B.U32 (Data, C, V);
            A.Class := Section_Offset;
            A.Value := V;
         when FORM_Loclistx =>
            B.ULEB (Data, C, V);
            A.Class := Loclist_Index;
            A.Value := V;
         when FORM_Rnglistx =>
            B.ULEB (Data, C, V);
            A.Class := Rnglist_Index;
            A.Value := V;
         when others =>
            C.Ok := False;   --  an unknown form: its size is unknown
      end case;
      pragma Unreferenced (Info_Base);
   end Read_Value;

   ---------------------------------------------------------------------------
   --  Units and abbreviations
   ---------------------------------------------------------------------------
   function Unit_Of (Img : Image; Die_Offset : B.Offset) return Natural is
      Low : Natural := 0;
      High : Natural := Img.Unit_Count;
      Mid : Natural;
   begin
      --  The last unit starting at or before Die_Offset.
      while High - Low > 1 loop
         Mid := (Low + High) / 2;
         if Img.Units (Mid).Offset <= Die_Offset then
            Low := Mid;
         else
            High := Mid;
         end if;
      end loop;
      return Low;
   end Unit_Of;

   function Unit_Name (Img : Image; Unit_Index : Natural) return String is
     (if Unit_Index < Img.Unit_Count and then Img.Units (Unit_Index).Has_Name
      then B.C_String (Img.Data.all, Img.Units (Unit_Index).Name_At, Name_Ceiling) else "");

   function Find_Abbrev (Img : Image; U : Unit; Code : Unsigned_64) return Integer is
   begin
      if Code >= 1 and then Code <= Unsigned_64 (U.Abbrev_Count)
        and then Img.Abbrevs (U.First_Abbrev + Natural (Code) - 1).Code = Code
      then
         return U.First_Abbrev + Natural (Code) - 1;
      end if;
      for I in U.First_Abbrev .. U.First_Abbrev + U.Abbrev_Count - 1 loop
         if Img.Abbrevs (I).Code = Code then
            return I;
         end if;
      end loop;
      return -1;
   end Find_Abbrev;

   procedure Read_Abbrevs (Img : in out Image; At_Offset : B.Offset; U : in out Unit; Ok : out Boolean) is
      Data : B.Byte_Buffer renames Img.Data.all;
      Sec : constant E.Section := Img.Sections (E.Debug_Abbrev);
      C : B.Cursor := B.Make (Sec.File_Offset + At_Offset, Sec.File_Offset + Sec.Size);
      Code, Tag, Children, Name, Form : Unsigned_64;
      Implicit : Integer_64;
   begin
      Ok := False;
      U.First_Abbrev := Img.Abbrev_Count;
      U.Abbrev_Count := 0;
      if At_Offset > Sec.Size then
         return;
      end if;
      loop
         B.ULEB (Data, C, Code);
         exit when not C.Ok or else Code = 0;
         B.ULEB (Data, C, Tag);
         B.U8 (Data, C, Children);
         if not C.Ok or else Img.Abbrev_Count = Img.Abbrevs'Length then
            return;
         end if;
         Img.Abbrevs (Img.Abbrev_Count) :=
           (Code => Code, Tag => Tag, Children => Children /= 0,
            First_Spec => Img.Spec_Count, Spec_Count => 0);
         loop
            B.ULEB (Data, C, Name);
            B.ULEB (Data, C, Form);
            Implicit := 0;
            if Form = FORM_Implicit_Const then
               B.SLEB (Data, C, Implicit);
            end if;
            if not C.Ok then
               return;
            end if;
            exit when Name = 0 and then Form = 0;
            if Img.Spec_Count = Img.Specs'Length
              or else Img.Abbrevs (Img.Abbrev_Count).Spec_Count = Attribute_Ceiling
            then
               return;
            end if;
            Img.Specs (Img.Spec_Count) := (Name => Name, Form => Form, Implicit => Implicit);
            Img.Spec_Count := Img.Spec_Count + 1;
            Img.Abbrevs (Img.Abbrev_Count).Spec_Count := Img.Abbrevs (Img.Abbrev_Count).Spec_Count + 1;
         end loop;
         Img.Abbrev_Count := Img.Abbrev_Count + 1;
         U.Abbrev_Count := U.Abbrev_Count + 1;
      end loop;
      Ok := C.Ok;
   end Read_Abbrevs;

   procedure Read_Die (Img : Image; Offset : B.Offset; D : out Die; Ok : out Boolean) is
      Data : B.Byte_Buffer renames Img.Data.all;
      Index : constant Natural := Unit_Of (Img, Offset);
      U : Unit renames Img.Units (Index);
      Base : constant B.Offset := Base_Of (Img, E.Debug_Info);
      C : B.Cursor;
      Code : Unsigned_64;
      Ab : Integer;
   begin
      D := (Offset => Offset, Unit_Index => Index, others => <>);
      Ok := False;
      if Img.Unit_Count = 0 or else Offset < U.First_Die or else Offset >= U.Limit then
         return;
      end if;
      C := B.Make (Base + Offset, Base + U.Limit);
      B.ULEB (Data, C, Code);
      if not C.Ok then
         return;
      end if;
      if Code = 0 then
         D.Next := C.At_Byte - Base;
         Ok := True;
         return;
      end if;
      Ab := Find_Abbrev (Img, U, Code);
      if Ab < 0 then
         return;
      end if;
      declare
         A : Abbrev renames Img.Abbrevs (Ab);
      begin
         D.Tag := A.Tag;
         D.Children := A.Children;
         for S in A.First_Spec .. A.First_Spec + A.Spec_Count - 1 loop
            D.Count := D.Count + 1;
            D.Attrs (D.Count) := (Name => Img.Specs (S).Name, others => <>);
            Read_Value (Img, U, Img.Specs (S).Form, Img.Specs (S).Implicit, C, D.Attrs (D.Count));
            if not C.Ok then
               return;
            end if;
         end loop;
      end;
      D.Next := C.At_Byte - Base;
      Ok := True;
   end Read_Die;

   function Find (D : Die; Name : Unsigned_64) return Attribute_Count is
   begin
      for I in 1 .. D.Count loop
         if D.Attrs (I).Name = Name then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Sibling_Of (Img : Image; D : Die) return B.Offset is
      S : constant Attribute_Count := Find (D, AT_Sibling);
      Depth : Natural := 0;
      Cur : B.Offset := D.Next;
      Child : Die;
      Ok : Boolean;
   begin
      if not D.Children then
         return D.Next;
      end if;
      if S /= 0 and then D.Attrs (S).Class = Reference_Value and then D.Attrs (S).Value > D.Offset then
         return D.Attrs (S).Value;
      end if;
      --  Walk the children: each null entry closes one level.
      Depth := 1;
      loop
         Read_Die (Img, Cur, Child, Ok);
         if not Ok then
            return Img.Units (D.Unit_Index).Limit;
         end if;
         Cur := Child.Next;
         if Child.Tag = 0 then
            Depth := Depth - 1;
            exit when Depth = 0;
         elsif Child.Children then
            declare
               CS : constant Attribute_Count := Find (Child, AT_Sibling);
            begin
               if CS /= 0 and then Child.Attrs (CS).Class = Reference_Value
                 and then Child.Attrs (CS).Value > Child.Offset
               then
                  Cur := Child.Attrs (CS).Value;
               elsif Depth < Depth_Ceiling then
                  Depth := Depth + 1;
               else
                  return Img.Units (D.Unit_Index).Limit;
               end if;
            end;
         end if;
      end loop;
      return Cur;
   end Sibling_Of;

   function Text_Of (Img : Image; A : Attribute) return String is
   begin
      if A.Class /= String_Value then
         return "";
      end if;
      return B.C_String (Img.Data.all, A.At_Byte, Name_Ceiling);
   end Text_Of;

   function Reference_Of (D : Die; Name : Unsigned_64; Target : out B.Offset) return Boolean is
      I : constant Attribute_Count := Find (D, Name);
   begin
      Target := 0;
      if I = 0 or else D.Attrs (I).Class /= Reference_Value then
         return False;
      end if;
      Target := D.Attrs (I).Value;
      return True;
   end Reference_Of;

   procedure Find_Inherited
     (Img : Image; D : Die; Name : Unsigned_64; A : out Attribute; Found : out Boolean)
   is
      I : Attribute_Count := Find (D, Name);
      Origin : B.Offset;
      O : Die;
      Ok : Boolean;
      Hops : Natural := 0;
      Max_Hops : constant := 3;
      Cur : Die := D;
   begin
      A := (others => <>);
      Found := False;
      loop
         if I /= 0 then
            A := Cur.Attrs (I);
            Found := True;
            return;
         end if;
         exit when Hops = Max_Hops;
         Hops := Hops + 1;
         if not Reference_Of (Cur, AT_Abstract_Origin, Origin)
           and then not Reference_Of (Cur, AT_Specification, Origin)
         then
            return;
         end if;
         Read_Die (Img, Origin, O, Ok);
         if not Ok then
            return;
         end if;
         Cur := O;
         I := Find (Cur, Name);
      end loop;
   end Find_Inherited;

   function Name_Of (Img : Image; D : Die) return String is
      A : Attribute;
      Found : Boolean;
   begin
      Find_Inherited (Img, D, AT_Name, A, Found);
      return (if Found then Text_Of (Img, A) else "");
   end Name_Of;

   function Constant_Of (A : Attribute; Value : out Integer_64) return Boolean is
   begin
      Value := 0;
      case A.Class is
         when Constant_Value | Signed_Value =>
            Value := A.Signed;
            return True;
         when others =>
            return False;
      end case;
   end Constant_Of;

   function Address_Of (Img : Image; Unit_Index : Natural; A : Attribute) return Unsigned_64 is
      U : Unit renames Img.Units (Unit_Index);
      C : B.Cursor;
      V : Unsigned_64;
   begin
      if A.Class /= Address_Value then
         return 0;
      end if;
      if A.Signed = 0 then
         return A.Value;
      end if;
      C := Section_Cursor (Img, E.Debug_Addr);
      B.Skip (C, U.Addr_Base + A.Value * Unsigned_64 (U.Address_Size));
      B.Sized (Img.Data.all, C, U.Address_Size, V);
      return (if C.Ok then V else 0);
   end Address_Of;

   function Indexed_Address (Img : Image; U : Unit; Index : Unsigned_64) return Unsigned_64 is
      C : B.Cursor := Section_Cursor (Img, E.Debug_Addr);
      V : Unsigned_64;
   begin
      B.Skip (C, U.Addr_Base + Index * Unsigned_64 (U.Address_Size));
      B.Sized (Img.Data.all, C, U.Address_Size, V);
      return (if C.Ok then V else 0);
   end Indexed_Address;

   ---------------------------------------------------------------------------
   --  Ranges
   ---------------------------------------------------------------------------
   type Range_Visitor is access procedure (Low, High : Unsigned_64; Stop : out Boolean);

   --  The offset (in .debug_rnglists, section-relative) a ranges attribute names.
   function Ranges_Offset (Img : Image; U : Unit; A : Attribute; Off : out B.Offset) return Boolean is
      C : B.Cursor;
      V : Unsigned_64;
   begin
      Off := 0;
      if A.Class = Section_Offset then
         Off := A.Value;
         return True;
      elsif A.Class = Rnglist_Index then
         C := Section_Cursor (Img, E.Debug_Rnglists);
         B.Skip (C, U.Rnglists_Base + A.Value * Offset_Bytes);
         B.U32 (Img.Data.all, C, V);
         Off := U.Rnglists_Base + V;
         return C.Ok;
      end if;
      return False;
   end Ranges_Offset;

   --  A DWARF 4 list's base-address selection entry: all ones.
   function Base_Selection (U : Unit) return Unsigned_64 is
     (if U.Address_Size = 8 then Unsigned_64'Last else 16#FFFF_FFFF#);

   procedure Walk_Ranges (Img : Image; U : Unit; Off : B.Offset; Visit : not null access procedure
                            (Low, High : Unsigned_64; Stop : out Boolean)) is
      Data : B.Byte_Buffer renames Img.Data.all;
      C : B.Cursor := Section_Cursor (Img, E.Debug_Rnglists);
      Kind, X, Y : Unsigned_64;
      Base : Unsigned_64 := U.Base_Address;
      Stop : Boolean := False;
   begin
      if U.Version < Version_5 then
         --  DWARF 4 .debug_ranges: address pairs relative to the base;
         --  (all ones, A) sets the base to A; (0, 0) ends the list.
         C := Section_Cursor (Img, E.Debug_Ranges);
         B.Skip (C, Off);
         for Count in 1 .. List_Entry_Ceiling loop
            B.Sized (Data, C, U.Address_Size, X);
            B.Sized (Data, C, U.Address_Size, Y);
            exit when not C.Ok or else (X = 0 and then Y = 0);
            if X = Base_Selection (U) then
               Base := Y;
            else
               Visit (Base + X, Base + Y, Stop);
               exit when Stop;
            end if;
         end loop;
         return;
      end if;
      B.Skip (C, Off);
      for Count in 1 .. List_Entry_Ceiling loop
         B.U8 (Data, C, Kind);
         exit when not C.Ok or else Kind = RLE_End_Of_List;
         case Kind is
            when RLE_Base_Addressx =>
               B.ULEB (Data, C, X);
               Base := Indexed_Address (Img, U, X);
            when RLE_Startx_Endx =>
               B.ULEB (Data, C, X);
               B.ULEB (Data, C, Y);
               Visit (Indexed_Address (Img, U, X), Indexed_Address (Img, U, Y), Stop);
            when RLE_Startx_Length =>
               B.ULEB (Data, C, X);
               B.ULEB (Data, C, Y);
               X := Indexed_Address (Img, U, X);
               Visit (X, X + Y, Stop);
            when RLE_Offset_Pair =>
               B.ULEB (Data, C, X);
               B.ULEB (Data, C, Y);
               Visit (Base + X, Base + Y, Stop);
            when RLE_Base_Address =>
               B.Sized (Data, C, U.Address_Size, Base);
            when RLE_Start_End =>
               B.Sized (Data, C, U.Address_Size, X);
               B.Sized (Data, C, U.Address_Size, Y);
               Visit (X, Y, Stop);
            when RLE_Start_Length =>
               B.Sized (Data, C, U.Address_Size, X);
               B.ULEB (Data, C, Y);
               Visit (X, X + Y, Stop);
            when others =>
               exit;
         end case;
         exit when Stop or else not C.Ok;
      end loop;
   end Walk_Ranges;

   function Contains (Img : Image; D : Die; PC : Unsigned_64) return Boolean is
      U : Unit renames Img.Units (D.Unit_Index);
      L : constant Attribute_Count := Find (D, AT_Low_PC);
      H : constant Attribute_Count := Find (D, AT_High_PC);
      R : constant Attribute_Count := Find (D, AT_Ranges);
      Low, High : Unsigned_64;
      Off : B.Offset;
      Hit : Boolean := False;
      procedure Check (Lo, Hi : Unsigned_64; Stop : out Boolean) is
      begin
         Stop := PC >= Lo and then PC < Hi;
         Hit := Hit or else Stop;
      end Check;
   begin
      if L /= 0 and then H /= 0 then
         Low := Address_Of (Img, D.Unit_Index, D.Attrs (L));
         High := (if D.Attrs (H).Class = Address_Value then Address_Of (Img, D.Unit_Index, D.Attrs (H))
                  else Low + D.Attrs (H).Value);
         return PC >= Low and then PC < High;
      elsif R /= 0 and then Ranges_Offset (Img, U, D.Attrs (R), Off) then
         Walk_Ranges (Img, U, Off, Check'Access);
         return Hit;
      end if;
      return False;
   end Contains;

   ---------------------------------------------------------------------------
   --  Location lists
   ---------------------------------------------------------------------------
   procedure Location_At
     (Img : Image; Unit_Index : Natural; A : Attribute; PC : Unsigned_64;
      Expr_At, Expr_Size : out B.Offset; Found : out Boolean)
   is
      U : Unit renames Img.Units (Unit_Index);
      Data : B.Byte_Buffer renames Img.Data.all;
      C : B.Cursor;
      Off : B.Offset := 0;
      Kind, X, Y, Len, V : Unsigned_64;
      Base : Unsigned_64 := U.Base_Address;
      Default_At, Default_Size : B.Offset := 0;
      Has_Default : Boolean := False;

      procedure Take (Lo, Hi : Unsigned_64; Matched : out Boolean) is
      begin
         B.ULEB (Data, C, Len);
         Matched := C.Ok and then PC >= Lo and then PC < Hi;
         if Matched then
            Expr_At := C.At_Byte;
            Expr_Size := Len;
         end if;
         B.Skip (C, Len);
      end Take;
      Matched : Boolean;
   begin
      Expr_At := 0;
      Expr_Size := 0;
      Found := False;
      if A.Class = Block_Value then
         Expr_At := A.At_Byte;
         Expr_Size := A.Size;
         Found := True;
         return;
      elsif A.Class = Section_Offset then
         Off := A.Value;
      elsif A.Class = Loclist_Index then
         C := Section_Cursor (Img, E.Debug_Loclists);
         B.Skip (C, U.Loclists_Base + A.Value * Offset_Bytes);
         B.U32 (Data, C, V);
         if not C.Ok then
            return;
         end if;
         Off := U.Loclists_Base + V;
      else
         return;
      end if;
      if U.Version < Version_5 then
         --  DWARF 4 .debug_loc: (begin, end) relative to the base, a 2-byte
         --  length and the expression; (all ones, A) sets the base; (0, 0)
         --  ends the list.
         C := Section_Cursor (Img, E.Debug_Loc);
         B.Skip (C, Off);
         for Count in 1 .. List_Entry_Ceiling loop
            B.Sized (Data, C, U.Address_Size, X);
            B.Sized (Data, C, U.Address_Size, Y);
            exit when not C.Ok or else (X = 0 and then Y = 0);
            if X = Base_Selection (U) then
               Base := Y;
            else
               B.U16 (Data, C, Len);
               exit when not C.Ok;
               if PC >= Base + X and then PC < Base + Y then
                  Expr_At := C.At_Byte;
                  Expr_Size := Len;
                  Found := True;
                  return;
               end if;
               B.Skip (C, Len);
            end if;
         end loop;
         return;
      end if;
      C := Section_Cursor (Img, E.Debug_Loclists);
      B.Skip (C, Off);
      for Count in 1 .. List_Entry_Ceiling loop
         B.U8 (Data, C, Kind);
         exit when not C.Ok or else Kind = LLE_End_Of_List;
         Matched := False;
         case Kind is
            when LLE_Base_Addressx =>
               B.ULEB (Data, C, X);
               Base := Indexed_Address (Img, U, X);
            when LLE_Startx_Endx =>
               B.ULEB (Data, C, X);
               B.ULEB (Data, C, Y);
               Take (Indexed_Address (Img, U, X), Indexed_Address (Img, U, Y), Matched);
            when LLE_Startx_Length =>
               B.ULEB (Data, C, X);
               B.ULEB (Data, C, Y);
               X := Indexed_Address (Img, U, X);
               Take (X, X + Y, Matched);
            when LLE_Offset_Pair =>
               B.ULEB (Data, C, X);
               B.ULEB (Data, C, Y);
               Take (Base + X, Base + Y, Matched);
            when LLE_Default =>
               B.ULEB (Data, C, Len);
               Default_At := C.At_Byte;
               Default_Size := Len;
               Has_Default := C.Ok;
               B.Skip (C, Len);
            when LLE_Base_Address =>
               B.Sized (Data, C, U.Address_Size, Base);
            when LLE_Start_End =>
               B.Sized (Data, C, U.Address_Size, X);
               B.Sized (Data, C, U.Address_Size, Y);
               Take (X, Y, Matched);
            when LLE_Start_Length =>
               B.Sized (Data, C, U.Address_Size, X);
               B.ULEB (Data, C, Y);
               Take (X, X + Y, Matched);
            when LLE_GNU_View_Pair =>
               B.ULEB (Data, C, X);
               B.ULEB (Data, C, Y);
            when others =>
               exit;
         end case;
         if Matched then
            Found := True;
            return;
         end if;
         exit when not C.Ok;
      end loop;
      if Has_Default then
         Expr_At := Default_At;
         Expr_Size := Default_Size;
         Found := True;
      end if;
   end Location_At;

   ---------------------------------------------------------------------------
   --  Line tables
   ---------------------------------------------------------------------------
   procedure Add_Row (Img : in out Image; R : Row) is
   begin
      if Img.Row_Count < Img.Rows'Length then
         Img.Rows (Img.Row_Count) := R;
         Img.Row_Count := Img.Row_Count + 1;
      end if;
   end Add_Row;

   procedure Read_Line_Program (Img : in out Image; U : in out Unit) is
      Data : B.Byte_Buffer renames Img.Data.all;
      Sec : constant E.Section := Img.Sections (E.Debug_Line);
      C : B.Cursor;
      Length, Version, Header_Length, V : Unsigned_64;
      Min_Inst, Default_Stmt, Line_Range, Opcode_Base : Unsigned_64;
      Line_Base : Integer_64;
      Program_End, Program_Start : B.Offset;
      Std_Lengths : array (1 .. Special_Opcode_Max) of Unsigned_64 := [others => 0];
      type Format is record
         Content, Form : Unsigned_64 := 0;
      end record;
      Formats : array (1 .. Entry_Format_Ceiling) of Format;
      Format_Count : Unsigned_64;
      Count : Unsigned_64;
      File_Base : constant Natural := Img.File_Count;

      procedure Read_Entries (As_Files : Boolean) is
         A : Attribute;
      begin
         B.U8 (Data, C, Format_Count);
         if Format_Count > Entry_Format_Ceiling then
            C.Ok := False;
            return;
         end if;
         for I in 1 .. Natural (Format_Count) loop
            B.ULEB (Data, C, Formats (I).Content);
            B.ULEB (Data, C, Formats (I).Form);
         end loop;
         B.ULEB (Data, C, Count);
         if not C.Ok or else Count > File_Ceiling then
            C.Ok := False;
            return;
         end if;
         for I in 1 .. Count loop
            declare
               F : File_Entry;
               Has_Path : Boolean := False;
            begin
               for J in 1 .. Natural (Format_Count) loop
                  A := (others => <>);
                  Read_Value (Img, U, Formats (J).Form, 0, C, A);
                  if not C.Ok then
                     return;
                  end if;
                  if Formats (J).Content = LNCT_Path and then A.Class = String_Value then
                     F := (Section => A.Str_Section, Name_At => A.At_Byte, Data_Section => True);
                     Has_Path := True;
                  end if;
               end loop;
               if As_Files and then Img.File_Count < Img.Files'Length then
                  Img.Files (Img.File_Count) := (if Has_Path then F else (others => <>));
                  Img.File_Count := Img.File_Count + 1;
               end if;
            end;
         end loop;
      end Read_Entries;

      Address : Unsigned_64 := 0;
      File : Unsigned_64 := 1;
      Line : Integer_64 := 1;
      Column : Unsigned_64 := 0;
      Opcode, Ext_Length, Ext_Op : Unsigned_64;
      Adv : Integer_64;
      File_Origin : Natural;   --  file index 0 (DWARF 5) or 1 (DWARF 4) is Files (File_Base)

      procedure Emit (End_Seq : Boolean) is
         Index : constant Unsigned_64 :=
           (if File >= Unsigned_64 (File_Origin) then File - Unsigned_64 (File_Origin) else 0);
      begin
         Add_Row (Img, (Address => Address,
                        File => File_Base + Natural (Unsigned_64'Min (Index, Unsigned_64 (File_Ceiling))),
                        Line => (if Line > 0 and then Line < Integer_64 (Natural'Last) then Natural (Line) else 0),
                        Column => Natural (Unsigned_64'Min (Column, Unsigned_64 (Natural'Last))),
                        End_Sequence => End_Seq));
      end Emit;
   begin
      U.First_File := File_Base;
      if not Sec.In_File or else U.Line_Offset >= Sec.Size then
         return;
      end if;
      C := B.Make (Sec.File_Offset + U.Line_Offset, Sec.File_Offset + Sec.Size);
      B.U32 (Data, C, Length);
      if not C.Ok or else Length = DWARF_64_Escape or else Length > B.Remaining (C) then
         return;
      end if;
      Program_End := C.At_Byte + Length;
      C.Limit := Program_End;
      B.U16 (Data, C, Version);
      if Version < Lowest_Version or else Version > Highest_Version then
         return;
      end if;
      if Version >= Version_5 then
         B.Skip (C, 2);   --  address_size, segment_selector_size
      end if;
      B.U32 (Data, C, Header_Length);
      Program_Start := C.At_Byte + Header_Length;
      B.U8 (Data, C, Min_Inst);
      if Version >= 4 then
         B.U8 (Data, C, V);   --  maximum_operations_per_instruction
      end if;
      B.U8 (Data, C, Default_Stmt);
      B.U8 (Data, C, V);
      Line_Base := B.Sign_Extend (V, 8);
      B.U8 (Data, C, Line_Range);
      B.U8 (Data, C, Opcode_Base);
      if not C.Ok or else Line_Range = 0 or else Opcode_Base = 0 then
         return;
      end if;
      for I in 1 .. Natural (Opcode_Base) - 1 loop
         B.U8 (Data, C, Std_Lengths (I));
      end loop;
      if Version >= Version_5 then
         Read_Entries (As_Files => False);   --  directories
         Read_Entries (As_Files => True);
         File_Origin := 0;
      else
         --  include_directories, then file_names, each list ending in "".
         loop
            B.U8 (Data, C, V);
            exit when not C.Ok or else V = 0;
            C.At_Byte := C.At_Byte - 1;
            B.Skip_String (Data, C);
         end loop;
         loop
            declare
               Start : constant B.Offset := C.At_Byte;
            begin
               B.U8 (Data, C, V);
               exit when not C.Ok or else V = 0;
               C.At_Byte := Start;
               B.Skip_String (Data, C);
               B.ULEB (Data, C, V);
               B.ULEB (Data, C, V);
               B.ULEB (Data, C, V);
               if Img.File_Count < Img.Files'Length then
                  Img.Files (Img.File_Count) := (Section => In_Info, Name_At => Start, Data_Section => False);
                  Img.File_Count := Img.File_Count + 1;
               end if;
            end;
         end loop;
         File_Origin := 1;
      end if;
      if not C.Ok then
         return;
      end if;
      U.File_Count := Img.File_Count - File_Base;
      U.Has_Lines := True;
      C.At_Byte := Program_Start;
      while C.Ok and then C.At_Byte < Program_End loop
         B.U8 (Data, C, Opcode);
         exit when not C.Ok;
         if Opcode >= Opcode_Base then
            V := Opcode - Opcode_Base;
            Address := Address + (V / Line_Range) * Min_Inst;
            Line := Line + Line_Base + Integer_64 (V mod Line_Range);
            Emit (False);
         elsif Opcode = 0 then
            B.ULEB (Data, C, Ext_Length);
            declare
               Ext_End : constant B.Offset := C.At_Byte + Ext_Length;
            begin
               B.U8 (Data, C, Ext_Op);
               if Ext_Op = LNE_End_Sequence then
                  Emit (True);
                  Address := 0;
                  File := 1;
                  Line := 1;
                  Column := 0;
               elsif Ext_Op = LNE_Set_Address then
                  B.Sized (Data, C, Natural (Unsigned_64'Min (Ext_Length - 1, 8)), Address);
               elsif Ext_Op = LNE_Define_File then
                  null;
               end if;
               C.At_Byte := Ext_End;
            end;
         else
            case Opcode is
               when LNS_Copy => Emit (False);
               when LNS_Advance_PC =>
                  B.ULEB (Data, C, V);
                  Address := Address + V * Min_Inst;
               when LNS_Advance_Line =>
                  B.SLEB (Data, C, Adv);
                  Line := Line + Adv;
               when LNS_Set_File => B.ULEB (Data, C, File);
               when LNS_Set_Column => B.ULEB (Data, C, Column);
               when LNS_Negate_Stmt | LNS_Set_Basic_Block | LNS_Set_Prologue_End
                  | LNS_Set_Epilogue_Begin => null;
               when LNS_Const_Add_PC =>
                  Address := Address + ((Special_Opcode_Max - Opcode_Base) / Line_Range) * Min_Inst;
               when LNS_Fixed_Advance_PC =>
                  B.U16 (Data, C, V);
                  Address := Address + V;
               when LNS_Set_ISA => B.ULEB (Data, C, V);
               when others =>
                  --  An opcode this reader does not know: skip its operands.
                  for I in 1 .. Std_Lengths (Natural (Opcode)) loop
                     B.ULEB (Data, C, V);
                  end loop;
            end case;
         end if;
      end loop;
      pragma Unreferenced (Default_Stmt);
   end Read_Line_Program;

   --  Rows by address, end-of-sequence rows before rows at the same address.
   function Before (L, R : Row) return Boolean is
     (L.Address < R.Address or else
      (L.Address = R.Address and then L.End_Sequence and then not R.End_Sequence));

   procedure Sort_Rows (Img : in out Image) is
      A : Row_Array renames Img.Rows.all;
      N : constant Natural := Img.Row_Count;
      procedure Sift (Start, Last : Natural) is
         Root : Natural := Start;
         Child : Natural;
         T : Row;
      begin
         loop
            Child := 2 * Root + 1;
            exit when Child > Last;
            if Child + 1 <= Last and then Before (A (Child), A (Child + 1)) then
               Child := Child + 1;
            end if;
            exit when not Before (A (Root), A (Child));
            T := A (Root);
            A (Root) := A (Child);
            A (Child) := T;
            Root := Child;
         end loop;
      end Sift;
      T : Row;
   begin
      if N < 2 then
         return;
      end if;
      for Start in reverse 0 .. (N - 2) / 2 loop
         Sift (Start, N - 1);
      end loop;
      for Last in reverse 1 .. N - 1 loop
         T := A (0);
         A (0) := A (Last);
         A (Last) := T;
         Sift (0, Last - 1);
      end loop;
   end Sort_Rows;

   procedure Line_At (Img : Image; PC : Unsigned_64; R : out Row; Found : out Boolean) is
      Low : Integer := -1;
      High : Integer := Img.Row_Count;
      Mid : Integer;
   begin
      R := (others => <>);
      Found := False;
      --  The last row with Address <= PC.
      while High - Low > 1 loop
         Mid := (Low + High) / 2;
         if Img.Rows (Mid).Address <= PC then
            Low := Mid;
         else
            High := Mid;
         end if;
      end loop;
      if Low < 0 then
         return;
      end if;
      --  Of several rows at that address, the last one describes it. A
      --  row of line 0 (LLVM's "no source line", as at a call rustc made)
      --  takes the line of the row before it in the same sequence.
      R := Img.Rows (Low);
      for Back in 1 .. Line_Zero_Lookback loop
         exit when R.End_Sequence or else R.Line /= 0 or else Low - Back < 0
           or else Img.Rows (Low - Back).End_Sequence;
         R.Line := Img.Rows (Low - Back).Line;
         R.Column := Img.Rows (Low - Back).Column;
         R.File := Img.Rows (Low - Back).File;
      end loop;
      Found := not R.End_Sequence and then R.Line /= 0;
   end Line_At;

   function File_Name (Img : Image; File : Natural) return String is
   begin
      if File >= Img.File_Count then
         return "";
      end if;
      declare
         Full : constant String := B.C_String (Img.Data.all, Img.Files (File).Name_At, Name_Ceiling);
         Start : Natural := Full'First;
      begin
         for I in Full'Range loop
            if Full (I) = '/' then
               Start := I + 1;
            end if;
         end loop;
         return Full (Start .. Full'Last);
      end;
   end File_Name;

   ---------------------------------------------------------------------------
   --  Indexes
   ---------------------------------------------------------------------------
   function Function_At (Img : Image; PC : Unsigned_64) return Integer is
      Best : Integer := -1;
      Low : Integer := -1;
      High : Integer := Img.Function_Count;
      Mid : Integer;
   begin
      --  Functions sorted by Low: the last one starting at or before PC,
      --  searching back a little for nested or overlapping ranges.
      while High - Low > 1 loop
         Mid := (Low + High) / 2;
         if Img.Functions (Mid).Low <= PC then
            Low := Mid;
         else
            High := Mid;
         end if;
      end loop;
      for I in reverse Integer'Max (0, Low - 8) .. Low loop
         if I >= 0 and then PC >= Img.Functions (I).Low and then PC < Img.Functions (I).High then
            Best := I;
            exit;
         end if;
      end loop;
      return Best;
   end Function_At;

   procedure Add_Function (Img : in out Image; F : Function_Entry) is
   begin
      if Img.Function_Count < Img.Functions'Length and then F.High > F.Low then
         Img.Functions (Img.Function_Count) := F;
         Img.Function_Count := Img.Function_Count + 1;
      end if;
   end Add_Function;

   procedure Sort_Functions (Img : in out Image) is
      A : Function_Array renames Img.Functions.all;
      N : constant Natural := Img.Function_Count;
      procedure Sift (Start, Last : Natural) is
         Root : Natural := Start;
         Child : Natural;
         T : Function_Entry;
      begin
         loop
            Child := 2 * Root + 1;
            exit when Child > Last;
            if Child + 1 <= Last and then A (Child).Low < A (Child + 1).Low then
               Child := Child + 1;
            end if;
            exit when not (A (Root).Low < A (Child).Low);
            T := A (Root);
            A (Root) := A (Child);
            A (Child) := T;
            Root := Child;
         end loop;
      end Sift;
      T : Function_Entry;
   begin
      if N < 2 then
         return;
      end if;
      for Start in reverse 0 .. (N - 2) / 2 loop
         Sift (Start, N - 1);
      end loop;
      for Last in reverse 1 .. N - 1 loop
         T := A (0);
         A (0) := A (Last);
         A (Last) := T;
         Sift (0, Last - 1);
      end loop;
   end Sort_Functions;

   --  One pass over a unit's DIEs: its functions and globals.
   procedure Index_Unit (Img : in out Image; Index : Natural) is
      U : constant Unit := Img.Units (Index);
      Cur : B.Offset := U.First_Die;
      D : Die;
      Ok : Boolean;
      Depth : Natural := 0;
      DW_OP_Addr : constant := 16#03#;
   begin
      while Cur < U.Limit loop
         Read_Die (Img, Cur, D, Ok);
         exit when not Ok;
         Cur := D.Next;
         if D.Tag = 0 then
            exit when Depth = 0;
            Depth := Depth - 1;
         else
            if D.Tag = TAG_Subprogram then
               declare
                  L : constant Attribute_Count := Find (D, AT_Low_PC);
                  H : constant Attribute_Count := Find (D, AT_High_PC);
                  R : constant Attribute_Count := Find (D, AT_Ranges);
                  Low, High : Unsigned_64;
                  Off : B.Offset;
                  procedure Add (Lo, Hi : Unsigned_64; Stop : out Boolean) is
                  begin
                     Add_Function (Img, (Low => Lo, High => Hi, Die_Offset => D.Offset,
                                         Unit_Index => Index));
                     Stop := False;
                  end Add;
               begin
                  if L /= 0 and then H /= 0 then
                     Low := Address_Of (Img, Index, D.Attrs (L));
                     High := (if D.Attrs (H).Class = Address_Value
                              then Address_Of (Img, Index, D.Attrs (H))
                              else Low + D.Attrs (H).Value);
                     Add_Function (Img, (Low => Low, High => High, Die_Offset => D.Offset,
                                         Unit_Index => Index));
                  elsif R /= 0 and then Ranges_Offset (Img, U, D.Attrs (R), Off) then
                     Walk_Ranges (Img, U, Off, Add'Access);
                  end if;
               end;
            elsif D.Tag = TAG_Variable then
               --  At library level (Ada) or in a namespace or function
               --  (Rust statics): any variable at a fixed address.
               declare
                  L : constant Attribute_Count := Find (D, AT_Location);
               begin
                  if L /= 0 and then D.Attrs (L).Class = Block_Value
                    and then D.Attrs (L).Size = 1 + B.Offset (U.Address_Size)
                    and then Img.Data (Natural (D.Attrs (L).At_Byte)) = DW_OP_Addr
                    and then Img.Global_Count < Img.Globals'Length
                  then
                     declare
                        C : B.Cursor := B.Make (D.Attrs (L).At_Byte + 1,
                                                D.Attrs (L).At_Byte + D.Attrs (L).Size);
                        A : Unsigned_64;
                     begin
                        B.Sized (Img.Data.all, C, U.Address_Size, A);
                        if C.Ok then
                           Img.Globals (Img.Global_Count) :=
                             (Die_Offset => D.Offset, Unit_Index => Index, Address => A);
                           Img.Global_Count := Img.Global_Count + 1;
                        end if;
                     end;
                  end if;
               end;
            end if;
            if D.Children then
               if Depth < Depth_Ceiling then
                  Depth := Depth + 1;
               else
                  exit;
               end if;
            end if;
         end if;
      end loop;
   end Index_Unit;

   ---------------------------------------------------------------------------
   --  Load
   ---------------------------------------------------------------------------
   procedure Free_Data is new Ada.Unchecked_Deallocation (B.Byte_Buffer, B.Buffer_Access);
   procedure Free_Units is new Ada.Unchecked_Deallocation (Unit_Array, Unit_Array_Access);
   procedure Free_Abbrevs is new Ada.Unchecked_Deallocation (Abbrev_Array, Abbrev_Array_Access);
   procedure Free_Specs is new Ada.Unchecked_Deallocation (Spec_Array, Spec_Array_Access);
   procedure Free_Functions is new Ada.Unchecked_Deallocation (Function_Array, Function_Array_Access);
   procedure Free_Globals is new Ada.Unchecked_Deallocation (Global_Array, Global_Array_Access);
   procedure Free_Rows is new Ada.Unchecked_Deallocation (Row_Array, Row_Array_Access);
   procedure Free_Files is new Ada.Unchecked_Deallocation (File_Array, File_Array_Access);
   procedure Free_Image is new Ada.Unchecked_Deallocation (Image, Image_Access);

   procedure Free (Img : in out Image_Access) is
   begin
      if Img = null then
         return;
      end if;
      Free_Data (Img.Data);
      Free_Units (Img.Units);
      Free_Abbrevs (Img.Abbrevs);
      Free_Specs (Img.Specs);
      Free_Functions (Img.Functions);
      Free_Globals (Img.Globals);
      Free_Rows (Img.Rows);
      Free_Files (Img.Files);
      Free_Image (Img);
   end Free;

   function Capped (Wanted : B.Offset; Ceiling : Natural) return Natural is
     (Natural (B.Offset'Min (B.Offset'Max (Wanted, 16), B.Offset (Ceiling))));

   procedure Load (Data : B.Buffer_Access; Img : out Image_Access; Result : out Load_Result) is
      Opened : E.Image_Result;
      Info_Size, Abbrev_Size, Line_Size : B.Offset;
      Unit_Guess : Natural;
   begin
      Img := new Image;
      Img.Data := Data;
      Result := Not_Elf;
      E.Read_Sections (Data.all, Img.Sections, Opened);
      if Opened /= E.Opened then
         return;
      end if;
      Result := No_Debug_Info;
      if not Img.Sections (E.Debug_Info).In_File or else not Img.Sections (E.Debug_Abbrev).In_File then
         return;
      end if;
      for S in E.Section_Name loop
         if Img.Sections (S).Present and then Img.Sections (S).Compressed then
            Result := Compressed_Sections;
            return;
         end if;
      end loop;
      Info_Size := Img.Sections (E.Debug_Info).Size;
      Abbrev_Size := Img.Sections (E.Debug_Abbrev).Size;
      Line_Size := (if Img.Sections (E.Debug_Line).In_File then Img.Sections (E.Debug_Line).Size else 0);
      --  Sized from the sections: each entry takes at least a few bytes.
      Unit_Guess := Capped (Info_Size / 11 + 1, Unit_Ceiling);
      Img.Units := new Unit_Array (0 .. Unit_Guess - 1);
      Img.Abbrevs := new Abbrev_Array (0 .. Capped (Abbrev_Size / 3 + 1, Abbrev_Ceiling) - 1);
      Img.Specs := new Spec_Array (0 .. Capped (Abbrev_Size / 2 + 1, Spec_Ceiling) - 1);
      Img.Functions := new Function_Array (0 .. Capped (Info_Size / 16 + 1, Function_Ceiling) - 1);
      Img.Globals := new Global_Array (0 .. Capped (Info_Size / 16 + 1, Global_Ceiling) - 1);
      Img.Rows := new Row_Array (0 .. Capped (Line_Size + 1, Row_Ceiling) - 1);
      Img.Files := new File_Array (0 .. Capped (Line_Size / 4 + 1, File_Ceiling) - 1);

      Result := Malformed_Info;
      declare
         Info : constant E.Section := Img.Sections (E.Debug_Info);
         C : B.Cursor := B.Make (Info.File_Offset, Info.File_Offset + Info.Size);
         Length, Version, Kind, Addr_Size, Abbrev_Offset : Unsigned_64;
         U : Unit;
         D : Die;
         Ok : Boolean;
      begin
         while C.Ok and then C.At_Byte < C.Limit and then Img.Unit_Count < Img.Units'Length loop
            U := (others => <>);
            U.Offset := C.At_Byte - Info.File_Offset;
            B.U32 (Data.all, C, Length);
            exit when not C.Ok or else Length = DWARF_64_Escape or else Length > B.Remaining (C);
            U.Limit := C.At_Byte + Length - Info.File_Offset;
            B.U16 (Data.all, C, Version);
            exit when Version < Lowest_Version or else Version > Highest_Version;
            U.Version := Natural (Version);
            if Version >= Version_5 then
               B.U8 (Data.all, C, Kind);
               B.U8 (Data.all, C, Addr_Size);
               B.U32 (Data.all, C, Abbrev_Offset);
            else
               Kind := UT_Compile;
               B.U32 (Data.all, C, Abbrev_Offset);
               B.U8 (Data.all, C, Addr_Size);
            end if;
            exit when not C.Ok or else Addr_Size not in 4 | 8;
            U.Address_Size := Natural (Addr_Size);
            U.First_Die := C.At_Byte - Info.File_Offset;
            if Kind = UT_Compile or else Kind = UT_Partial then
               Read_Abbrevs (Img.all, Abbrev_Offset, U, Ok);
               if Ok then
                  Img.Units (Img.Unit_Count) := U;
                  Img.Unit_Count := Img.Unit_Count + 1;
                  --  The unit DIE: its base address, list bases and lines.
                  Read_Die (Img.all, U.First_Die, D, Ok);
                  if Ok and then D.Tag = TAG_Compile_Unit then
                     declare
                        UR : Unit renames Img.Units (Img.Unit_Count - 1);
                        I : Attribute_Count;
                     begin
                        I := Find (D, AT_Str_Offsets_Base);
                        if I /= 0 then UR.Str_Offsets_Base := D.Attrs (I).Value; end if;
                        I := Find (D, AT_Addr_Base);
                        if I /= 0 then UR.Addr_Base := D.Attrs (I).Value; end if;
                        I := Find (D, AT_Loclists_Base);
                        if I /= 0 then UR.Loclists_Base := D.Attrs (I).Value; end if;
                        I := Find (D, AT_Rnglists_Base);
                        if I /= 0 then UR.Rnglists_Base := D.Attrs (I).Value; end if;
                        I := Find (D, AT_Language);
                        if I /= 0 then UR.Language := D.Attrs (I).Value; end if;
                        I := Find (D, AT_Name);
                        if I /= 0 and then D.Attrs (I).Class = String_Value then
                           UR.Name_At := D.Attrs (I).At_Byte;
                           UR.Has_Name := True;
                        end if;
                        I := Find (D, AT_Low_PC);
                        if I /= 0 then UR.Base_Address := Address_Of (Img.all, Img.Unit_Count - 1, D.Attrs (I)); end if;
                        I := Find (D, AT_Stmt_List);
                        if I /= 0 then
                           UR.Line_Offset := D.Attrs (I).Value;
                           Read_Line_Program (Img.all, UR);
                        end if;
                     end;
                  end if;
               end if;
            end if;
            C.At_Byte := U.Limit + Info.File_Offset;
         end loop;
      end;
      if Img.Unit_Count = 0 then
         return;
      end if;
      for I in 0 .. Img.Unit_Count - 1 loop
         Index_Unit (Img.all, I);
      end loop;
      Sort_Functions (Img.all);
      Sort_Rows (Img.all);
      Result := Loaded;
   end Load;

end CuBit.Debug.Dwarf;
