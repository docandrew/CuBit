------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;

package body CuBit.Debug.Cfi is
   use type B.Offset;

   --  Pointer encodings (LSB, eh_frame).
   PE_Omit     : constant := 16#FF#;
   PE_Format   : constant := 16#0F#;
   PE_Apply    : constant := 16#70#;
   PE_Indirect : constant := 16#80#;
   PE_Absptr   : constant := 16#00#;
   PE_Uleb128  : constant := 16#01#;
   PE_Udata2   : constant := 16#02#;
   PE_Udata4   : constant := 16#03#;
   PE_Udata8   : constant := 16#04#;
   PE_Sleb128  : constant := 16#09#;
   PE_Sdata2   : constant := 16#0A#;
   PE_Sdata4   : constant := 16#0B#;
   PE_Sdata8   : constant := 16#0C#;
   PE_Pcrel    : constant := 16#10#;

   --  Call frame instructions (DWARF 5, 6.4.2).
   CFA_Advance_Loc        : constant := 16#40#;
   CFA_Offset             : constant := 16#80#;
   CFA_Restore            : constant := 16#C0#;
   CFA_High_Mask          : constant := 16#C0#;
   CFA_Low_Mask           : constant := 16#3F#;
   CFA_Nop                : constant := 16#00#;
   CFA_Set_Loc            : constant := 16#01#;
   CFA_Advance_Loc1       : constant := 16#02#;
   CFA_Advance_Loc2       : constant := 16#03#;
   CFA_Advance_Loc4       : constant := 16#04#;
   CFA_Offset_Extended    : constant := 16#05#;
   CFA_Restore_Extended   : constant := 16#06#;
   CFA_Undefined          : constant := 16#07#;
   CFA_Same_Value         : constant := 16#08#;
   CFA_Register           : constant := 16#09#;
   CFA_Remember_State     : constant := 16#0A#;
   CFA_Restore_State      : constant := 16#0B#;
   CFA_Def_Cfa            : constant := 16#0C#;
   CFA_Def_Cfa_Register   : constant := 16#0D#;
   CFA_Def_Cfa_Offset     : constant := 16#0E#;
   CFA_Def_Cfa_Expression : constant := 16#0F#;
   CFA_Expression         : constant := 16#10#;
   CFA_Offset_Extended_SF : constant := 16#11#;
   CFA_Def_Cfa_SF         : constant := 16#12#;
   CFA_Def_Cfa_Offset_SF  : constant := 16#13#;
   CFA_Val_Offset         : constant := 16#14#;
   CFA_Val_Offset_SF      : constant := 16#15#;
   CFA_Val_Expression     : constant := 16#16#;
   CFA_GNU_Args_Size      : constant := 16#2E#;
   CFA_GNU_Negative_Offset_Extended : constant := 16#2F#;

   CIE_Id_In_Eh_Frame : constant := 0;
   DWARF_64_Escape : constant := 16#FFFF_FFFF#;
   Remember_Ceiling : constant := 8;
   Instruction_Ceiling : constant := 4096;
   Augmentation_Ceiling : constant := 8;

   function Register_Name (R : Register) return String is
     (case R is
         when 0 => "rax", when 1 => "rdx", when 2 => "rcx", when 3 => "rbx",
         when 4 => "rsi", when 5 => "rdi", when 6 => "rbp", when 7 => "rsp",
         when 8 => "r8", when 9 => "r9", when 10 => "r10", when 11 => "r11",
         when 12 => "r12", when 13 => "r13", when 14 => "r14", when 15 => "r15",
         when 16 => "rip");

   --  An encoded pointer at C (Field_At: its offset, for pcrel).
   procedure Read_Pointer
     (T : Table; C : in out B.Cursor; Encoding : Unsigned_64; V : out Unsigned_64)
   is
      Data : B.Byte_Buffer renames T.Data.all;
      Field_At : constant B.Offset := C.At_Byte;
      S : Integer_64;
   begin
      V := 0;
      if Encoding = PE_Omit then
         return;
      end if;
      case Encoding and PE_Format is
         when PE_Absptr | PE_Udata8 => B.U64 (Data, C, V);
         when PE_Uleb128 => B.ULEB (Data, C, V);
         when PE_Udata2 => B.U16 (Data, C, V);
         when PE_Udata4 => B.U32 (Data, C, V);
         when PE_Sleb128 => B.SLEB (Data, C, S); V := B.To_Unsigned (S);
         when PE_Sdata2 => B.U16 (Data, C, V); V := B.To_Unsigned (B.Sign_Extend (V, 16));
         when PE_Sdata4 => B.U32 (Data, C, V); V := B.To_Unsigned (B.Sign_Extend (V, 32));
         when PE_Sdata8 => B.U64 (Data, C, V);
         when others => C.Ok := False;
      end case;
      if (Encoding and PE_Apply) = PE_Pcrel then
         V := V + T.Address + Field_At;
      elsif (Encoding and PE_Apply) /= 0 or else (Encoding and PE_Indirect) /= 0 then
         C.Ok := False;   --  textrel, datarel, funcrel, indirect: not on x86-64 static
      end if;
   end Read_Pointer;

   type CIE_Info is record
      Code_Align : Unsigned_64 := 1;
      Data_Align : Integer_64 := 1;
      RA_Column  : Unsigned_64 := Unsigned_64 (Return_Address);
      FDE_Encoding : Unsigned_64 := PE_Absptr;
      Has_Augmentation_Data : Boolean := False;
      Instructions_At, Instructions_End : B.Offset := 0;
      Ok : Boolean := False;
   end record;

   procedure Read_CIE (T : Table; At_Byte : B.Offset; Info : out CIE_Info) is
      Data : B.Byte_Buffer renames T.Data.all;
      C : B.Cursor := B.Make (At_Byte, B.Offset (Data'Length));
      Length, Id, Version, V, Aug_Length : Unsigned_64;
      Aug : String (1 .. Augmentation_Ceiling) := [others => ' '];
      Aug_Count : Natural := 0;
      Ch : Unsigned_64;
      Aug_End : B.Offset;
   begin
      Info := (others => <>);
      B.U32 (Data, C, Length);
      if not C.Ok or else Length = DWARF_64_Escape or else Length > B.Remaining (C) then
         return;
      end if;
      Info.Instructions_End := C.At_Byte + Length;
      B.U32 (Data, C, Id);
      B.U8 (Data, C, Version);
      if Id /= CIE_Id_In_Eh_Frame then
         return;
      end if;
      loop
         B.U8 (Data, C, Ch);
         exit when not C.Ok or else Ch = 0;
         if Aug_Count = Augmentation_Ceiling then
            return;
         end if;
         Aug_Count := Aug_Count + 1;
         Aug (Aug_Count) := Character'Val (Ch);
      end loop;
      B.ULEB (Data, C, Info.Code_Align);
      B.SLEB (Data, C, Info.Data_Align);
      if Version = 1 then
         B.U8 (Data, C, Info.RA_Column);
      else
         B.ULEB (Data, C, Info.RA_Column);
      end if;
      if Aug_Count > 0 and then Aug (1) = 'z' then
         Info.Has_Augmentation_Data := True;
         B.ULEB (Data, C, Aug_Length);
         Aug_End := C.At_Byte + Aug_Length;
         for I in 2 .. Aug_Count loop
            case Aug (I) is
               when 'R' => B.U8 (Data, C, Info.FDE_Encoding);
               when 'L' => B.U8 (Data, C, V);
               when 'P' =>
                  B.U8 (Data, C, V);
                  Read_Pointer (T, C, V and not PE_Indirect, V);
               when others => null;   --  'S' and unknown letters: data skipped below
            end case;
         end loop;
         C.At_Byte := Aug_End;
      elsif Aug_Count > 0 then
         return;   --  an augmentation without 'z': its layout is unknown
      end if;
      Info.Instructions_At := C.At_Byte;
      Info.Ok := C.Ok and then Info.Instructions_At <= Info.Instructions_End
        and then Info.RA_Column < Register_Columns;
   end Read_CIE;

   procedure Build (Data : B.Buffer_Access; Address : Unsigned_64; T : out Table_Access) is
      Size : constant B.Offset := B.Offset (Data'Length);
      C : B.Cursor;
      Length, Id : Unsigned_64;
      Entry_At, Id_At : B.Offset := 0;
      Info : CIE_Info;
      Low, Range_V : Unsigned_64;
   begin
      T := new Table'(Data => Data, Address => Address,
                      FDEs => new FDE_Array (0 .. Natural (B.Offset'Min (Size / 16 + 1, FDE_Ceiling)) - 1),
                      Count => 0);
      while Entry_At + 8 <= Size loop
         C := B.Make (Entry_At, Size);
         B.U32 (Data.all, C, Length);
         exit when not C.Ok or else Length = 0 or else Length = DWARF_64_Escape
           or else Length > B.Remaining (C);
         Id_At := C.At_Byte;
         B.U32 (Data.all, C, Id);
         if C.Ok and then Id /= CIE_Id_In_Eh_Frame and then Id <= Id_At then
            --  An FDE: Id is the distance back to its CIE.
            Read_CIE (T.all, Id_At - Id, Info);
            if Info.Ok then
               Read_Pointer (T.all, C, Info.FDE_Encoding, Low);
               Read_Pointer (T.all, C, Info.FDE_Encoding and PE_Format, Range_V);
               if C.Ok and then Range_V > 0 and then T.Count < T.FDEs'Length then
                  T.FDEs (T.Count) := (Low => Low, High => Low + Range_V,
                                       FDE_At => Entry_At, CIE_At => Id_At - Id);
                  T.Count := T.Count + 1;
               end if;
            end if;
         end if;
         Entry_At := Id_At + Length;
      end loop;
      --  Sort by Low (insertion: FDEs come nearly sorted).
      for I in 1 .. T.Count - 1 loop
         declare
            Item : constant FDE_Entry := T.FDEs (I);
            J : Integer := I - 1;
         begin
            while J >= 0 and then T.FDEs (J).Low > Item.Low loop
               T.FDEs (J + 1) := T.FDEs (J);
               J := J - 1;
            end loop;
            T.FDEs (J + 1) := Item;
         end;
      end loop;
   end Build;

   procedure Free_Data is new Ada.Unchecked_Deallocation (B.Byte_Buffer, B.Buffer_Access);
   procedure Free_FDEs is new Ada.Unchecked_Deallocation (FDE_Array, FDE_Array_Access);
   procedure Free_Table is new Ada.Unchecked_Deallocation (Table, Table_Access);
   procedure Free (T : in out Table_Access) is
   begin
      if T /= null then
         Free_Data (T.Data);
         Free_FDEs (T.FDEs);
         Free_Table (T);
      end if;
   end Free;

   function Find_FDE (T : Table; PC : Unsigned_64) return Integer is
      Low : Integer := -1;
      High : Integer := T.Count;
      Mid : Integer;
   begin
      while High - Low > 1 loop
         Mid := (Low + High) / 2;
         if T.FDEs (Mid).Low <= PC then
            Low := Mid;
         else
            High := Mid;
         end if;
      end loop;
      if Low >= 0 and then PC < T.FDEs (Low).High then
         return Low;
      end if;
      return -1;
   end Find_FDE;

   function Covers (T : Table; PC : Unsigned_64) return Boolean is (Find_FDE (T, PC) >= 0);

   type Rule_Kind is (Undefined, Same_Value, Saved_At, Value_At, In_Register, Unsupported);
   type Rule is record
      Kind : Rule_Kind := Undefined;
      Offset : Integer_64 := 0;
      Other : Register := 0;
   end record;
   type Rule_Array is array (Register) of Rule;
   type Row_State is record
      Rules : Rule_Array;
      CFA_Register : Register := RSP;
      CFA_Offset : Integer_64 := 0;
      CFA_Ok : Boolean := False;
   end record;

   function Default_Row return Row_State is
      R : Row_State;
   begin
      for Reg in Register loop
         R.Rules (Reg) :=
           (Kind => (if Reg in RBX | RBP | R12 | R13 | R14 | R15 then Same_Value else Undefined),
            others => <>);
      end loop;
      return R;
   end Default_Row;

   --  Run instructions [From, Until) while Loc <= Target.
   procedure Execute
     (T : Table; Info : CIE_Info; From, Until_At : B.Offset; Start_Loc, Target : Unsigned_64;
      Initial : Row_State; State : in out Row_State; Ok : in out Boolean)
   is
      Data : B.Byte_Buffer renames T.Data.all;
      C : B.Cursor := B.Make (From, Until_At);
      Loc : Unsigned_64 := Start_Loc;
      Op, Low, Reg_V, Off_V, Delta_V : Unsigned_64;
      S : Integer_64;
      Saved : array (1 .. Remember_Ceiling) of Row_State;
      Saved_Count : Natural := 0;

      function Reg_Of (V : Unsigned_64) return Register is
        (if V < Register_Columns then Register (V) else Return_Address);
      procedure Advance (By : Unsigned_64; Stop : out Boolean) is
      begin
         Loc := Loc + By * Info.Code_Align;
         Stop := Loc > Target;
      end Advance;
      Stop : Boolean := False;
   begin
      for Count in 1 .. Instruction_Ceiling loop
         exit when not C.Ok or else C.At_Byte >= Until_At;
         B.U8 (Data, C, Op);
         Low := Op and CFA_Low_Mask;
         case Op and CFA_High_Mask is
            when CFA_Advance_Loc =>
               Advance (Low, Stop);
            when CFA_Offset =>
               B.ULEB (Data, C, Off_V);
               if Low < Register_Columns then
                  State.Rules (Register (Low)) :=
                    (Kind => Saved_At, Offset => B.To_Signed (Off_V) * Info.Data_Align, others => <>);
               end if;
            when CFA_Restore =>
               if Low < Register_Columns then
                  State.Rules (Register (Low)) := Initial.Rules (Register (Low));
               end if;
            when others =>
               case Op is
                  when CFA_Nop => null;
                  when CFA_Set_Loc =>
                     Read_Pointer (T, C, Info.FDE_Encoding, Loc);
                     Stop := Loc > Target;
                  when CFA_Advance_Loc1 => B.U8 (Data, C, Delta_V); Advance (Delta_V, Stop);
                  when CFA_Advance_Loc2 => B.U16 (Data, C, Delta_V); Advance (Delta_V, Stop);
                  when CFA_Advance_Loc4 => B.U32 (Data, C, Delta_V); Advance (Delta_V, Stop);
                  when CFA_Offset_Extended =>
                     B.ULEB (Data, C, Reg_V);
                     B.ULEB (Data, C, Off_V);
                     State.Rules (Reg_Of (Reg_V)) :=
                       (Kind => Saved_At, Offset => B.To_Signed (Off_V) * Info.Data_Align, others => <>);
                  when CFA_Offset_Extended_SF | CFA_GNU_Negative_Offset_Extended =>
                     B.ULEB (Data, C, Reg_V);
                     if Op = CFA_Offset_Extended_SF then
                        B.SLEB (Data, C, S);
                        S := S * Info.Data_Align;
                     else
                        B.ULEB (Data, C, Off_V);
                        S := -B.To_Signed (Off_V) * Info.Data_Align;
                     end if;
                     State.Rules (Reg_Of (Reg_V)) := (Kind => Saved_At, Offset => S, others => <>);
                  when CFA_Restore_Extended =>
                     B.ULEB (Data, C, Reg_V);
                     State.Rules (Reg_Of (Reg_V)) := Initial.Rules (Reg_Of (Reg_V));
                  when CFA_Undefined =>
                     B.ULEB (Data, C, Reg_V);
                     State.Rules (Reg_Of (Reg_V)) := (Kind => Undefined, others => <>);
                  when CFA_Same_Value =>
                     B.ULEB (Data, C, Reg_V);
                     State.Rules (Reg_Of (Reg_V)) := (Kind => Same_Value, others => <>);
                  when CFA_Register =>
                     B.ULEB (Data, C, Reg_V);
                     B.ULEB (Data, C, Off_V);
                     State.Rules (Reg_Of (Reg_V)) := (Kind => In_Register, Other => Reg_Of (Off_V), others => <>);
                  when CFA_Remember_State =>
                     if Saved_Count = Remember_Ceiling then
                        Ok := False;
                        return;
                     end if;
                     Saved_Count := Saved_Count + 1;
                     Saved (Saved_Count) := State;
                  when CFA_Restore_State =>
                     if Saved_Count = 0 then
                        Ok := False;
                        return;
                     end if;
                     --  The CFA rule is not part of the remembered rows' restore
                     --  in GCC's use, but DWARF says the whole row: restore it all.
                     State := Saved (Saved_Count);
                     Saved_Count := Saved_Count - 1;
                  when CFA_Def_Cfa =>
                     B.ULEB (Data, C, Reg_V);
                     B.ULEB (Data, C, Off_V);
                     State.CFA_Register := Reg_Of (Reg_V);
                     State.CFA_Offset := B.To_Signed (Off_V);
                     State.CFA_Ok := True;
                  when CFA_Def_Cfa_SF =>
                     B.ULEB (Data, C, Reg_V);
                     B.SLEB (Data, C, S);
                     State.CFA_Register := Reg_Of (Reg_V);
                     State.CFA_Offset := S * Info.Data_Align;
                     State.CFA_Ok := True;
                  when CFA_Def_Cfa_Register =>
                     B.ULEB (Data, C, Reg_V);
                     State.CFA_Register := Reg_Of (Reg_V);
                  when CFA_Def_Cfa_Offset =>
                     B.ULEB (Data, C, Off_V);
                     State.CFA_Offset := B.To_Signed (Off_V);
                  when CFA_Def_Cfa_Offset_SF =>
                     B.SLEB (Data, C, S);
                     State.CFA_Offset := S * Info.Data_Align;
                  when CFA_Def_Cfa_Expression =>
                     B.ULEB (Data, C, Off_V);
                     B.Skip (C, Off_V);
                     State.CFA_Ok := False;   --  not evaluated in the preview
                  when CFA_Expression | CFA_Val_Expression =>
                     B.ULEB (Data, C, Reg_V);
                     B.ULEB (Data, C, Off_V);
                     B.Skip (C, Off_V);
                     State.Rules (Reg_Of (Reg_V)) := (Kind => Unsupported, others => <>);
                  when CFA_Val_Offset =>
                     B.ULEB (Data, C, Reg_V);
                     B.ULEB (Data, C, Off_V);
                     State.Rules (Reg_Of (Reg_V)) :=
                       (Kind => Value_At, Offset => B.To_Signed (Off_V) * Info.Data_Align, others => <>);
                  when CFA_Val_Offset_SF =>
                     B.ULEB (Data, C, Reg_V);
                     B.SLEB (Data, C, S);
                     State.Rules (Reg_Of (Reg_V)) :=
                       (Kind => Value_At, Offset => S * Info.Data_Align, others => <>);
                  when CFA_GNU_Args_Size =>
                     B.ULEB (Data, C, Off_V);
                  when others =>
                     Ok := False;
                     return;
               end case;
         end case;
         exit when Stop;
      end loop;
      Ok := Ok and then C.Ok;
   end Execute;

   procedure Step
     (T : Table; Callee : Register_Set; Innermost : Boolean;
      Read : not null access procedure (Address : Unsigned_64; Value : out Unsigned_64; Ok : out Boolean);
      Caller : out Register_Set; CFA : out Unsigned_64; Ok : out Boolean)
   is
      PC : constant Unsigned_64 := Callee.Value (Return_Address);
      Lookup : constant Unsigned_64 := (if Innermost or else PC = 0 then PC else PC - 1);
      Index : Integer;
      Info : CIE_Info;
      Initial, State : Row_State;
      Data : B.Byte_Buffer renames T.Data.all;
      C : B.Cursor;
      Length, V, Aug_Length : Unsigned_64;
      Start_Loc : Unsigned_64;
      Instr_At, Instr_End : B.Offset;
      Word : Unsigned_64;
      Read_Ok : Boolean;
   begin
      Caller := (others => <>);
      CFA := 0;
      Ok := False;
      if not Callee.Known (Return_Address) then
         return;
      end if;
      Index := Find_FDE (T, Lookup);
      if Index < 0 then
         return;
      end if;
      Read_CIE (T, T.FDEs (Index).CIE_At, Info);
      if not Info.Ok then
         return;
      end if;
      --  The FDE's instructions follow its header.
      C := B.Make (T.FDEs (Index).FDE_At, B.Offset (Data'Length));
      B.U32 (Data, C, Length);
      Instr_End := C.At_Byte + Length;
      B.U32 (Data, C, V);
      Read_Pointer (T, C, Info.FDE_Encoding, Start_Loc);
      Read_Pointer (T, C, Info.FDE_Encoding and PE_Format, V);
      if Info.Has_Augmentation_Data then
         B.ULEB (Data, C, Aug_Length);
         B.Skip (C, Aug_Length);
      end if;
      if not C.Ok then
         return;
      end if;
      Instr_At := C.At_Byte;
      Ok := True;
      Initial := Default_Row;
      Execute (T, Info, Info.Instructions_At, Info.Instructions_End, Start_Loc,
               Unsigned_64'Last, Default_Row, Initial, Ok);
      State := Initial;
      Execute (T, Info, Instr_At, Instr_End, Start_Loc, Lookup, Initial, State, Ok);
      if not Ok or else not State.CFA_Ok or else not Callee.Known (State.CFA_Register) then
         Ok := False;
         return;
      end if;
      CFA := B.To_Unsigned (B.To_Signed (Callee.Value (State.CFA_Register)) + State.CFA_Offset);
      for Reg in Register loop
         declare
            R : constant Rule := State.Rules (Reg);
         begin
            case R.Kind is
               when Undefined | Unsupported =>
                  null;
               when Same_Value =>
                  Caller.Value (Reg) := Callee.Value (Reg);
                  Caller.Known (Reg) := Callee.Known (Reg);
               when Saved_At =>
                  Read (B.To_Unsigned (B.To_Signed (CFA) + R.Offset), Word, Read_Ok);
                  Caller.Value (Reg) := Word;
                  Caller.Known (Reg) := Read_Ok;
               when Value_At =>
                  Caller.Value (Reg) := B.To_Unsigned (B.To_Signed (CFA) + R.Offset);
                  Caller.Known (Reg) := True;
               when In_Register =>
                  Caller.Value (Reg) := Callee.Value (R.Other);
                  Caller.Known (Reg) := Callee.Known (R.Other);
            end case;
         end;
      end loop;
      --  The return address column gives the caller's PC; the CFA is its SP.
      if Natural (Info.RA_Column) /= Natural (Return_Address) then
         Caller.Value (Return_Address) := Caller.Value (Register (Info.RA_Column));
         Caller.Known (Return_Address) := Caller.Known (Register (Info.RA_Column));
      end if;
      Caller.Value (RSP) := CFA;
      Caller.Known (RSP) := True;
      Ok := Caller.Known (Return_Address) and then Caller.Value (Return_Address) /= 0;
   end Step;
end CuBit.Debug.Cfi;
