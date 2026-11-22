------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with CuBit.UI.Instrument;

package body Debugger_Locals is
   use type DM.Value_Kind;
   P : CuBit.UI.Instrument.Palette renames CuBit.UI.Instrument.Instrument;
   Marks_At : DM.Microseconds := DM.NEVER;
   NAME_COLUMN : constant Column_Index := 1;
   VALUE_COLUMN : constant Column_Index := 2;

   procedure Set_Now (Now : DM.Microseconds) is
   begin
      Marks_At := Now;
   end Set_Now;

   function Cell (S : DM.Snapshot; I : Item_Index; Column : Column_Index) return String is
     (case Column is
        when NAME_COLUMN => DM.Without_Package (S, DM.Image (S.Locals (I).Name)),
        when VALUE_COLUMN => DM.Without_Package (S, DM.Image (S.Locals (I).Value)),
        when others =>
          (if S.Locals (I).Kind = DM.Discriminant then "discriminant " else "")
          & DM.Without_Package (S, DM.Image (S.Locals (I).Type_Name)));

   function Cell_Tone (S : DM.Snapshot; I : Item_Index; Column : Column_Index) return Tone is
      E : DM.Local_Entry renames S.Locals (I);
   begin
      case Column is
         when NAME_COLUMN => return (if E.Kind = DM.Discriminant then Kind_Tone else Normal);
         when VALUE_COLUMN =>
            return (if E.Out_Of_Range then Danger elsif E.Kind = DM.Pointer then Link else Normal);
         when others => return (if E.Kind = DM.Discriminant then Kind_Tone else Type_Tone);
      end case;
   end Cell_Tone;

   function Square (S : DM.Snapshot; I : Item_Index; Has_Square : out Boolean) return CuBit.UI.Color is
   begin
      Has_Square := True;
      return (case S.Locals (I).Kind is
                when DM.Scalar | DM.Unknown => P.Kind_Scalar,
                when DM.Enum | DM.Rust_Enum | DM.Discriminant => P.Kind_Enum,
                when DM.Record_Value | DM.Variant_Record => P.Kind_Record,
                when DM.Array_Value | DM.Slice => P.Kind_Array,
                when DM.Pointer => P.Kind_Pointer);
   end Square;

   function Marked (S : DM.Snapshot; I : Item_Index) return Boolean is
     (DM.Marked (S.Locals (I).Changed_At, S, Marks_At));
end Debugger_Locals;
