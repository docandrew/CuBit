------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Interfaces;
with CuBit.UI.Instrument;

package body Debugger_Graph is
   use type Interfaces.Unsigned_64;
   Marks_At : DM.Microseconds := DM.NEVER;

   procedure Set_Now (Now : DM.Microseconds) is
   begin
      Marks_At := Now;
   end Set_Now;

   function Title (S : DM.Snapshot; N : Node_Index) return String is
     (DM.Without_Package
        (S, (if S.Nodes (N).Label.Length > 0 then DM.Image (S.Nodes (N).Label) & " : " & DM.Image (S.Nodes (N).Title)
             else DM.Image (S.Nodes (N).Title))));

   ADDRESS_DIGITS : constant := 12;
   function Detail (S : DM.Snapshot; N : Node_Index) return String is
     ("@" & CuBit.UI.Instrument.Hex (S.Nodes (N).Address, ADDRESS_DIGITS));

   function Style (S : DM.Snapshot; N : Node_Index) return Node_Style is
     (case S.Nodes (N).Kind is
        when DM.Array_Value | DM.Slice => Array_Style,
        when DM.Pointer => Pointer_Style,
        when DM.Enum | DM.Rust_Enum => Enum_Style,
        when others => Record_Style);

   function Port (S : DM.Snapshot; N : Node_Index; R : Row_Index) return Port_State is
      F : DM.Graph_Field renames S.Nodes (N).Fields (R);
   begin
      if not F.Is_Pointer then
         return No_Port;
      elsif F.Dangling then
         return Dangling_Port;
      elsif F.Pointee = DM.NO_REF then
         return Null_Port;
      elsif F.Target /= DM.NO_NODE then
         return Linked_Port;
      else
         return Open_Port;
      end if;
   end Port;

   function Row_Marked (S : DM.Snapshot; N : Node_Index; R : Row_Index) return Boolean is
     (DM.Marked (S.Nodes (N).Fields (R).Changed_At, S, Marks_At));
   function Node_Marked (S : DM.Snapshot; N : Node_Index) return Boolean is
     (DM.Recently (S.Nodes (N).Appeared_At, Marks_At));
end Debugger_Graph;
