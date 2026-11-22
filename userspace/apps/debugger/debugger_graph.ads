------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Debugger (DBG-001): the snapshot's data structures as a toolkit graph
------------------------------------------------------------------------------
--  The toolkit's CuBit.UI.Graphs.Views over Debugger_Model's nodes and
--  fields: records, arrays and access targets as nodes, pointer fields as
--  out-ports, null access values as ground marks. Read-only (Viewing).
with CuBit.UI.Graphs; use CuBit.UI.Graphs;
with CuBit.UI.Graphs.Views;
with Debugger_Model;

package Debugger_Graph is
   package DM renames Debugger_Model;

   --  The time change marks are judged at (set before each draw).
   procedure Set_Now (Now : DM.Microseconds);

   function Node_Total (S : DM.Snapshot) return Node_Count is (S.Node_Total);
   function Key_Of (S : DM.Snapshot; N : Node_Index) return Node_Key is (S.Nodes (N).Id);
   function Title (S : DM.Snapshot; N : Node_Index) return String;
   function Detail (S : DM.Snapshot; N : Node_Index) return String;
   function Style (S : DM.Snapshot; N : Node_Index) return Node_Style;
   function Row_Total (S : DM.Snapshot; N : Node_Index) return Row_Count is (S.Nodes (N).Field_Total);
   function More_Rows (S : DM.Snapshot; N : Node_Index) return Natural is (S.Nodes (N).More_Fields);
   function Row_Name (S : DM.Snapshot; N : Node_Index; R : Row_Index) return String is
     (DM.Image (S.Nodes (N).Fields (R).Name));
   function Row_Value (S : DM.Snapshot; N : Node_Index; R : Row_Index) return String is
     (DM.Without_Package (S, DM.Image (S.Nodes (N).Fields (R).Value)));
   function Port (S : DM.Snapshot; N : Node_Index; R : Row_Index) return Port_State;
   function Row_Marked (S : DM.Snapshot; N : Node_Index; R : Row_Index) return Boolean;
   function Node_Marked (S : DM.Snapshot; N : Node_Index) return Boolean;
   function Is_Pinned (S : DM.Snapshot; N : Node_Index) return Boolean is (False);
   function Edge_Total (S : DM.Snapshot) return Edge_Count is (S.Edge_Total);
   function Edge_At (S : DM.Snapshot; E : Edge_Index) return Edge is
     ((From => S.Edges (E).From, From_Row => S.Edges (E).From_Field, To => S.Edges (E).To, To_Row => 0,
       Back => S.Edges (E).Back));
   --  Read-only: no connection is ever made.
   function Check (S : DM.Snapshot; Proposal : Connection) return Boolean is (False);

   package Views is new CuBit.UI.Graphs.Views
     (Model => DM.Snapshot, Node_Total => Node_Total, Key_Of => Key_Of, Title => Title, Detail => Detail,
      Style => Style, Row_Total => Row_Total, More_Rows => More_Rows, Row_Name => Row_Name,
      Row_Value => Row_Value, Port => Port, Row_Marked => Row_Marked, Node_Marked => Node_Marked,
      Is_Pinned => Is_Pinned, Edge_Total => Edge_Total, Edge_At => Edge_At, Check => Check);
end Debugger_Graph;
