------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Editing a dashboard (MET-002's app): move, resize, remove and duplicate
--  panels, with undo and redo. Every edit leaves a Dashboard the CCL type
--  accepts (sizes stay in the type's ranges), so what is saved, copied or
--  handed to an agent is always a valid Dashboard value.
--
--  @description
--  The history keeps each layout before an edit (the panels' order, widths
--  and heights, by panel identity) and every panel ever shown (a removed
--  panel comes back with undo). It grows on the heap up to Maximum_History
--  edits; past it the oldest is let go.
------------------------------------------------------------------------------
pragma Ada_2022;
with Dash_Model;

package Dash_Edit is
   Maximum_History : constant := 512;

   type Operation is (Move_Earlier, Move_Later, Wider, Narrower, Taller, Shorter, Remove, Duplicate);

   --  Begin editing D: its panels are known, the history is empty.
   procedure Start (D : Dash_Model.Dashboard);

   --  Op on panel P of D. Done False when it would leave the type (the
   --  narrowest panel, the last panel removed, no room for a copy) or P is
   --  not a panel. Selected: the panel to select after it.
   procedure Apply
     (D : in out Dash_Model.Dashboard; Op : Operation; P : Positive; Done : out Boolean; Selected : out Natural);

   --  Panel P of D becomes Item (a picked source or view): Item takes a new
   --  identity, so the panel's data is read again and undo brings the
   --  old panel back as it was. Done False when P is not a panel.
   procedure Replace (D : in out Dash_Model.Dashboard; P : Positive; Item : Dash_Model.Panel; Done : out Boolean);

   --  Item (a new panel) at index At_Index of D, with a new identity.
   --  Done False when D holds as many panels as a dashboard can.
   procedure Insert
     (D : in out Dash_Model.Dashboard; Item : Dash_Model.Panel; At_Index : Positive; Done : out Boolean);

   --  Edits between Begin_Group and End_Group (a drag) undo as one.
   procedure Begin_Group;
   procedure End_Group;

   procedure Undo (D : in out Dash_Model.Dashboard; Done : out Boolean);
   procedure Redo (D : in out Dash_Model.Dashboard; Done : out Boolean);
   function Can_Undo return Boolean;
   function Can_Redo return Boolean;
end Dash_Edit;
