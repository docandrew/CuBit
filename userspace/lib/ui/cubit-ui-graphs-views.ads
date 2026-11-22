------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Graph view over a caller's model: drawing, pan and zoom, level of
--  detail, change marks, clicks on nodes and ports
------------------------------------------------------------------------------
--  The caller keeps its own nodes and edges and answers these functions;
--  the view keeps only placement and viewing state. Damage-only: every
--  input procedure says whether the view changed (the caller repaints its
--  area then). Text is drawn at the zoom's density, so it stays crisp.
--
--  Editing (Mode = Editing) is planned, not built: see CuBit.UI.Graphs.
--  The Check function below is the caller's validation of a proposed
--  connection; a view in Viewing mode never calls it.
with CuBit.UI.Instrument;

generic
   type Model (<>) is limited private;
   with function Node_Total (M : Model) return Node_Count;
   with function Key_Of (M : Model; N : Node_Index) return Node_Key;
   with function Title (M : Model; N : Node_Index) return String;
   --  The header's right side (an address, a rate); "" for none.
   with function Detail (M : Model; N : Node_Index) return String;
   with function Style (M : Model; N : Node_Index) return Node_Style;
   with function Row_Total (M : Model; N : Node_Index) return Row_Count;
   --  Rows past those shown (drawn as one "+N more" row).
   with function More_Rows (M : Model; N : Node_Index) return Natural;
   with function Row_Name (M : Model; N : Node_Index; R : Row_Index) return String;
   with function Row_Value (M : Model; N : Node_Index; R : Row_Index) return String;
   with function Port (M : Model; N : Node_Index; R : Row_Index) return Port_State;
   --  Change marks, by the caller's policy (recent, or since the last stop).
   with function Row_Marked (M : Model; N : Node_Index; R : Row_Index) return Boolean;
   with function Node_Marked (M : Model; N : Node_Index) return Boolean;
   with function Is_Pinned (M : Model; N : Node_Index) return Boolean;
   with function Edge_Total (M : Model) return Edge_Count;
   with function Edge_At (M : Model; E : Edge_Index) return Edge;
   --  Editing (planned): may this connection be made? (a type check)
   with function Check (M : Model; Proposal : Connection) return Boolean;
package CuBit.UI.Graphs.Views is

   type View is record
      Places     : Layout;
      Pan_X, Pan_Y : Integer := 0;
      Zoom       : Zoom_Step := UNIT_ZOOM;
      Selected   : Natural := 0;           --  node index, 0: none
      Fitted     : Boolean := False;
      Dragging   : Boolean := False;
      Drag_Moved : Boolean := False;
      Drag_X, Drag_Y : Natural := 0;
      Drag_Pan_X, Drag_Pan_Y : Integer := 0;
      Editing    : Mode := Viewing;
   end record;

   --  The smallest zoom Fit uses, and the one the wrap width assumes.
   FIT_ZOOM_MIN : constant Zoom_Step := 3;

   --  Place new nodes (Area: the view's rectangle, for the wrap width).
   procedure Update (V : in out View; M : Model; Area : Rect);
   --  Lay every unpinned node out again (on request).
   procedure Relayout (V : in out View; M : Model; Area : Rect);
   --  Zoom (not below FIT_ZOOM_MIN, not above 100%) and pan so the whole
   --  graph shows, if it can.
   procedure Fit (V : in out View; M : Model; Area : Rect);

   procedure Draw
     (C : Canvas; Area : Rect; M : Model; V : View; Colors : CuBit.UI.Instrument.Palette);

   --  Input. Changed: the view must be repainted.
   type Hit_Kind is (Hit_Nothing, Hit_Node, Hit_Port);
   type Hit is record
      Kind : Hit_Kind := Hit_Nothing;
      Node : Natural := 0;
      Row  : Row_Or_Header := 0;
   end record;
   procedure Pointer_Down (V : in out View; X, Y : Natural);
   procedure Pointer_Move (V : in out View; X, Y : Natural; Changed : out Boolean);
   --  A click (not a drag): selects the node under it; Hit_Port when it
   --  landed on an Open_Port row (the caller expands that link).
   procedure Pointer_Up (V : in out View; M : Model; Area : Rect; X, Y : Natural; Result : out Hit;
                         Changed : out Boolean);
   procedure Wheel (V : in out View; Area : Rect; X, Y : Natural; Steps : Integer; Changed : out Boolean);
   --  Arrows pan, + and - zoom about the centre, 0 fits, R lays out again.
   procedure Key (V : in out View; M : Model; Area : Rect; Code : Unsigned_64; Handled : out Boolean);
   --  Editing (planned): whether the caller's Check accepts a proposed
   --  connection; the editing UI will draw a refused drop as refused.
   function Allowed (M : Model; Proposal : Connection) return Boolean is (Check (M, Proposal));
   --  World coordinates of a point in Area.
   function World_X (V : View; Area : Rect; X : Natural) return Integer;
   function World_Y (V : View; Area : Rect; Y : Natural) return Integer;
end CuBit.UI.Graphs.Views;
