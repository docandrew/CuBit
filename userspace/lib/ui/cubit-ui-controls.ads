------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small control map for hit testing and damage lookup
------------------------------------------------------------------------------
package CuBit.UI.Controls is
   subtype Control_ID is Natural;
   NO_CONTROL : constant Control_ID := 0;

   MAX_CONTROLS : constant Natural := 128;
   subtype Control_Index is Natural range 1 .. MAX_CONTROLS;

   type Control_Entry is record
      id           : Control_ID := NO_CONTROL;
      bounds       : CuBit.UI.Rect := (others => 0);
      --  Hover and pressed-state changes are always local to the control.
      visualDamage : CuBit.UI.Rect := (others => 0);
      --  Actions may change a containing view (selection, scroll, layout).
      actionDamage : CuBit.UI.Rect := (others => 0);
      cursor       : CuBit.UI.Pointer_Cursor_Style :=
        CuBit.UI.Pointer_Default;
      continuousAction : Boolean := False;
      enabled      : Boolean := False;
   end record;

   type Control_Entries is array (Control_Index) of Control_Entry;

   type Control_Map is record
      entries : Control_Entries := (others => (others => <>));
      valid : Boolean := True;
   end record;

   procedure Clear (m : in out Control_Map);

   --  False means registration exhausted its bounded storage or two live
   --  controls claimed the same ID. Hit testing and damage lookup then fail
   --  inertly for the complete map rather than returning ambiguous results.
   function Is_Valid (m : Control_Map) return Boolean;

   procedure Add
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       --  Region whose pixels may change when the control is operated.
       --  Ordinary pointer motion still invalidates only bounds.
       actionDamage : CuBit.UI.Rect;
       cursor : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default;
       continuousAction : Boolean := False);

   function Hit
      (m : Control_Map; x, y : Natural) return Control_ID;

   --  The exact visible interaction rectangle registered for a control.
   --  Widgets use this for state evaluation so clipping and hit testing can
   --  never disagree.
   function Bounds
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect;

   function Visual_Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect;

   function Action_Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect;

   function Cursor
      (m : Control_Map; id : Control_ID)
      return CuBit.UI.Pointer_Cursor_Style;

   function Has_Continuous_Action
      (m : Control_Map; id : Control_ID) return Boolean;

   procedure Mark_Visual_Dirty
      (dirty : in out CuBit.UI.Rect; m : Control_Map; id : Control_ID);

   procedure Mark_Action_Dirty
      (dirty : in out CuBit.UI.Rect; m : Control_Map; id : Control_ID);
end CuBit.UI.Controls;
