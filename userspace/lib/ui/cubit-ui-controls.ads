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

   MAX_CONTROLS : constant Natural := 48;
   subtype Control_Index is Natural range 1 .. MAX_CONTROLS;

   type Control_Entry is record
      id      : Control_ID := NO_CONTROL;
      bounds  : CuBit.UI.Rect := (others => 0);
      damage  : CuBit.UI.Rect := (others => 0);
      enabled : Boolean := False;
   end record;

   type Control_Entries is array (Control_Index) of Control_Entry;

   type Control_Map is record
      entries : Control_Entries := (others => (others => <>));
   end record;

   procedure Clear (m : in out Control_Map);

   procedure Add
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect);

   function Hit
      (m : Control_Map; x, y : Natural) return Control_ID;

   function Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect;

   procedure Mark_Dirty
      (dirty : in out CuBit.UI.Rect; m : Control_Map; id : Control_ID);
end CuBit.UI.Controls;
