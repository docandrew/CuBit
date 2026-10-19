------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's tooltips: its icon-only targets (window buttons, the
--  Deskbar's status icons), their encoding as tooltip-policy targets, and
--  what each tip says.
--
--  @description
--  When a tip shows is Client_Tooltip_Policy's, where it goes
--  Client_Popup_Layout's (both proved, tests/ui-popups); this is the
--  Desktop's half. A target encodes to a policy target and decodes back to
--  itself (proved, tests/deskbar), so a tip never names another window's
--  button or another status item.
------------------------------------------------------------------------------
with Client_Tooltip_Policy;
with Desktop_Status_Items;
with Desktop_Surface_Limits;

package Desktop_Tooltips with SPARK_Mode, Pure is
   package TP renames Client_Tooltip_Policy;
   package DSI renames Desktop_Status_Items;

   --  Window slots (Desktop's surface table) a target can name: all of them.
   MAXIMUM_WINDOWS : constant := Desktop_Surface_Limits.Maximum_Surfaces;
   subtype Window_Slot is Natural range 0 .. MAXIMUM_WINDOWS - 1;
   type Window_Button is (Minimize_Button, Maximize_Button, Close_Button);

   type Target_Kind is (Nothing, Button_Of_Window, Status_Icon, Unread_Count, Status_Button);
   type Target (Kind : Target_Kind := Nothing) is record
      case Kind is
         when Button_Of_Window =>
            Slot : Window_Slot := 0;
            Button : Window_Button := Minimize_Button;
         when Status_Icon =>
            Item : DSI.Item := DSI.Item'First;
         when Nothing | Unread_Count | Status_Button =>
            null;
      end case;
   end record;
   NO_TARGET : constant Target := (Kind => Nothing);

   --  Policy targets: from WINDOW_BASE the window buttons (slot by slot),
   --  then the status items, the unread count and the status button.
   BUTTONS : constant := Window_Button'Pos (Window_Button'Last) + 1;
   ITEMS : constant := DSI.Item'Pos (DSI.Item'Last) + 1;
   WINDOW_BASE : constant := 1;
   STATUS_BASE : constant := WINDOW_BASE + MAXIMUM_WINDOWS * BUTTONS;
   UNREAD_TARGET : constant := STATUS_BASE + ITEMS;
   BUTTON_TARGET : constant := UNREAD_TARGET + 1;

   function Decode (Value : TP.Target) return Target is
     (if Value in WINDOW_BASE .. STATUS_BASE - 1 then
        (Kind => Button_Of_Window, Slot => (Value - WINDOW_BASE) / BUTTONS,
         Button => Window_Button'Val ((Value - WINDOW_BASE) mod BUTTONS))
      elsif Value in STATUS_BASE .. UNREAD_TARGET - 1 then
        (Kind => Status_Icon, Item => DSI.Item'Val (Value - STATUS_BASE))
      elsif Value = UNREAD_TARGET then (Kind => Unread_Count)
      elsif Value = BUTTON_TARGET then (Kind => Status_Button)
      else NO_TARGET);

   function Encode (Of_Target : Target) return TP.Target is
     (case Of_Target.Kind is
         when Nothing => TP.NO_TARGET,
         when Button_Of_Window =>
           WINDOW_BASE + Of_Target.Slot * BUTTONS + Window_Button'Pos (Of_Target.Button),
         when Status_Icon => STATUS_BASE + DSI.Item'Pos (Of_Target.Item),
         when Unread_Count => UNREAD_TARGET,
         when Status_Button => BUTTON_TARGET)
     with Post => Decode (Encode'Result) = Of_Target
                  and then (Encode'Result = TP.NO_TARGET) = (Of_Target.Kind = Nothing);

   --  A window button's tip, by the window's state; a greyed maximize says
   --  why nothing happens.
   function Button_Text (Button : Window_Button; Maximized, Maximize_Available : Boolean) return String is
     (case Button is
         when Minimize_Button => "Minimize",
         when Maximize_Button =>
           (if not Maximize_Available then "Maximize unavailable" elsif Maximized then "Restore" else "Maximize"),
         when Close_Button => "Close");

   --  The box, in logical pixels: text inset by PADDING, a shortcut
   --  HINT_GAP after it; POINTER_GAP below and right of the pointer, or
   --  FOCUS_GAP from the focused control.
   PADDING : constant := 6;
   HINT_GAP : constant := 12;
   POINTER_GAP : constant := 18;
   FOCUS_GAP : constant := 4;
end Desktop_Tooltips;
