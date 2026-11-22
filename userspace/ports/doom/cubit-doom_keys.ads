------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  DOOM's keyboard input: desktop scan codes to doomgeneric key codes, and
--  the bounded queue DG_GetKey drains (docs/c-removal.md).
--
--  Key codes are doomgeneric's (doomkeys.h); tests/doom-port checks them
--  against that header.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Doom_Keys with SPARK_Mode, Pure is

   --  A PS/2 set 1 make code, as desktop key events carry it.
   type Scancode is range 0 .. 127;

   --  A doomgeneric key code; zero is no key.
   type Key is new Unsigned_8;
   No_Key : constant Key := 0;

   Key_Right_Arrow : constant Key := 16#AE#;
   Key_Left_Arrow  : constant Key := 16#AC#;
   Key_Up_Arrow    : constant Key := 16#AD#;
   Key_Down_Arrow  : constant Key := 16#AF#;
   Key_Use         : constant Key := 16#A2#;
   Key_Fire        : constant Key := 16#A3#;
   Key_Escape      : constant Key := 27;
   Key_Enter       : constant Key := 13;
   Key_Tab         : constant Key := 9;
   Key_Backspace   : constant Key := 16#7F#;
   Key_Equals      : constant Key := 16#3D#;
   Key_Minus       : constant Key := 16#2D#;
   --  doomkeys.h: keys without an ASCII code are 0x80 + their scan code.
   Scan_Key_Base   : constant Key := 16#80#;
   Key_F1          : constant Key := Scan_Key_Base + 16#3B#;
   Key_F2          : constant Key := Scan_Key_Base + 16#3C#;
   Key_F3          : constant Key := Scan_Key_Base + 16#3D#;
   Key_F4          : constant Key := Scan_Key_Base + 16#3E#;
   Key_F5          : constant Key := Scan_Key_Base + 16#3F#;
   Key_F6          : constant Key := Scan_Key_Base + 16#40#;
   Key_F7          : constant Key := Scan_Key_Base + 16#41#;
   Key_F8          : constant Key := Scan_Key_Base + 16#42#;
   Key_F9          : constant Key := Scan_Key_Base + 16#43#;
   Key_F10         : constant Key := Scan_Key_Base + 16#44#;
   Key_F11         : constant Key := Scan_Key_Base + 16#57#;
   Key_F12         : constant Key := Scan_Key_Base + 16#58#;
   Key_Right_Shift : constant Key := Scan_Key_Base + 16#36#;
   Key_Right_Alt   : constant Key := Scan_Key_Base + 16#38#;
   Key_Caps_Lock   : constant Key := Scan_Key_Base + 16#3A#;
   Key_Num_Lock    : constant Key := Scan_Key_Base + 16#45#;
   Key_Scroll_Lock : constant Key := Scan_Key_Base + 16#46#;
   Key_Home        : constant Key := Scan_Key_Base + 16#47#;
   Key_End         : constant Key := Scan_Key_Base + 16#4F#;
   Key_Page_Up     : constant Key := Scan_Key_Base + 16#49#;
   Key_Page_Down   : constant Key := Scan_Key_Base + 16#51#;
   Key_Insert      : constant Key := Scan_Key_Base + 16#52#;
   Key_Delete      : constant Key := Scan_Key_Base + 16#53#;

   --  The key DOOM sees for a scan code; No_Key for keys it ignores.
   function Translate (Code : Scancode) return Key;

   type Event is record
      Code    : Key := No_Key;
      Pressed : Boolean := False;
   end record;

   Queue_Capacity : constant := 32;
   subtype Queue_Count is Natural range 0 .. Queue_Capacity;

   type Queue is private;
   Empty_Queue : constant Queue;

   function Length (Q : Queue) return Queue_Count;

   --  Events whose key DOOM ignores are dropped, as are events arriving
   --  while the queue is full (DOOM polls far faster than anyone types).
   procedure Push (Q : in out Queue; Item : Event)
     with Post => Length (Q) =
       (if Item.Code = No_Key or else Length (Q'Old) = Queue_Capacity
        then Length (Q'Old) else Length (Q'Old) + 1);

   --  The oldest event, or Found = False when empty.
   procedure Pop (Q : in out Queue; Item : out Event; Found : out Boolean)
     with Post => Found = (Length (Q'Old) > 0) and then
       Length (Q) = (if Found then Length (Q'Old) - 1 else 0);

private

   type Slot is mod Queue_Capacity;
   type Event_Array is array (Slot) of Event;

   type Queue is record
      Items : Event_Array := [others => (Code => No_Key, Pressed => False)];
      First : Slot := 0;
      Count : Queue_Count := 0;
   end record;

   Empty_Queue : constant Queue :=
     (Items => [others => (Code => No_Key, Pressed => False)],
      First => 0, Count => 0);

   function Length (Q : Queue) return Queue_Count is (Q.Count);

end CuBit.Doom_Keys;
