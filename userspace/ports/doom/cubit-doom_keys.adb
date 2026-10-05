pragma Ada_2022;

package body CuBit.Doom_Keys with SPARK_Mode is

   type Key_Table is array (Scancode) of Key;

   --  Keypad keys share the scan codes of the navigation keys they double
   --  as; DOOM gets the navigation key. Left Ctrl fires, Left Alt strafes,
   --  Space uses.
   Keys : constant Key_Table :=
     [16#01# => Key_Escape,
      16#02# => Character'Pos ('1'), 16#03# => Character'Pos ('2'),
      16#04# => Character'Pos ('3'), 16#05# => Character'Pos ('4'),
      16#06# => Character'Pos ('5'), 16#07# => Character'Pos ('6'),
      16#08# => Character'Pos ('7'), 16#09# => Character'Pos ('8'),
      16#0A# => Character'Pos ('9'), 16#0B# => Character'Pos ('0'),
      16#0C# => Key_Minus, 16#0D# => Key_Equals,
      16#0E# => Key_Backspace, 16#0F# => Key_Tab,
      16#10# => Character'Pos ('q'), 16#11# => Character'Pos ('w'),
      16#12# => Character'Pos ('e'), 16#13# => Character'Pos ('r'),
      16#14# => Character'Pos ('t'), 16#15# => Character'Pos ('y'),
      16#16# => Character'Pos ('u'), 16#17# => Character'Pos ('i'),
      16#18# => Character'Pos ('o'), 16#19# => Character'Pos ('p'),
      16#1A# => Character'Pos ('['), 16#1B# => Character'Pos (']'),
      16#1C# => Key_Enter, 16#1D# => Key_Fire,
      16#1E# => Character'Pos ('a'), 16#1F# => Character'Pos ('s'),
      16#20# => Character'Pos ('d'), 16#21# => Character'Pos ('f'),
      16#22# => Character'Pos ('g'), 16#23# => Character'Pos ('h'),
      16#24# => Character'Pos ('j'), 16#25# => Character'Pos ('k'),
      16#26# => Character'Pos ('l'), 16#27# => Character'Pos (';'),
      16#28# => Character'Pos ('''), 16#29# => Character'Pos ('`'),
      16#2A# => Key_Right_Shift, 16#2B# => Character'Pos ('\'),
      16#2C# => Character'Pos ('z'), 16#2D# => Character'Pos ('x'),
      16#2E# => Character'Pos ('c'), 16#2F# => Character'Pos ('v'),
      16#30# => Character'Pos ('b'), 16#31# => Character'Pos ('n'),
      16#32# => Character'Pos ('m'), 16#33# => Character'Pos (','),
      16#34# => Character'Pos ('.'), 16#35# => Character'Pos ('/'),
      16#36# => Key_Right_Shift, 16#37# => Character'Pos ('*'),
      16#38# => Key_Right_Alt, 16#39# => Key_Use,
      16#3A# => Key_Caps_Lock,
      16#3B# => Key_F1, 16#3C# => Key_F2, 16#3D# => Key_F3,
      16#3E# => Key_F4, 16#3F# => Key_F5, 16#40# => Key_F6,
      16#41# => Key_F7, 16#42# => Key_F8, 16#43# => Key_F9,
      16#44# => Key_F10,
      16#45# => Key_Num_Lock, 16#46# => Key_Scroll_Lock,
      16#47# => Key_Home, 16#48# => Key_Up_Arrow, 16#49# => Key_Page_Up,
      16#4A# => Key_Minus, 16#4B# => Key_Left_Arrow,
      16#4C# => Character'Pos ('5'), 16#4D# => Key_Right_Arrow,
      16#4E# => Key_Equals, 16#4F# => Key_End, 16#50# => Key_Down_Arrow,
      16#51# => Key_Page_Down, 16#52# => Key_Insert, 16#53# => Key_Delete,
      16#57# => Key_F11, 16#58# => Key_F12,
      others => No_Key];

   function Translate (Code : Scancode) return Key is (Keys (Code));

   procedure Push (Q : in out Queue; Item : Event) is
   begin
      if Item.Code /= No_Key and then Q.Count < Queue_Capacity then
         Q.Items (Q.First + Slot (Q.Count)) := Item;
         Q.Count := Q.Count + 1;
      end if;
   end Push;

   procedure Pop (Q : in out Queue; Item : out Event; Found : out Boolean) is
   begin
      if Q.Count = 0 then
         Item := (Code => No_Key, Pressed => False);
         Found := False;
      else
         Item := Q.Items (Q.First);
         Q.First := Q.First + 1;
         Q.Count := Q.Count - 1;
         Found := True;
      end if;
   end Pop;

end CuBit.Doom_Keys;
