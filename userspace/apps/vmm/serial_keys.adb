------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  The bytes a serial terminal sends for a key (serial_keys.ads).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Serial_Keys with SPARK_Mode is

   --  The letters of each row of keys, in scan-code order (US layout).
   Top_Row    : constant String := "QWERTYUIOP";
   Middle_Row : constant String := "ASDFGHJKL";
   Bottom_Row : constant String := "ZXCVBNM";

   function Letter (Scan_Code : Unsigned_64) return Unsigned_8 is
   begin
      if Scan_Code in Scan_Q .. Scan_P then
         return Character'Pos (Top_Row (Top_Row'First + Natural (Scan_Code - Scan_Q)));
      elsif Scan_Code in Scan_A .. Scan_L then
         return Character'Pos (Middle_Row (Middle_Row'First + Natural (Scan_Code - Scan_A)));
      elsif Scan_Code in Scan_Z .. Scan_M then
         return Character'Pos (Bottom_Row (Bottom_Row'First + Natural (Scan_Code - Scan_Z)));
      end if;
      return 0;
   end Letter;

   function One (B : Unsigned_8) return Sequence is ((Bytes => [1 => B, others => 0], Length => 1));
   function Two (A, B : Unsigned_8) return Sequence is ((Bytes => [A, B, 0, 0], Length => 2));
   --  ESC [ Final: the xterm cursor keys.
   function Cursor_Key (Final : Character) return Sequence is
     ((Bytes => [Escape_Code, CSI_Code, Character'Pos (Final), 0], Length => 3));

   function From_Key (Scan_Code, Modifiers : Unsigned_64) return Sequence is
      L : constant Unsigned_8 := Letter (Scan_Code);
   begin
      case Scan_Code is
         when Scan_Enter     => return One (CR_Code);
         when Scan_Backspace => return One (DEL_Code);
         when Scan_Tab       => return One (Tab_Code);
         when Scan_Escape    => return One (Escape_Code);
         when Scan_Up        => return Cursor_Key ('A');
         when Scan_Down      => return Cursor_Key ('B');
         when Scan_Right     => return Cursor_Key ('C');
         when Scan_Left      => return Cursor_Key ('D');
         when Scan_Home      => return Cursor_Key ('H');
         when Scan_End       => return Cursor_Key ('F');
         when Scan_Delete    =>
            return (Bytes => [Escape_Code, CSI_Code, Character'Pos ('3'), Character'Pos ('~')], Length => 4);
         when others => null;
      end case;
      if (Modifiers and Ctrl_Modifier) /= 0 then
         if L /= 0 then
            return One (L - Control_Offset);
         end if;
         case Scan_Code is
            when Scan_Left_Square  => return One (Character'Pos ('[') - Control_Offset);
            when Scan_Backslash    => return One (Character'Pos ('\') - Control_Offset);
            when Scan_Right_Square => return One (Character'Pos (']') - Control_Offset);
            when Scan_Space        => return One (NUL_Code);
            when others            => return Nothing;
         end case;
      elsif (Modifiers and Alt_Modifier) /= 0 and then L /= 0 then
         return Two (Escape_Code, (if (Modifiers and Shift_Modifier) /= 0 then L else L + Lower_Offset));
      end if;
      return Nothing;
   end From_Key;

   function From_Text (Code : Unsigned_64) return Sequence is
   begin
      if Code in Unsigned_64 (First_Printable) .. Unsigned_64 (Last_Printable) then
         return One (Unsigned_8 (Code));
      end if;
      return Nothing;
   end From_Text;
end Serial_Keys;
