------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The bytes a serial terminal sends for a key (VMM-001): what vmm puts in
--  the guest UART's receive FIFO for the desktop's key and text events.
--
--  @description
--  Text events (the desktop's composed characters, Shift and Caps Lock
--  applied) send their printable ASCII byte. Key events send what a VT100
--  or xterm sends for keys that are not text: Enter CR, Backspace DEL,
--  Tab, Escape, the arrows, Home, End and Delete as ANSI (xterm) escape
--  sequences, Ctrl with a letter or [ \ ] its control code (Ctrl+C is ETX),
--  and Alt with a letter ESC then the letter (the meta prefix). The desktop
--  sends no text event while Ctrl or Alt is held, so these never send twice.
--  Letters under Ctrl and Alt are read from their set-1 scan codes in the
--  US layout (the physical keys), not from the desktop's layout.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Serial_Keys with SPARK_Mode, Pure is

   --  The longest sequence: Delete, ESC [ 3 ~.
   Max_Bytes : constant := 4;
   subtype Byte_Count is Natural range 0 .. Max_Bytes;
   subtype Byte_Index is Positive range 1 .. Max_Bytes;
   type Byte_Array is array (Byte_Index) of Unsigned_8;
   type Sequence is record
      Bytes  : Byte_Array := [others => 0];
      Length : Byte_Count := 0;
   end record;
   Nothing : constant Sequence := (others => <>);

   --  ASCII control codes and the escape-sequence bytes.
   NUL_Code       : constant Unsigned_8 := 16#00#;
   ETX_Code       : constant Unsigned_8 := 16#03#;   --  Ctrl+C
   EOT_Code       : constant Unsigned_8 := 16#04#;   --  Ctrl+D
   Tab_Code       : constant Unsigned_8 := 16#09#;
   CR_Code        : constant Unsigned_8 := 16#0D#;
   Escape_Code    : constant Unsigned_8 := 16#1B#;
   DEL_Code       : constant Unsigned_8 := 16#7F#;
   CSI_Code       : constant Unsigned_8 := Character'Pos ('[');
   Control_Offset : constant Unsigned_8 := 16#40#;   --  Ctrl+X = 'X' - 40h
   Lower_Offset   : constant Unsigned_8 := 16#20#;   --  'a' - 'A'
   First_Printable : constant Unsigned_8 := 16#20#;
   Last_Printable  : constant Unsigned_8 := 16#7E#;

   --  Set-1 scan codes of the desktop's key events (payload0).
   Scan_Escape      : constant Unsigned_64 := 16#01#;
   Scan_Backspace   : constant Unsigned_64 := 16#0E#;
   Scan_Tab         : constant Unsigned_64 := 16#0F#;
   Scan_Left_Square : constant Unsigned_64 := 16#1A#;
   Scan_Right_Square : constant Unsigned_64 := 16#1B#;
   Scan_Enter       : constant Unsigned_64 := 16#1C#;
   Scan_Backslash   : constant Unsigned_64 := 16#2B#;
   Scan_Space       : constant Unsigned_64 := 16#39#;
   Scan_Home        : constant Unsigned_64 := 16#47#;
   Scan_Up          : constant Unsigned_64 := 16#48#;
   Scan_Left        : constant Unsigned_64 := 16#4B#;
   Scan_Right       : constant Unsigned_64 := 16#4D#;
   Scan_End         : constant Unsigned_64 := 16#4F#;
   Scan_Down        : constant Unsigned_64 := 16#50#;
   Scan_Delete      : constant Unsigned_64 := 16#53#;
   --  The letter rows: Q .. P, A .. L, Z .. M.
   Scan_Q : constant Unsigned_64 := 16#10#;  Scan_P : constant Unsigned_64 := 16#19#;
   Scan_A : constant Unsigned_64 := 16#1E#;  Scan_L : constant Unsigned_64 := 16#26#;
   Scan_Z : constant Unsigned_64 := 16#2C#;  Scan_M : constant Unsigned_64 := 16#32#;

   --  The desktop's KEYMOD_* bits (CuBit.UI.App).
   Shift_Modifier : constant Unsigned_64 := 1;
   Ctrl_Modifier  : constant Unsigned_64 := 2;
   Alt_Modifier   : constant Unsigned_64 := 4;

   --  The upper-case letter a scan code's key bears (US layout), or 0.
   function Letter (Scan_Code : Unsigned_64) return Unsigned_8
     with Post => Letter'Result = 0 or else Letter'Result in Character'Pos ('A') .. Character'Pos ('Z');

   --  A key press (not a release) with its modifiers.
   function From_Key (Scan_Code, Modifiers : Unsigned_64) return Sequence;
   --  A text event's character.
   function From_Text (Code : Unsigned_64) return Sequence
     with Post => From_Text'Result.Length <= 1
       and then (if From_Text'Result.Length = 1 then
                   From_Text'Result.Bytes (1) in First_Printable .. Last_Printable);
end Serial_Keys;
