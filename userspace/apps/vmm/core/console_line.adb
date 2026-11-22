------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  The line the guest is writing on its serial console (console_line.ads).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Console_Line with SPARK_Mode is

   BEL : constant Unsigned_8 := 16#07#;
   BS  : constant Unsigned_8 := 16#08#;
   HT  : constant Unsigned_8 := 16#09#;
   LF  : constant Unsigned_8 := 16#0A#;
   CR  : constant Unsigned_8 := 16#0D#;
   CAN : constant Unsigned_8 := 16#18#;
   SUB : constant Unsigned_8 := 16#1A#;
   ESC : constant Unsigned_8 := 16#1B#;
   DEL : constant Unsigned_8 := 16#7F#;
   First_Printable : constant Unsigned_8 := 16#20#;
   Tab_Stop : constant := 8;
   --  CSI bytes (ECMA-48 5.4): parameters 30h-3Fh, intermediates
   --  20h-2Fh, the final byte 40h-7Eh.
   subtype Parameter_Byte is Unsigned_8 range 16#30# .. 16#3F#;
   subtype Intermediate_Byte is Unsigned_8 range 16#20# .. 16#2F#;
   subtype Final_Byte is Unsigned_8 range 16#40# .. 16#7E#;
   Digit_Zero : constant Unsigned_8 := Character'Pos ('0');
   Digit_Nine : constant Unsigned_8 := Character'Pos ('9');
   Separator  : constant Unsigned_8 := Character'Pos (';');
   Erase_To_End : constant := 0;
   Erase_To_Cursor : constant := 1;
   Erase_All : constant := 2;
   Last_Column : constant Column := Width - 1;

   function Count (L : Line) return Positive is (if L.Has_Value and then L.Value > 0 then L.Value else 1);

   procedure Blank (L : in out Line; First, Last : Natural) is
   begin
      for I in First .. Last loop
         pragma Loop_Invariant (L.Cursor = L.Cursor'Loop_Entry);
         if I in Text_Index then
            L.Text (I) := ' ';
         end if;
      end loop;
   end Blank;

   procedure Final (L : in out Line; B : Final_Byte)
     with Pre => L.Cursor < Width, Post => L.Cursor < Width
   is
   begin
      case Character'Val (B) is
         when 'C' => L.Cursor := Natural'Min (L.Cursor + Natural'Min (Count (L), Width), Last_Column);
         when 'D' => L.Cursor := L.Cursor - Natural'Min (Count (L), L.Cursor);
         when 'G' => L.Cursor := Natural'Min (Count (L) - 1, Last_Column);
         when 'K' | 'J' =>
            --  Erase in line, and erase in display as far as this one line
            --  goes (busybox's line editor erases a deleted character with
            --  ESC [ J).
            case (if L.Has_Value then L.Value else Erase_To_End) is
               when Erase_To_End =>
                  Blank (L, L.Cursor + 1, L.Length);
                  L.Length := Natural'Min (L.Length, L.Cursor);
               when Erase_To_Cursor =>
                  Blank (L, 1, Natural'Min (L.Cursor + 1, L.Length));
               when Erase_All =>
                  Blank (L, 1, L.Length);
                  L.Length := 0;
               when others => null;
            end case;
         when others => null;
      end case;
   end Final;

   --  A control byte (below 20h, or DEL): acted on in every state.
   procedure Control (L : in out Line; B : Unsigned_8; Done : out Boolean)
     with Pre => L.Cursor < Width, Post => L.Cursor < Width
   is
   begin
      Done := False;
      case B is
         when LF => Done := True;
         when CR => L.Cursor := 0;
         when BS => L.Cursor := L.Cursor - Natural'Min (1, L.Cursor);
         when HT => L.Cursor := Natural'Min ((L.Cursor / Tab_Stop + 1) * Tab_Stop, Last_Column);
         when CAN | SUB => L.State := Ground;
         when others => null;   --  BEL, NUL, DEL and the rest
      end case;
   end Control;

   procedure Put (L : in out Line; B : Unsigned_8; Done : out Boolean) is
   begin
      Done := False;
      if B = ESC then
         L.State := (if L.State in Operating_System_Command then OSC_Escape else After_Escape);
         return;
      end if;
      case L.State is
         when Operating_System_Command =>
            if B = BEL then
               L.State := Ground;
            end if;
         when OSC_Escape =>
            L.State := Ground;          --  ESC \ ends it; anything else ends it too
         when After_Escape =>
            if B = Character'Pos ('[') then
               L.State := Control_Sequence;
               L.Value := 0;
               L.Has_Value := False;
               L.In_Second := False;
            elsif B = Character'Pos (']') then
               L.State := Operating_System_Command;
            elsif B < First_Printable then
               Control (L, B, Done);
            else
               L.State := Ground;       --  a two-byte sequence (ESC 7, ESC c ...)
            end if;
         when Control_Sequence =>
            if B in Digit_Zero .. Digit_Nine then
               if not L.In_Second then
                  L.Value := (if L.Value > (Parameter_Ceiling - Natural (B - Digit_Zero)) / 10
                              then Parameter_Ceiling
                              else L.Value * 10 + Natural (B - Digit_Zero));
                  L.Has_Value := True;
               end if;
            elsif B = Separator then
               L.In_Second := True;
            elsif B in Parameter_Byte | Intermediate_Byte then
               null;                    --  '?', '>' and intermediates: taken
            elsif B in Final_Byte then
               Final (L, B);
               L.State := Ground;
            elsif B < First_Printable or else B = DEL then
               Control (L, B, Done);
            else
               L.State := Ground;
            end if;
         when Ground =>
            if B < First_Printable or else B = DEL then
               Control (L, B, Done);
            else
               L.Text (L.Cursor + 1) := Character'Val (B);
               L.Cursor := L.Cursor + 1;
               L.Length := Natural'Max (L.Length, L.Cursor);
               --  Past the last column: the line wraps.
               Done := L.Cursor = Width;
            end if;
      end case;
   end Put;

   procedure Clear (L : in out Line) is
   begin
      Blank (L, 1, L.Length);
      L.Length := 0;
      L.Cursor := 0;
   end Clear;
end Console_Line;
