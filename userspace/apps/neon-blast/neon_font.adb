------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Neon_Font with SPARK_Mode is

   --  Each glyph is a string of four-digit segments "x0y0x1y1".
   function Encoded (Letter : Character) return String is
   begin
      case Letter is
         when 'A' => return "06020220204242460343";
         when 'B' => return "0006003030414142423303333344444545363606";
         when 'C' => return "40101001010505161646";
         when 'D' => return "000600303041414545363606";
         when 'E' => return "0006004003330646";
         when 'F' => return "000600400333";
         when 'G' => return "41303010100101050516164646434323";
         when 'H' => return "000640460343";
         when 'I' => return "103020261636";
         when 'J' => return "4045453636161605";
         when 'K' => return "0006401313460313";
         when 'L' => return "00060646";
         when 'M' => return "0600002323404046";
         when 'N' => return "060000464640";
         when 'O' => return "10303041414545363616160505010110";
         when 'P' => return "060000303041414242333303";
         when 'Q' => return "103030414145453636161605050101102446";
         when 'R' => return "0600003030414142423333032346";
         when 'S' => return "41303010100101020213133333444445453636161605";
         when 'T' => return "00402026";
         when 'U' => return "00050516163636454540";
         when 'V' => return "00262640";
         when 'W' => return "0016162323363640";
         when 'X' => return "00464006";
         when 'Y' => return "002340232326";
         when 'Z' => return "004040060646";
         when '0' => return "103030414145453636161605050101104105";
         when '1' => return "112020261636";
         when '2' => return "011010303041414242060646";
         when '3' => return "00404023233333444445453636161605";
         when '4' => return "303630040444";
         when '5' => return "4000000303333344444545363606";
         when '6' => return "301010010105051616363645454444333303";
         when '7' => return "00404016";
         when '8' =>
            return "103030414142423333131302020101103344444545363616160505040413";
         when '9' => return "431313020201011010303041414545363616";
         when ':' => return "21222425";
         when '.' => return "2526";
         when ',' => return "2516";
         when '-' => return "1333";
         when '+' => return "13332224";
         when '!' => return "20242526";
         when '/' => return "4006";
         when '%' => return "400600103646";
         when ''' => return "2021";
         when '?' => return "0110103030414142422323242526";
         when '>' => return "00232306";
         when '<' => return "40030346";
         when '=' => return "12321434";
         when '(' => return "211111151525";
         when ')' => return "213131353525";
         when '*' => return "1335331522241434";
         when '#' => return "1115313502420444";
         when 'x' => return "13353315";
         when others => return "";
      end case;
   end Encoded;

   function Digit (C : Character) return Natural is
     (if C in '0' .. '9' then Character'Pos (C) - Character'Pos ('0') else 0);

   function Shape (Letter : Character) return Glyph is
      Code : constant String := Encoded (Letter);
      Result : Glyph;
      Index : Natural := Code'First;
   begin
      while Index + 3 <= Code'Last and then Result.Count < Maximum_Segments loop
         Result.Count := Result.Count + 1;
         Result.Segments (Result.Count) :=
           (X0 => Natural'Min (Grid_Width, Digit (Code (Index))),
            Y0 => Natural'Min (Grid_Height, Digit (Code (Index + 1))),
            X1 => Natural'Min (Grid_Width, Digit (Code (Index + 2))),
            Y1 => Natural'Min (Grid_Height, Digit (Code (Index + 3))));
         Index := Index + 4;
      end loop;
      return Result;
   end Shape;

end Neon_Font;
