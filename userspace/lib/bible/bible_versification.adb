with Bible_Canon; use Bible_Canon;

package body Bible_Versification with SPARK_Mode is

   --  The Hebrew psalms whose title is a verse of its own (Hebrew and the
   --  Vulgate number it; English translations print it unnumbered above
   --  verse 1): two verses for 51, 52, 54 and 60, one for these.
   function Title_Verse_Count (Hebrew : Chapter_Number) return Title_Verses is
     (case Hebrew is
         when 51 | 52 | 54 | 60 => 2,
         when 3 .. 9 | 12 | 13 | 18 .. 22 | 30 | 31 | 34 | 36 | 38 .. 42 | 44 .. 49 | 53 | 55 .. 59
            | 61 .. 65 | 67 .. 70 | 75 .. 77 | 80 | 81 | 83 .. 85 | 88 | 89 | 92 | 102 | 108 | 140
            | 142 => 1,
         when others => 0);

   --  A verse number from arithmetic on one, kept in range.
   function Verse_Of (Value : Integer) return Verse_Number is
     (if Value < 1 then 1 elsif Value > MAXIMUM_VERSE then MAXIMUM_VERSE else Value);

   function Vulgate_Psalm (Hebrew : Chapter_Number) return Chapter_Number is
     (case Hebrew is
         when 1 .. 8 => Hebrew,
         when 9 | 10 => 9,
         when 11 .. 113 => Hebrew - 1,
         when 114 | 115 => 113,
         when 116 => 114,
         when 117 .. 146 => Hebrew - 1,
         when 147 => 146,
         when 148 .. 150 => Hebrew);

   procedure Hebrew_Psalms (Vulgate : Chapter_Number; First, Last : out Chapter_Number) is
   begin
      case Vulgate is
         when 1 .. 8 | 148 .. 150 => First := Vulgate; Last := Vulgate;
         when 9 => First := 9; Last := 10;
         when 10 .. 112 | 116 .. 145 => First := Vulgate + 1; Last := Vulgate + 1;
         when 113 => First := 114; Last := 115;
         when 114 | 115 => First := 116; Last := 116;
         when 146 | 147 => First := 147; Last := 147;
      end case;
   end Hebrew_Psalms;

   --  English to the Vulgate.
   function To_Vulgate (Item : Reference) return Reference is
      C : constant Chapter_Number := Item.Chapter;
      V : constant Verse_Number := Item.Verse;
   begin
      case Item.Book is
         when Psalms =>
            case C is
               when 9 => return (Psalms, 9, Verse_Of (V + 1));
               when 10 => return (Psalms, 9, Verse_Of (V + 21));
               when 114 => return (Psalms, 113, V);
               when 115 => return (Psalms, 113, Verse_Of (V + 8));
               when 116 => return (if V <= 9 then (Psalms, 114, V) else (Psalms, 115, V - 9));
               when 147 => return (if V <= 11 then (Psalms, 146, V) else (Psalms, 147, V - 11));
               when 148 .. 150 => return Item;
               when others => return (Psalms, Vulgate_Psalm (C), Verse_Of (V + Title_Verse_Count (C)));
            end case;
         when Esther_Greek => return (Esther, C, V);
         --  Its 68 verses are the Vulgate's 3:24-90 (67): the last two share 3:90.
         when Song_Of_The_Three => return (Daniel, 3, Natural'Min (V, 67) + 23);
         when Susanna => return (Daniel, 13, V);
         when Bel_And_The_Dragon => return (Daniel, 14, V);
         when Daniel =>
            if C = 3 and then V >= 24 then
               return (Daniel, 3, Verse_Of (V + 67));
            elsif C = 4 and then V <= 3 then
               return (Daniel, 3, V + 97);
            elsif C = 4 then
               return (Daniel, 4, V - 3);
            else
               return Item;
            end if;
         when others => return Item;
      end case;
   end To_Vulgate;

   --  The Vulgate to English.
   function From_Vulgate (Item : Reference) return Reference is
      C : constant Chapter_Number := Item.Chapter;
      V : constant Verse_Number := Item.Verse;
   begin
      case Item.Book is
         when Psalms =>
            case C is
               when 9 => return (if V <= 21 then (Psalms, 9, Verse_Of (V - 1)) else (Psalms, 10, V - 21));
               when 113 => return (if V <= 8 then (Psalms, 114, V) else (Psalms, 115, V - 8));
               when 114 => return (Psalms, 116, V);
               when 115 => return (Psalms, 116, Verse_Of (V + 9));
               when 146 => return (Psalms, 147, V);
               when 147 => return (Psalms, 147, Verse_Of (V + 11));
               when 1 .. 8 => return (Psalms, C, Verse_Of (V - Title_Verse_Count (C)));
               when 10 .. 112 | 116 .. 145 =>
                  return (Psalms, C + 1, Verse_Of (V - Title_Verse_Count (C + 1)));
               when 148 .. 150 => return Item;
            end case;
         when Esther =>
            return (if C >= 11 or else (C = 10 and then V >= 4) then (Esther_Greek, C, V) else Item);
         when Daniel =>
            case C is
               when 3 =>
                  if V <= 23 then
                     return Item;
                  elsif V <= 90 then
                     return (Song_Of_The_Three, 1, V - 23);
                  elsif V <= 97 then
                     return (Daniel, 3, V - 67);
                  else
                     return (Daniel, 4, Verse_Of (V - 97));
                  end if;
               when 4 => return (Daniel, 4, Verse_Of (V + 3));
               when 13 => return (Susanna, 1, V);
               when 14 => return (Bel_And_The_Dragon, 1, V);
               when others => return Item;
            end case;
         when others => return Item;
      end case;
   end From_Vulgate;

   function Map (From, To : Numbering; Item : Reference) return Reference is
     (if From = To then Item
      elsif To = Vulgate then To_Vulgate (Item)
      else From_Vulgate (Item));

end Bible_Versification;
