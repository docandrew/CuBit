package body Bible_Text with SPARK_Mode is

   function Byte (C : Character) return Natural is (Character'Pos (C));

   --  The face character for the UTF-8 sequence at Source (I), and how many
   --  bytes it takes.
   procedure Decode (Source : String; I : Positive; Face : out Character; Width : out Positive)
     with Pre  => I in Source'Range,
          Post => Width <= 3 and then Width - 1 <= Source'Last - I
   is
      Lead : constant Natural := Byte (Source (I));
      function Next (K : Positive) return Natural is (Byte (Source (I + K)))
        with Pre => K <= 2 and then I in Source'Range and then Source'Last - I >= K;
   begin
      Face := UNKNOWN;
      Width := 1;
      if Lead in 16#20# .. 16#7E# then
         Face := Source (I);
      elsif Lead in 16#09# | 16#0A# | 16#0D# then
         Face := ' ';
      elsif Lead in 16#C2# .. 16#C5# and then I < Source'Last and then Next (1) in 16#80# .. 16#BF# then
         Width := 2;
         case Lead is
            when 16#C2# =>
               --  U+0080 .. U+00BF: the same byte (no-break space, one
               --  half ...); the C1 controls are not text.
               if Next (1) >= 16#A0# then
                  Face := Character'Val (Next (1));
               end if;
            when 16#C3# =>
               Face := Character'Val (Next (1) + 16#40#);   --  U+00C0 .. U+00FF
            when 16#C4# =>
               if Next (1) = 16#93# then Face := 'e'; end if;   --  U+0113
            when others =>
               --  U+0152, U+0153: Windows-1252's OE and oe; U+014D folds.
               if Lead = 16#C5# then
                  case Next (1) is
                     when 16#92# => Face := OE_UPPER;
                     when 16#93# => Face := OE_LOWER;
                     when 16#8D# => Face := 'o';
                     when others => null;
                  end case;
               end if;
         end case;
      elsif Lead = 16#E2# and then I < Source'Last - 1 and then Next (1) = 16#80#
        and then Next (2) in 16#80# .. 16#BF#
      then
         Width := 3;
         case Next (2) is
            when 16#93# => Face := EN_DASH;
            when 16#94# => Face := EM_DASH;
            when 16#98# => Face := LEFT_SINGLE;
            when 16#99# => Face := RIGHT_SINGLE;
            when 16#9C# => Face := LEFT_DOUBLE;
            when 16#9D# => Face := RIGHT_DOUBLE;
            when 16#A6# => Face := ELLIPSIS;
            when others => null;
         end case;
      elsif Lead in 16#C0# .. 16#DF# and then I < Source'Last then
         Width := 2;
      elsif Lead in 16#E0# .. 16#EF# and then I < Source'Last - 1 then
         Width := 3;
      end if;
   end Decode;

   procedure To_Face (Source : String; Target : out String; Used : out Natural) is
      I : Integer := Source'First;
      Face : Character;
      Width : Positive;
   begin
      Target := [others => ' '];
      Used := 0;
      if Source'Length = 0 then
         return;
      end if;
      loop
         pragma Loop_Invariant (I in Source'Range);
         pragma Loop_Invariant (Used <= I - Source'First);
         pragma Loop_Variant (Increases => I);
         Decode (Source, I, Face, Width);
         Target (Target'First + Used) := Face;
         Used := Used + 1;
         exit when Width > Source'Last - I;
         I := I + Width;
      end loop;
   end To_Face;

   function Face_Text (Source : String) return String is
      Target : String (1 .. Source'Length);
      Used : Natural;
   begin
      To_Face (Source, Target, Used);
      return Target (1 .. Used);
   end Face_Text;

   function To_ASCII (Source : String) return String is
      Face : constant String := Face_Text (Source);
      Result : String (1 .. 3 * Face'Length) := [others => ' '];
      Used : Natural := 0;
      procedure Put (Text : String)
        with Pre => Text'Length <= 3 and then Used <= Result'Length - Text'Length,
             Post => Used = Used'Old + Text'Length
      is
      begin
         Result (Used + 1 .. Used + Text'Length) := Text;
         Used := Used + Text'Length;
      end Put;
   begin
      for I in Face'Range loop
         pragma Loop_Invariant (Used <= 3 * (I - Face'First));
         declare
            C : constant Character := Face (I);
         begin
            if C in ' ' .. '~' then
               Put ([1 => C]);
            elsif C in LEFT_SINGLE | RIGHT_SINGLE then
               Put ("'");
            elsif C in LEFT_DOUBLE | RIGHT_DOUBLE then
               Put ("""");
            elsif C = EN_DASH then
               Put ("-");
            elsif C = EM_DASH then
               Put ("--");
            elsif C = ELLIPSIS then
               Put ("...");
            elsif C = NO_BREAK then
               Put (" ");
            elsif C = OE_LOWER then
               Put ("oe");
            elsif C = OE_UPPER then
               Put ("OE");
            else
               case Character'Pos (C) is
                  when 16#E6# => Put ("ae");
                  when 16#C6# => Put ("AE");
                  when 16#E0# .. 16#E5# => Put ("a");
                  when 16#C0# .. 16#C5# => Put ("A");
                  when 16#E8# .. 16#EB# => Put ("e");
                  when 16#C8# .. 16#CB# => Put ("E");
                  when 16#D7# => Put ("x");
                  when 16#BD# => Put ("1/2");
                  when others => Put ("?");
               end case;
            end if;
         end;
      end loop;
      return Result (1 .. Used);
   end To_ASCII;

end Bible_Text;
