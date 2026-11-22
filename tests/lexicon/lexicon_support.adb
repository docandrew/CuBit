with Ada.Directories;
with Ada.Streams.Stream_IO;

package body Lexicon_Support is

   function Load (Path : String) return Image_Access is
      use Ada.Streams;
      Size : constant Natural := Natural (Ada.Directories.Size (Path));
      Result : constant Image_Access := new Byte_Array (0 .. Size - 1);
      File : Stream_IO.File_Type;
      Buffer : Stream_Element_Array (1 .. 2 ** 20);
      Last : Stream_Element_Offset;
      At_Byte : Natural := 0;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, Path);
      while not Stream_IO.End_Of_File (File) loop
         Stream_IO.Read (File, Buffer, Last);
         for I in Buffer'First .. Last loop
            Result (At_Byte) := Unsigned_8 (Buffer (I));
            At_Byte := At_Byte + 1;
         end loop;
      end loop;
      Stream_IO.Close (File);
      return Result;
   end Load;

   procedure Decode (Page : Answer_Bytes; Into : in out Item_Lists.Vector; Damaged : out Boolean) is
      Position : Natural := 0;
      Found : Boolean;
      Got : Item;
      First : Unsigned_32;
      Length : Unsigned_16;
   begin
      loop
         Next (Page, Position, Found, Damaged, Got);
         exit when not Found;
         declare
            Text : String (1 .. Got.Length);
         begin
            for I in 1 .. Got.Length loop
               Text (I) := Character'Val (Page (Got.First + I - 1));
            end loop;
            First := 0;
            Length := 0;
            if Got.Kind = Misspelling then
               Span (Page, Got, First, Length);
            elsif Got.Kind in Checked_Upto | Continue_At | Sense_Total then
               First := Number (Page, Got);
            elsif Got.Kind = Sense_Start and then Got.Length = 1 then
               First := Unsigned_32 (Page (Got.First));
            end if;
            Into.Append (Decoded'(Length => Got.Length, Kind => Got.Kind, Number => First, Span_Length => Length,
                          Text => Text));
         end;
      end loop;
   end Decode;

   procedure Ask_All
     (Data : Byte_Array; L : Layout; Op : Lexicon_Answers.Operation; Text : String;
      Page_Bytes : Positive; Suggestions : Lexicon_Answers.Suggestion_Count;
      Into : out Item_Lists.Vector; Pages : out Natural)
   is
      use type Lexicon_Answers.Operation;
      Resume : Unsigned_32 := 0;
      Page : Answer_Bytes (0 .. Page_Bytes - 1);
      Used : Natural;
      One : Item_Lists.Vector;
      Damaged : Boolean;
      More : Boolean;
   begin
      Into.Clear;
      Pages := 0;
      loop
         Lexicon_Answers.Answer (Data, L, Op, Text, Resume, Suggestions, Page, Used);
         Pages := Pages + 1;
         One.Clear;
         Decode (Page (0 .. Used - 1), One, Damaged);
         if Damaged then
            raise Program_Error with "damaged page";
         end if;
         More := False;
         for D of One loop
            if D.Kind = Continue_At then
               Resume := D.Number;
               More := True;
            elsif D.Kind = Checked_Upto and then Op = Lexicon_Answers.Check
              and then Natural (D.Number) < Text'Length
            then
               if D.Number <= Resume and then Pages > 1 then
                  raise Program_Error with "check made no progress";
               end if;
               Resume := D.Number;
               More := True;
            elsif D.Kind = Checked_Upto then
               null;
            else
               Into.Append (D);
            end if;
         end loop;
         exit when not More;
         if Pages > 100_000 then
            raise Program_Error with "no progress";
         end if;
      end loop;
   end Ask_All;

   function Has (Items : Item_Lists.Vector; Kind : Item_Kind; Text : String) return Boolean is
   begin
      for D of Items loop
         if D.Kind = Kind and then D.Text = Text then
            return True;
         end if;
      end loop;
      return False;
   end Has;

   function Count_Items (Items : Item_Lists.Vector; Kind : Item_Kind) return Natural is
      N : Natural := 0;
   begin
      for D of Items loop
         if D.Kind = Kind then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Count_Items;

   function Texts (Items : Item_Lists.Vector; Kind : Item_Kind) return String is
      function Join (From : Positive) return String is
        (if From > Natural (Items.Length) then ""
         elsif Items (From).Kind = Kind then Items (From).Text & "|" & Join (From + 1)
         else Join (From + 1));
   begin
      return Join (1);
   end Texts;

end Lexicon_Support;
