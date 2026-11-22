package body Bible_References with SPARK_Mode is

   subtype Number_Text_Length is Positive range 1 .. 3;
   function Digits_Of (Value : Natural) return Number_Text_Length is
     (if Value < 10 then 1 elsif Value < 100 then 2 else 3)
   with Pre => Value <= 999;

   function Decimal (Value : Natural) return String
     with Pre => Value <= 999, Post => Decimal'Result'First = 1 and then Decimal'Result'Length = Digits_Of (Value)
   is
      Result : String (1 .. Digits_Of (Value));
      Rest : Natural := Value;
   begin
      for I in reverse Result'Range loop
         pragma Loop_Invariant (Rest <= 999);
         Result (I) := Character'Val (Character'Pos ('0') + Rest mod 10);
         Rest := Rest / 10;
      end loop;
      return Result;
   end Decimal;

   function Number_Image (Value : Natural) return String is (Decimal (Value));

   function Image (Item : Reference) return String is
     (Name (Item.Book) & " " & Decimal (Item.Chapter) & ":" & Decimal (Item.Verse));

   function Image (Table : Versification; Item : Passage) return String is
      From : Reference renames Item.From;
      To : Reference renames Item.To;
      Whole_From : constant Boolean := From.Verse = 1;
      Whole_To : constant Boolean :=
        To.Chapter <= Table.Chapters (To.Book) and then To.Verse = Verses_In (Table, To.Book, To.Chapter);
   begin
      if From.Book /= To.Book then
         --  Never made by the parser; shown as its two ends.
         return Name (From.Book) & " " & Decimal (From.Chapter);
      elsif Whole_From and then Whole_To then
         if From.Chapter = To.Chapter then
            return Name (From.Book) & " " & Decimal (From.Chapter);
         else
            return Name (From.Book) & " " & Decimal (From.Chapter) & "-" & Decimal (To.Chapter);
         end if;
      elsif From.Chapter = To.Chapter then
         if From.Verse = To.Verse then
            return Image (From);
         else
            return Image (From) & "-" & Decimal (To.Verse);
         end if;
      else
         return Image (From) & "-" & Decimal (To.Chapter) & ":" & Decimal (To.Verse);
      end if;
   end Image;

end Bible_References;
