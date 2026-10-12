pragma Ada_2022;
package body Desktop_Bootstrap_Notes with SPARK_Mode is
   function Make (Text : String) return Line
     with Pre => Text'Last < Positive'Last
   is
      Result : Line;
      Length : constant Line_Length := Natural'Min (Text'Length, Line_Bytes);
   begin
      if Length > 0 then
         Result.Text (1 .. Length) := Text (Text'First .. Text'First + (Length - 1));
      end if;
      Result.Length := Length;
      return Result;
   end Make;

   function Number_After (Prefix, Text : String) return Record_Number is
      Value : Record_Number := 0;
      Digit : Natural;
   begin
      if Text'Length <= Prefix'Length
        or else Text (Text'First .. Text'First + (Prefix'Length - 1)) /= Prefix
      then
         return 0;
      end if;
      for I in Text'First + Prefix'Length .. Text'Last loop
         exit when Text (I) not in '0' .. '9';
         Digit := Character'Pos (Text (I)) - Character'Pos ('0');
         if Value > (Number_Limit - Digit) / 10 then
            return Number_Limit;
         end if;
         Value := Value * 10 + Digit;
      end loop;
      return Value;
   end Number_After;

   procedure Note (N : in out Notes; Important : Boolean; Text : String) is
      Number : constant Record_Number := Denial_Number (Text);
      Kill   : constant Record_Number := Kill_Number (Text);
   begin
      if Number > N.Highest_Denial then
         N.Highest_Denial := Number;
      end if;
      if Kill > 0 then
         if Kill > N.Highest_Kill then
            N.Highest_Kill := Kill;
         end if;
         if N.Kills_Used = Kill_Capacity then
            N.Kills (1 .. Kill_Capacity - 1) := N.Kills (2 .. Kill_Capacity);
            N.Kills (Kill_Capacity) := Make (Text);
         else
            N.Kills_Used := N.Kills_Used + 1;
            N.Kills (N.Kills_Used) := Make (Text);
         end if;
      end if;
      if Is_Driver_Record (Text) then
         N.Driver := Make (Text);
      end if;
      if Is_Map_Record (Text) then
         if N.Map_Used < Map_Capacity then
            N.Map_Used := N.Map_Used + 1;
            N.Map (N.Map_Used) := Make (Text);
         end if;
      elsif Important then
         if N.Shown_Total < Natural'Last then
            N.Shown_Total := N.Shown_Total + 1;
         end if;
         if N.Recent_Used = Recent_Capacity then
            N.Recent (1 .. Recent_Capacity - 1) := N.Recent (2 .. Recent_Capacity);
            N.Recent (Recent_Capacity) := Make (Text);
         else
            N.Recent_Used := N.Recent_Used + 1;
            N.Recent (N.Recent_Used) := Make (Text);
         end if;
      end if;
   end Note;
end Desktop_Bootstrap_Notes;
