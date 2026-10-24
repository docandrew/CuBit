pragma Ada_2022;

package body CCL.Canonical_Readers with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   procedure Expect (Source : String; Position : in out Positive; OK : in out Boolean; Literal : String) is
   begin
      if OK and then Next_Is (Source, Position, Literal) then
         Position := Position + Literal'Length;
      else
         OK := False;
      end if;
   end Expect;

   procedure Expect_Word (Source : String; Position : in out Positive; OK : in out Boolean; Literal : String) is
   begin
      if OK and then Next_Is_Word (Source, Position, Literal) then
         Position := Position + Literal'Length;
      else
         OK := False;
      end if;
   end Expect_Word;

   procedure Read_Integer
     (Source : String; Position : in out Positive; OK : in out Boolean;
      Low, High : Interfaces.Integer_64; Value : out Interfaces.Integer_64)
   is
      Decimal_Base : constant := 10;
      --  The largest magnitudes an Integer_64 spelling may have.
      Positive_Limit : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
      Negative_Limit : constant Interfaces.Unsigned_64 := Positive_Limit + 1;
      Cursor : Positive := Position;
      Negative : Boolean := False;
      Limit : Interfaces.Unsigned_64;
      Magnitude : Interfaces.Unsigned_64 := 0;
      Digit : Interfaces.Unsigned_64;
      Result : Interfaces.Integer_64;
   begin
      Value := Low;
      if not OK then
         return;
      end if;
      if Cursor <= Source'Last and then Source (Cursor) = '-' then
         Negative := True;
         Cursor := Cursor + 1;
      end if;
      Limit := (if Negative then Negative_Limit else Positive_Limit);
      if Cursor > Source'Last or else Source (Cursor) not in '0' .. '9' then
         OK := False;
         return;
      end if;
      if Source (Cursor) = '0' then
         --  Zero is "0": never signed, never followed by another digit.
         Cursor := Cursor + 1;
         if Negative or else (Cursor <= Source'Last and then Source (Cursor) in '0' .. '9') then
            OK := False;
            return;
         end if;
      else
         while Cursor <= Source'Last and then Source (Cursor) in '0' .. '9' loop
            pragma Loop_Invariant (Cursor in Position .. Source'Last and then Magnitude <= Limit);
            pragma Loop_Variant (Increases => Cursor);
            Digit := Character'Pos (Source (Cursor)) - Character'Pos ('0');
            if Magnitude > (Limit - Digit) / Decimal_Base then
               OK := False;
               return;
            end if;
            Magnitude := Magnitude * Decimal_Base + Digit;
            Cursor := Cursor + 1;
         end loop;
      end if;
      if not Negative then
         Result := Interfaces.Integer_64 (Magnitude);
      elsif Magnitude = Negative_Limit then
         Result := Interfaces.Integer_64'First;
      else
         Result := -Interfaces.Integer_64 (Magnitude);
      end if;
      if Result not in Low .. High then
         OK := False;
         return;
      end if;
      Value := Result;
      Position := Cursor;
   end Read_Integer;

   True_Spelling : constant String := "true";
   False_Spelling : constant String := "false";

   procedure Read_Boolean (Source : String; Position : in out Positive; OK : in out Boolean; Value : out Boolean) is
   begin
      Value := False;
      if not OK then
         return;
      elsif Next_Is_Word (Source, Position, True_Spelling) then
         Position := Position + True_Spelling'Length;
         Value := True;
      elsif Next_Is_Word (Source, Position, False_Spelling) then
         Position := Position + False_Spelling'Length;
      else
         OK := False;
      end if;
   end Read_Boolean;

   Quote : constant Character := '"';
   Escape : constant Character := '\';
   --  What follows Escape for a line feed and a carriage return.
   Line_Feed_Escape : constant Character := 'n';
   Return_Escape : constant Character := 'r';

   procedure Read_Text (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Text) is
      Cursor : Positive;
      Result : Text := Empty_Text;
   begin
      Item := Empty_Text;
      if not OK then
         return;
      end if;
      if Position > Source'Last or else Source (Position) /= Quote then
         OK := False;
         return;
      end if;
      Cursor := Position + 1;
      loop
         pragma Loop_Invariant (Cursor in Position + 1 .. Source'Last + 1);
         pragma Loop_Variant (Increases => Cursor);
         if Cursor > Source'Last then
            --  No closing quote.
            OK := False;
            return;
         end if;
         exit when Source (Cursor) = Quote;
         if Source (Cursor) in ASCII.LF | ASCII.CR or else Result.Length = Maximum_Canonical then
            OK := False;
            return;
         end if;
         Result.Length := Result.Length + 1;
         if Source (Cursor) /= Escape then
            Result.Data (Result.Length) := Source (Cursor);
         elsif Cursor < Source'Last and then Source (Cursor + 1) in Quote | Escape then
            Cursor := Cursor + 1;
            Result.Data (Result.Length) := Source (Cursor);
         elsif Cursor < Source'Last and then Source (Cursor + 1) = Line_Feed_Escape then
            Cursor := Cursor + 1;
            Result.Data (Result.Length) := ASCII.LF;
         elsif Cursor < Source'Last and then Source (Cursor + 1) = Return_Escape then
            Cursor := Cursor + 1;
            Result.Data (Result.Length) := ASCII.CR;
         else
            OK := False;
            return;
         end if;
         Cursor := Cursor + 1;
      end loop;
      Item := Result;
      Position := Cursor + 1;
   end Read_Text;

   procedure Finish (Source : String; Position : Positive; OK : in out Boolean) is
   begin
      OK := OK and then Position = Source'Last + 1;
   end Finish;
end CCL.Canonical_Readers;
