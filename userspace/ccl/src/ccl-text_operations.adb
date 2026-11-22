with CCL.Checked_Arithmetic;

package body CCL.Text_Operations with SPARK_Mode => On is

   CASE_OFFSET : constant := Character'Pos ('a') - Character'Pos ('A');

   procedure Transform
     (Item : Operation; Subject : String; Result : out String)
   is
   begin
      Result := [others => ' '];
      for I in 0 .. Subject'Length - 1 loop
         declare
            C : constant Character :=
              (if Item = Reverse_Text then Subject (Subject'Last - I)
               else Subject (Subject'First + I));
         begin
            Result (Result'First + I) :=
              (if Item = Upper and then C in 'a' .. 'z' then
                  Character'Val (Character'Pos (C) - CASE_OFFSET)
               elsif Item = Lower and then C in 'A' .. 'Z' then
                  Character'Val (Character'Pos (C) + CASE_OFFSET)
               else C);
         end;
      end loop;
   end Transform;

   procedure Slice
     (Item : Operation; Subject : String; N : Integer_64;
      Low : out Positive; High : out Natural)
   is
      Count : constant Natural :=
        (if N <= 0 then 0
         elsif N >= Integer_64 (Subject'Length) then Subject'Length
         else Natural (N));
   begin
      pragma Assert (Count <= Subject'Length);
      Low := 1;
      High := Subject'Length;
      case Item is
         when Trim =>
            while Low <= High and then Is_Blank (Subject (Low)) loop
               pragma Loop_Invariant (Low <= High and then High <= Subject'Last);
               Low := Low + 1;
            end loop;
            while High >= Low and then Is_Blank (Subject (High)) loop
               pragma Loop_Invariant (Low <= High and then High <= Subject'Last);
               High := High - 1;
            end loop;
         when First_Chars => High := Count;
         when Last_Chars => Low := Subject'Length - Count + 1;
         when Skip_Chars => Low := Count + 1;
         when others => High := 0;
      end case;
   end Slice;

   function Find (Subject, Pattern : String; From : Positive) return Natural is
   begin
      if Pattern'Length > Subject'Length or else
        From > Subject'Length - Pattern'Length + 1
      then
         return 0;
      end if;
      for P in From .. Subject'Length - Pattern'Length + 1 loop
         if Subject (P .. P + Pattern'Length - 1) = Pattern then
            return P;
         end if;
      end loop;
      return 0;
   end Find;

   function Test (Item : Operation; Subject, Pattern : String) return Boolean is
     (Pattern'Length = 0 or else
      (Pattern'Length <= Subject'Length and then
       (case Item is
          when Starts_With =>
            Subject (Subject'First .. Subject'First + Pattern'Length - 1) = Pattern,
          when Ends_With =>
            Subject (Subject'Last - Pattern'Length + 1 .. Subject'Last) = Pattern,
          when others => Find (Subject, Pattern, 1) > 0)));

   procedure Replace_All
     (Subject, Pattern, Replacement : String;
      Result : out String; Length : out String_Length; Status : out Outcome)
   is
      P : Positive := 1;
      Next : Natural;
      Piece : Natural;
   begin
      Result := [others => ' '];
      Length := 0;
      Status := Done;
      if Pattern'Length = 0 then
         Result (1 .. Subject'Length) := Subject;
         Length := Subject'Length;
         return;
      end if;
      --  Copy the text before each match, then the replacement.
      while P <= Subject'Length loop
         pragma Loop_Invariant (Length <= Result'Length);
         Next := Find (Subject, Pattern, P);
         Piece := (if Next = 0 then Subject'Length - P + 1 else Next - P);
         if Piece > Result'Length - Length then
            Status := Text_Exhausted;
            return;
         end if;
         Result (Length + 1 .. Length + Piece) := Subject (P .. P + Piece - 1);
         Length := Length + Piece;
         exit when Next = 0;
         if Replacement'Length > Result'Length - Length then
            Status := Text_Exhausted;
            return;
         end if;
         Result (Length + 1 .. Length + Replacement'Length) := Replacement;
         Length := Length + Replacement'Length;
         exit when Next > Subject'Length - Pattern'Length;
         P := Next + Pattern'Length;
      end loop;
   end Replace_All;

   procedure Parse_Integer (Subject : String; Value : out Integer_64; Status : out Outcome) is
      DECIMAL_BASE : constant := 10;
      Low : Positive := 1;
      High : Natural := Subject'Length;
      Negative : Boolean := False;
      Scaled, Digit : Integer_64;
      Overflowed : Boolean;
   begin
      Value := 0;
      Status := Invalid_Number;
      while Low <= High and then Is_Blank (Subject (Low)) loop
         pragma Loop_Invariant (Low <= High and then High <= Subject'Last);
         Low := Low + 1;
      end loop;
      while High >= Low and then Is_Blank (Subject (High)) loop
         pragma Loop_Invariant (Low <= High and then High <= Subject'Last);
         High := High - 1;
      end loop;
      if Low <= High and then Subject (Low) in '-' | '+' then
         Negative := Subject (Low) = '-';
         Low := Low + 1;
      end if;
      if Low > High then
         return;
      end if;
      for I in Low .. High loop
         if Subject (I) not in '0' .. '9' then
            Value := 0;
            return;
         end if;
         Digit := Integer_64 (Character'Pos (Subject (I)) - Character'Pos ('0'));
         CCL.Checked_Arithmetic.Multiply (Value, DECIMAL_BASE, Scaled, Overflowed);
         if not Overflowed then
            if Negative then
               CCL.Checked_Arithmetic.Subtract (Scaled, Digit, Value, Overflowed);
            else
               CCL.Checked_Arithmetic.Add (Scaled, Digit, Value, Overflowed);
            end if;
         end if;
         if Overflowed then
            Value := 0;
            Status := Overflow;
            return;
         end if;
      end loop;
      Status := Done;
   end Parse_Integer;

   function Decimal_Image (Value : Integer_64) return String is
      Buffer : String (1 .. MAX_INTEGER_IMAGE) := [others => '0'];
      First : Positive range 2 .. MAX_INTEGER_IMAGE := MAX_INTEGER_IMAGE;
      Signed_First : Positive range 1 .. MAX_INTEGER_IMAGE;
      Magnitude : Unsigned_64;
      Digit : Unsigned_64 range 0 .. 9;
   begin
      Magnitude := (if Value < 0 then Unsigned_64 (-(Value + 1)) + 1
                    else Unsigned_64 (Value));
      --  Nineteen magnitude digits, plus a reserved sign position. The
      --  bounded reverse loop cannot underflow, even for Integer_64'First.
      for Position in reverse 2 .. MAX_INTEGER_IMAGE loop
         First := Position;
         Digit := Magnitude mod 10;
         Buffer (First) := Character'Val (Character'Pos ('0') + Natural (Digit));
         Magnitude := Magnitude / 10;
         exit when Magnitude = 0;
      end loop;
      Signed_First := First;
      if Value < 0 then
         Signed_First := First - 1;
         Buffer (Signed_First) := '-';
      end if;
      pragma Assert (Signed_First >= 1 and then Signed_First <= MAX_INTEGER_IMAGE);
      return Buffer (Signed_First .. MAX_INTEGER_IMAGE);
   end Decimal_Image;
   procedure Next_Piece
     (Subject, Separator : String; Position : in out Positive;
      Finished : in out Boolean; Low : out Positive; High : out Natural;
      Found : out Boolean)
   is
      Next : Natural;
   begin
      Low := 1; High := 0; Found := False;
      if Finished then
         return;
      elsif Separator'Length = 0 then
         while Position <= Subject'Length and then Is_Blank (Subject (Position)) loop
            pragma Loop_Variant (Increases => Position);
            Position := Position + 1;
         end loop;
         if Position > Subject'Length then
            Finished := True;
            return;
         end if;
         Low := Position;
         while Position <= Subject'Length and then not Is_Blank (Subject (Position)) loop
            pragma Loop_Invariant (Low <= Position);
            pragma Loop_Variant (Increases => Position);
            Position := Position + 1;
         end loop;
         High := Position - 1;
         Found := True;
      else
         Next := (if Position <= Subject'Length then Find (Subject, Separator, Position) else 0);
         Low := Position;
         Found := True;
         if Next = 0 then
            High := Subject'Length;
            Finished := True;
         else
            High := Next - 1;
            Position := Next + Separator'Length;
         end if;
      end if;
   end Next_Piece;

end CCL.Text_Operations;
