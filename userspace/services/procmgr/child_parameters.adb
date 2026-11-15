pragma Ada_2022;
with Interfaces; use Interfaces;

package body Child_Parameters with SPARK_Mode is

   --  The decimal image of Value, without Integer_64'Image's leading space.
   Maximum_Image : constant := 20;   --  "-9223372036854775808"
   procedure Image (Value : Integer_64; Text : out String; Length : out Natural)
     with Pre  => Text'First = 1 and then Text'Length = Maximum_Image,
          Post => Length <= Maximum_Image
   is
      Digits_Of : String (1 .. Maximum_Image) := [others => '0'];
      Count : Natural := 0;
      --  The magnitude, as an unsigned (Integer_64'First has none signed).
      Rest : Unsigned_64 :=
        (if Value >= 0 then Unsigned_64 (Value)
         else Unsigned_64 (-(Value + 1)) + 1);
   begin
      Text := [others => ' '];
      --  A magnitude of at most 2**63 has at most 19 digits.
      for Step in 1 .. Maximum_Image - 1 loop
         pragma Loop_Invariant (Count = Step - 1);
         Count := Step;
         Digits_Of (Count) := Character'Val (Character'Pos ('0') + Natural (Rest mod 10));
         Rest := Rest / 10;
         exit when Rest = 0;
      end loop;
      Length := 0;
      if Value < 0 then
         Length := 1;
         Text (1) := '-';
      end if;
      for K in reverse 1 .. Count loop
         pragma Loop_Invariant (Length = (if Value < 0 then 1 else 0) + (Count - K));
         Length := Length + 1;
         Text (Length) := Digits_Of (K);
      end loop;
   end Image;

   procedure Merge
     (Policy : String;
      Declared : Declared_Array; Declared_Total : Declared_Count;
      Facts : CS.Fact_Array; Fact_Total : CS.Fact_Count;
      Into : out String; Length : out Natural; Status : out Merge_Status)
   is
      Next : Natural := 0;
      Fits : Boolean := True;

      procedure Put (Text : String)
        with Pre  => Into'First = 1 and then Into'Last = Maximum_Merged_Bytes and then Next <= Maximum_Merged_Bytes,
             Post => Next <= Maximum_Merged_Bytes
      is
      begin
         if not Fits or else Text'Length > Maximum_Merged_Bytes - Next then
            Fits := False;
            return;
         end if;
         for K in Text'Range loop
            pragma Loop_Invariant (Next = Next'Loop_Entry + (K - Text'First));
            Next := Next + 1;
            Into (Next) := Text (K);
         end loop;
      end Put;
   begin
      Into := [others => ' '];
      Length := 0;
      if Policy'Length < 2 or else Policy (Policy'First) /= '('
        or else Policy (Policy'Last) /= ')'
      then
         Status := Policy_Not_Record;
         return;
      end if;
      for I in 1 .. Declared_Total loop
         if not CS.Valid_Field (Declared (I))
           or else (for some J in I + 1 .. Declared_Total => CS.Same (Declared (I), Declared (J)))
         then
            Status := Declaration_Invalid;
            return;
         end if;
      end loop;
      for I in 1 .. Fact_Total loop
         if not Declares (Declared, Declared_Total, Facts (I).Field) then
            Status := Fact_Not_Declared;
            return;
         end if;
      end loop;
      for I in 1 .. Declared_Total loop
         if not Given (Facts, Fact_Total, Declared (I)) then
            Status := Fact_Missing;
            return;
         end if;
      end loop;
      pragma Assert (Same_Facts (Declared, Declared_Total, Facts, Fact_Total));

      --  The policy without its closing parenthesis, each fact as a named
      --  association, then the parenthesis.
      Put (Policy (Policy'First .. Policy'Last - 1));
      for I in 1 .. Fact_Total loop
         pragma Loop_Invariant (Next <= Maximum_Merged_Bytes);
         declare
            Field : CS.Field_Text renames Facts (I).Field;
            Number : String (1 .. Maximum_Image);
            Digits_Length : Natural;
         begin
            Image (Facts (I).Value, Number, Digits_Length);
            Put (" ");
            Put (Field.Data (1 .. Field.Length));
            Put (" => ");
            Put (Number (1 .. Digits_Length));
         end;
      end loop;
      Put (")");
      --  (Fits, the first and the last character are what was put; the
      --  check states it for the postcondition.)
      if not Fits or else Next < 2 or else Into (1) /= '(' or else Into (Next) /= ')' then
         Into := [others => ' '];
         Status := Too_Long;
         return;
      end if;
      Length := Next;
      Status := Merged;
   end Merge;
   function Blank (C : Character) return Boolean is
     (C in ' ' | ASCII.HT | ASCII.LF | ASCII.CR);
   function Name_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_');

   procedure Policy_Schema
     (Signature, Root : String;
      Facts : CS.Fact_Array; Fact_Total : CS.Fact_Count;
      Into : out String; Length : out Natural; Status : out Schema_Status)
   is
      Type_Word : constant String := "(type";
      Record_Word : constant String := "(record";
      --  Both texts, 1-based and of bounded length, so every index below is
      --  small.
      N : constant Natural range 0 .. Maximum_Signature_Bytes := Signature'Length;
      S : constant String (1 .. N) := Signature;
      RN : constant Positive range 1 .. Maximum_Root_Bytes := Root'Length;
      R : constant String (1 .. RN) := Root;
      subtype Position is Positive range 1 .. Maximum_Signature_Bytes + 1;
      Next : Natural := 0;
      Fits : Boolean := True;
      Fields_First : Natural := 0;   --  just past the root's "(record"
      Cursor : Position;
      Used : array (1 .. CS.Maximum_Facts) of Boolean := [others => False];

      procedure Put (Text : String)
        with Pre  => Into'First = 1 and then Into'Last = Maximum_Schema_Bytes and then Next <= Maximum_Schema_Bytes,
             Post => Next <= Maximum_Schema_Bytes
      is
      begin
         if not Fits or else Text'Length > Maximum_Schema_Bytes - Next then
            Fits := False;
            return;
         end if;
         for K in Text'Range loop
            pragma Loop_Invariant (Next = Next'Loop_Entry + (K - Text'First));
            Next := Next + 1;
            Into (Next) := Text (K);
         end loop;
      end Put;

      --  Past the blanks from From (at most N + 1).
      function Past_Blanks (From : Position) return Position
        with Pre  => From <= N + 1,
             Post => Past_Blanks'Result in From .. N + 1
      is
         P : Position := From;
      begin
         while P <= N and then Blank (S (P)) loop
            pragma Loop_Invariant (P in From .. N);
            pragma Loop_Variant (Increases => P);
            P := P + 1;
         end loop;
         return P;
      end Past_Blanks;

      --  Whether Text is at P in S.
      function At_Text (P : Position; Text : String) return Boolean is
        (Text'Length <= Maximum_Signature_Bytes and then P <= N and then Text'Length <= N - P + 1
         and then S (P .. P + Text'Length - 1) = Text);

      --  "(type" blanks Root blanks "(record" at P: Fields_At is just past it.
      procedure Root_At (P : Position; Found : out Boolean; Fields_At : out Natural)
        with Pre  => P <= N,
             Post => (if Found then Fields_At in P + 1 .. N + 1)
      is
         Q : Position;
      begin
         Found := False;
         Fields_At := 0;
         if not At_Text (P, Type_Word) then
            return;
         end if;
         Q := P + Type_Word'Length;
         if Q > N or else not Blank (S (Q)) then
            return;
         end if;
         Q := Past_Blanks (Q);
         if not At_Text (Q, R) then
            return;
         end if;
         Q := Q + RN;
         if Q > N or else not Blank (S (Q)) then
            return;
         end if;
         Q := Past_Blanks (Q);
         if not At_Text (Q, Record_Word) then
            return;
         end if;
         Found := True;
         Fields_At := Q + Record_Word'Length;
      end Root_At;

      --  The parenthesis closing the form that opens at From; 0 when the
      --  signature ends first. Strings (with \ escapes) may hold parentheses.
      function Group_End (From : Position) return Natural
        with Pre  => From <= N,
             Post => Group_End'Result = 0 or else Group_End'Result in From + 1 .. N
      is
         Depth : Natural := 0;
         Quoted, Escaped : Boolean := False;
      begin
         for P in From .. N loop
            pragma Loop_Invariant (Depth <= P - From);
            if Quoted then
               if Escaped then
                  Escaped := False;
               elsif S (P) = '\' then
                  Escaped := True;
               elsif S (P) = '"' then
                  Quoted := False;
               end if;
            elsif S (P) = '"' then
               Quoted := True;
            elsif S (P) = '(' then
               Depth := Depth + 1;
            elsif S (P) = ')' then
               if Depth <= 1 then
                  return (if P > From then P else 0);
               end if;
               Depth := Depth - 1;
            end if;
         end loop;
         return 0;
      end Group_End;

      --  The fact a field named S (First .. Last) is, or 0.
      function Fact_Named (First, Last : Position) return Natural
        with Pre  => First <= Last and then Last <= N,
             Post => Fact_Named'Result <= Fact_Total
      is
      begin
         for I in 1 .. Fact_Total loop
            if Facts (I).Field.Length = Last - First + 1
              and then Facts (I).Field.Data (1 .. Facts (I).Field.Length) = S (First .. Last)
            then
               return I;
            end if;
         end loop;
         return 0;
      end Fact_Named;

      procedure Refuse (Why : Schema_Status) is
      begin
         Into := [others => ' '];
         Length := 0;
         Status := Why;
      end Refuse;
   begin
      Into := [others => ' '];
      Status := Built;
      --  The last declaration of Root as a record (a signature declares each
      --  type once, the root last).
      for P in 1 .. N loop
         pragma Loop_Invariant (Fields_First = 0 or else Fields_First in 2 .. N + 1);
         declare
            Found : Boolean;
            Fields_At : Natural;
         begin
            Root_At (P, Found, Fields_At);
            if Found then
               Fields_First := Fields_At;
            end if;
         end;
      end loop;
      if Fields_First = 0 then
         Refuse (Root_Not_Record);
         return;
      end if;
      Put (S (1 .. Fields_First - 1));
      Cursor := Fields_First;
      --  Each field form; the root's closing parenthesis ends the list.
      loop
         pragma Loop_Invariant (Cursor in Fields_First .. N + 1 and then Next <= Maximum_Schema_Bytes);
         pragma Loop_Variant (Increases => Cursor);
         Cursor := Past_Blanks (Cursor);
         if Cursor > N then
            Refuse (Root_Not_Record);
            return;
         end if;
         exit when S (Cursor) = ')';
         if S (Cursor) /= '(' or else Cursor = N then
            Refuse (Root_Not_Record);
            return;
         end if;
         declare
            Close : constant Natural := Group_End (Cursor);
            Name_First : constant Position := Past_Blanks (Cursor + 1);
            Name_Last : Position;
            Fact : Natural;
         begin
            if Close = 0 or else Name_First > N or else not Name_Character (S (Name_First)) then
               Refuse (Root_Not_Record);
               return;
            end if;
            Name_Last := Name_First;
            while Name_Last < N and then Name_Character (S (Name_Last + 1)) loop
               pragma Loop_Invariant (Name_Last in Name_First .. N - 1);
               pragma Loop_Variant (Increases => Name_Last);
               Name_Last := Name_Last + 1;
            end loop;
            Fact := Fact_Named (Name_First, Name_Last);
            if Fact = 0 then
               Put (" ");
               Put (S (Cursor .. Close));
            elsif Used (Fact) then
               Refuse (Fact_Not_Field);
               return;
            else
               Used (Fact) := True;
            end if;
            Cursor := Close + 1;
         end;
      end loop;
      Put (S (Cursor .. N));
      for I in 1 .. Fact_Total loop
         if not Used (I) then
            Refuse (Fact_Not_Field);
            return;
         end if;
      end loop;
      Put (" (type " & Launch_Type & " (record (facts (List String)) (policy ");
      Put (R);
      Put (")))");
      if not Fits then
         Refuse (Schema_Too_Long);
         return;
      end if;
      Length := Next;
   end Policy_Schema;

   procedure Read_Launch
     (Canonical : String; Facts : CS.Fact_Array; Fact_Total : CS.Fact_Count;
      Policy_First, Policy_Last : out Natural; Status : out Read_Status)
   is
      Prefix : constant String := "(" & Launch_Type & " facts => [";
      Middle : constant String := "] policy => ";
      P : Natural;
      Listed : Natural := 0;
      Seen : array (1 .. CS.Maximum_Facts) of Boolean := [others => False];
   begin
      Policy_First := 0;
      Policy_Last := 0;
      Status := Not_Canonical;
      if Canonical'Length <= Prefix'Length + Middle'Length + 2
        or else Canonical (Canonical'First .. Canonical'First + Prefix'Length - 1) /= Prefix
        or else Canonical (Canonical'Last) /= ')'
      then
         return;
      end if;
      P := Canonical'First + Prefix'Length;
      --  "a" "b" ... up to the closing bracket.
      loop
         pragma Loop_Invariant (P in Canonical'First + Prefix'Length .. Canonical'Last and then Listed <= P);
         exit when Canonical (P) = ']';
         if Listed > 0 then
            if Canonical (P) /= ' ' or else P = Canonical'Last then
               return;
            end if;
            P := P + 1;
         end if;
         if Canonical (P) /= '"' or else P = Canonical'Last then
            return;
         end if;
         P := P + 1;
         declare
            First : constant Positive := P;
            Fact : Natural := 0;
         begin
            while Name_Character (Canonical (P)) loop
               pragma Loop_Invariant (P in First .. Canonical'Last);
               if P = Canonical'Last then
                  return;
               end if;
               P := P + 1;
            end loop;
            if Canonical (P) /= '"' or else P = First or else P = Canonical'Last then
               return;
            end if;
            for I in 1 .. Fact_Total loop
               if Facts (I).Field.Length = P - First
                 and then Facts (I).Field.Data (1 .. Facts (I).Field.Length) = Canonical (First .. P - 1)
               then
                  Fact := I;
               end if;
            end loop;
            if Fact = 0 or else Seen (Fact) then
               Status := Facts_Differ;
               return;
            end if;
            Seen (Fact) := True;
         end;
         Listed := Listed + 1;
         P := P + 1;
      end loop;
      if Middle'Length > Canonical'Last - P
        or else Canonical (P .. P + Middle'Length - 1) /= Middle
      then
         return;
      end if;
      P := P + Middle'Length;
      if P >= Canonical'Last - 1 or else Canonical (P) /= '(' or else Canonical (Canonical'Last - 1) /= ')' then
         return;
      end if;
      for I in 1 .. Fact_Total loop
         if not Seen (I) then
            Status := Facts_Differ;
            return;
         end if;
      end loop;
      Policy_First := P;
      Policy_Last := Canonical'Last - 1;
      Status := Read;
   end Read_Launch;
end Child_Parameters;
