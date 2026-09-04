package body CuBit.UI.Editor.Search with SPARK_Mode is
   subtype Pattern_Offset is Natural range 0 .. MAX_PATTERN_LENGTH - 1;
   type Prefix_Table is array (Pattern_Offset) of Natural
     range 0 .. MAX_PATTERN_LENGTH;

   function Fold_ASCII (Value : Character) return Character is
     (if Value >= 'A' and then Value <= 'Z' then
         Character'Val
           (Character'Pos (Value) + Character'Pos ('a') -
              Character'Pos ('A'))
      else Value);

   function Is_Word_Character (Value : Character) return Boolean is
     ((Value >= 'a' and then Value <= 'z') or else
      (Value >= 'A' and then Value <= 'Z') or else
      (Value >= '0' and then Value <= '9') or else Value = '_');

   procedure Find_Next
     (Text, Pattern : String;
      Start_At : CuBit.UI.Editor.Documents.Document_Position;
      Wrap : Boolean;
      Whole_Word : Boolean;
      Case_Sensitive : Boolean;
      Result : out Search_Result)
   is
      Prefix : Prefix_Table := [others => 0];
      Pattern_Data : String (1 .. MAX_PATTERN_LENGTH) := [others => ' '];
      Pattern_Length : constant Natural := Pattern'Length;
      Matched : Natural range 0 .. MAX_PATTERN_LENGTH := 0;

      function Equal_Characters (Left, Right : Character) return Boolean is
        (if Case_Sensitive then Left = Right
         else Fold_ASCII (Left) = Fold_ASCII (Right));

      function Valid_Word_Boundaries
        (First_Offset : Natural) return Boolean
      is
         Last_Offset : constant Natural := First_Offset + Pattern_Length;
      begin
         return not Whole_Word or else
           ((First_Offset = 0 or else
             not Is_Word_Character
               (Text (Text'First + First_Offset - 1))) and then
            (Last_Offset = Text'Length or else
             not Is_Word_Character
               (Text (Text'First + Last_Offset))));
      end Valid_Word_Boundaries;

      procedure Search_Range (First_Start, Last_Start : Natural)
      is
         Scan_Last : Natural;
         Match_Offset : Natural;
         Processed : Natural := 0;
      begin
         Matched := 0;
         if First_Start > Last_Start then
            return;
         end if;
         Scan_Last := Last_Start + Pattern_Length - 1;
         for Text_Offset in First_Start .. Scan_Last loop
            pragma Loop_Invariant (Matched < Pattern_Length);
            pragma Loop_Invariant (Matched <= Processed);
            pragma Loop_Invariant
              (Processed = Text_Offset - First_Start);
            pragma Loop_Invariant
              (for all Offset in 0 .. Pattern_Length - 1 =>
                 Prefix (Offset) <= Offset);
            while Matched > 0 and then
              not Equal_Characters
                (Text (Text'First + Text_Offset),
                 Pattern_Data (Matched + 1))
            loop
               pragma Loop_Invariant (Matched <= Processed);
               pragma Loop_Invariant (Matched < Pattern_Length);
               pragma Loop_Invariant
                 (for all Offset in 0 .. Pattern_Length - 1 =>
                    Prefix (Offset) <= Offset);
               Matched := Prefix (Matched - 1);
            end loop;
            if Equal_Characters
              (Text (Text'First + Text_Offset),
               Pattern_Data (Matched + 1))
            then
               Matched := Matched + 1;
            end if;
            Processed := Processed + 1;
            pragma Assert (Matched <= Processed);
            if Matched = Pattern_Length then
               Match_Offset := First_Start + (Processed - Pattern_Length);
               if Valid_Word_Boundaries (Match_Offset) then
                  Result :=
                    (Status => Match_Found,
                     First => Match_Offset + 1,
                     Last => Match_Offset + Pattern_Length + 1);
                  return;
               end if;
               Matched := Prefix (Pattern_Length - 1);
            end if;
         end loop;
      end Search_Range;

      Last_Start : Natural;
      Start_Offset : constant Natural := Start_At - 1;
   begin
      Result := (others => <>);
      if Pattern_Length = 0 then
         Result.Status := Empty_Pattern;
         return;
      elsif Pattern_Length > MAX_PATTERN_LENGTH then
         Result.Status := Pattern_Too_Long;
         return;
      elsif Pattern_Length > Text'Length then
         return;
      end if;

      Pattern_Data (1 .. Pattern_Length) := Pattern;

      if Pattern_Length > 1 then
         for Offset in 1 .. Pattern_Length - 1 loop
            pragma Loop_Invariant (Matched < Offset);
            pragma Loop_Invariant
              (for all Previous in 0 .. Offset - 1 =>
                 Prefix (Previous) <= Previous);
            while Matched > 0 and then
              not Equal_Characters
                (Pattern_Data (Offset + 1),
                 Pattern_Data (Matched + 1))
            loop
               Matched := Prefix (Matched - 1);
            end loop;
            if Equal_Characters
              (Pattern_Data (Offset + 1),
               Pattern_Data (Matched + 1))
            then
               Matched := Matched + 1;
            end if;
            Prefix (Offset) := Matched;
         end loop;
      end if;
      pragma Assert
        (for all Offset in 0 .. Pattern_Length - 1 =>
           Prefix (Offset) <= Offset);

      Last_Start := Text'Length - Pattern_Length;
      if Start_Offset <= Last_Start then
         Search_Range (Start_Offset, Last_Start);
         if Result.Status = Match_Found then
            return;
         end if;
      end if;
      if Wrap and then Start_Offset > 0 then
         Search_Range
           (0, Natural'Min (Last_Start, Start_Offset - 1));
      end if;
   end Find_Next;
end CuBit.UI.Editor.Search;
