package body Text_Search with SPARK_Mode is
   function Fold (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);
   function Is_Word (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' or else Character'Pos (C) >= 16#80#);

   function Same (A, B : Character; Case_Sensitive : Boolean) return Boolean is
     (if Case_Sensitive then A = B else Fold (A) = Fold (B));

   function Matches_At (Text, Pattern : String; At_Index : Positive; Settings : Options) return Boolean is
     (for all K in 0 .. Pattern'Length - 1 =>
        Same (Text (At_Index + K), Pattern (Pattern'First + K), Settings.Case_Sensitive))
     with Pre => Pattern'Length in 1 .. MAXIMUM_PATTERN and then At_Index >= Text'First
                 and then At_Index <= Text'Last and then Pattern'Length - 1 <= Text'Last - At_Index;

   function Word_Edges
     (Text : String; Length : Positive; At_Index : Positive; Before, After : Character) return Boolean
   is
     ((if At_Index > Text'First then not Is_Word (Text (At_Index - 1)) else not Is_Word (Before))
      and then (if At_Index + Length <= Text'Last then not Is_Word (Text (At_Index + Length))
                else not Is_Word (After)))
     with Pre => At_Index >= Text'First and then Length <= MAXIMUM_PATTERN
                 and then Text'Last < Positive'Last - MAXIMUM_PATTERN and then At_Index <= Text'Last;

   procedure Find_Literal
     (Text, Pattern : String; From : Positive; Settings : Options; Before, After : Character;
      Found : out Boolean; At_Index : out Positive)
   is
      Start : constant Positive := Positive'Max (From, Text'First);
   begin
      Found := False;
      At_Index := Start;
      if Text'Length < Pattern'Length or else Start > Text'Last - Pattern'Length + 1 then
         return;
      end if;
      for K in Start .. Text'Last - Pattern'Length + 1 loop
         if Same (Text (K), Pattern (Pattern'First), Settings.Case_Sensitive)
           and then Matches_At (Text, Pattern, K, Settings)
           and then (not Settings.Whole_Word or else Word_Edges (Text, Pattern'Length, K, Before, After))
         then
            Found := True;
            At_Index := K;
            return;
         end if;
      end loop;
   end Find_Literal;

   procedure Find_Literal_Backward
     (Text, Pattern : String; Upto : Natural; Settings : Options; Before, After : Character;
      Found : out Boolean; At_Index : out Positive)
   is
   begin
      Found := False;
      At_Index := Text'First;
      if Text'Length < Pattern'Length or else Upto < Text'First + Pattern'Length - 1 then
         return;
      end if;
      for K in reverse Text'First .. Natural'Min (Upto, Text'Last) - Pattern'Length + 1 loop
         if Matches_At (Text, Pattern, K, Settings)
           and then (not Settings.Whole_Word or else Word_Edges (Text, Pattern'Length, K, Before, After))
         then
            Found := True;
            At_Index := K;
            return;
         end if;
      end loop;
   end Find_Literal_Backward;

   ---------------------------------------------------------------------------
   --  Compiling.

   MAXIMUM_DEPTH : constant := 32;

   procedure Compile (Pattern : String; Case_Sensitive : Boolean; Result : out Program; Status : out Compile_Result)
   is
      --  A 1-based copy whose bounds every nested procedure knows.
      Last : constant Pattern_Length := Pattern'Length;
      Source : constant String (1 .. Last) := Pattern;
      subtype Cursor is Positive range 1 .. MAXIMUM_PATTERN + 3;
      P : Cursor := 1;   --  the next pattern character
      Failed : Boolean := False;

      function At_End return Boolean is (P > Last);
      function Peek return Character is (if P <= Last then Source (P) else ASCII.NUL);

      procedure Fail (Why : Compile_Result) is
      begin
         if not Failed then
            Failed := True;
            Status := Why;
         end if;
      end Fail;

      procedure Emit (Item : Instruction) is
      begin
         if Result.Length >= MAXIMUM_PROGRAM then
            Fail (Too_Long);
            return;
         end if;
         Result.Length := Result.Length + 1;
         Result.Code (Result.Length) := Item;
      end Emit;

      --  Put Item at Position, the new start of the atom there, moving the
      --  atom's code. A target at Position from code before it now reaches
      --  Item (the atom's start); targets inside the atom, and beyond,
      --  follow their instructions.
      procedure Insert_At (Position : Program_Index; Item : Instruction) is
         function Moved (Target : Program_Index; From : Program_Index) return Program_Index is
           (if Target < MAXIMUM_PROGRAM and then (Target > Position or else (Target = Position and then From >= Position))
            then Target + 1 else Target);
      begin
         if Result.Length >= MAXIMUM_PROGRAM or else Position > Result.Length + 1 then
            Fail (Too_Long);
            return;
         end if;
         for K in 1 .. Result.Length loop
            if Result.Code (K).Op in Split | Jump then
               Result.Code (K).X := Moved (Result.Code (K).X, K);
               if Result.Code (K).Op = Split then
                  Result.Code (K).Y := Moved (Result.Code (K).Y, K);
               end if;
            end if;
         end loop;
         if Position <= Result.Length then
            Result.Code (Position + 1 .. Result.Length + 1) := Result.Code (Position .. Result.Length);
         end if;
         Result.Code (Position) := Item;
         Result.Length := Result.Length + 1;
      end Insert_At;

      function Next_Index return Program_Index is
        (if Result.Length < MAXIMUM_PROGRAM then Result.Length + 1 else MAXIMUM_PROGRAM);

      procedure Add_Range (Set : in out Byte_Set; Low, High : Character) is
      begin
         for C in Low .. High loop
            Set (C) := True;
            if Result.Fold then
               if C in 'a' .. 'z' then
                  Set (Character'Val (Character'Pos (C) - 32)) := True;
               elsif C in 'A' .. 'Z' then
                  Set (Character'Val (Character'Pos (C) + 32)) := True;
               end if;
            end if;
         end loop;
      end Add_Range;

      --  \d \w \s and their negations into Set; False if E is none of them.
      function Named_Class (E : Character) return Boolean is (E in 'd' | 'w' | 's' | 'D' | 'W' | 'S');
      procedure Add_Named (Set : in out Byte_Set; E : Character) is
         Named : Byte_Set := [others => False];
      begin
         case E is
            when 'd' | 'D' => Named ('0' .. '9') := [others => True];
            when 'w' | 'W' =>
               Named ('a' .. 'z') := [others => True];
               Named ('A' .. 'Z') := [others => True];
               Named ('0' .. '9') := [others => True];
               Named ('_') := True;
               for C in Character'Val (16#80#) .. Character'Last loop
                  Named (C) := True;
               end loop;
            when others =>
               Named (' ') := True;
               Named (ASCII.HT) := True;
               Named (ASCII.CR) := True;
               Named (ASCII.FF) := True;
               Named (ASCII.VT) := True;
         end case;
         for C in Character loop
            if (E in 'D' | 'W' | 'S') /= Named (C) then
               Set (C) := True;
            end if;
         end loop;
      end Add_Named;

      function Escaped (E : Character) return Character is
        (case E is when 't' => ASCII.HT, when 'n' => ASCII.LF, when 'r' => ASCII.CR, when others => E);

      procedure New_Class (Set : Byte_Set) is
      begin
         if Result.Class_Used >= MAXIMUM_CLASSES then
            Fail (Too_Long);
            return;
         end if;
         Result.Class_Used := Result.Class_Used + 1;
         Result.Classes (Result.Class_Used) := Set;
         Emit ((Op => In_Class, Class => Result.Class_Used, others => <>));
      end New_Class;

      procedure Parse_Class with Pre => P <= Last + 1, Post => P <= Last + 1 is
         Set : Byte_Set := [others => False];
         Negate : Boolean := False;
         First : Boolean := True;
         Low : Character;
      begin
         --  P is just after '['.
         if Peek = '^' then
            Negate := True;
            P := P + 1;
         end if;
         loop
            pragma Loop_Invariant (P <= Last + 1);
            if At_End then
               Fail (Bad_Class);
               return;
            end if;
            exit when Peek = ']' and then not First;
            First := False;
            Low := Peek;
            P := P + 1;
            if Low = '\' and then At_End then
               Fail (Bad_Class);
               return;
            end if;
            if Low = '\' and then Named_Class (Peek) then
               Add_Named (Set, Peek);
               P := P + 1;
            else
            if Low = '\' then
               Low := Escaped (Peek);
               P := P + 1;
            end if;
            if Peek = '-' and then P < Last and then Source (P + 1) /= ']' then
               declare
                  High : Character := Source (P + 1);
               begin
                  P := P + 2;
                  if High = '\' then
                     if At_End then
                        Fail (Bad_Class);
                        return;
                     end if;
                     High := Escaped (Peek);
                     P := P + 1;
                  end if;
                  if High < Low then
                     Fail (Bad_Class);
                     return;
                  end if;
                  Add_Range (Set, Low, High);
               end;
            else
               Add_Range (Set, Low, Low);
            end if;
            end if;
         end loop;
         P := P + 1;   --  the ']'
         if Negate then
            for C in Character loop
               Set (C) := not Set (C) and then C /= ASCII.LF;
            end loop;
         end if;
         New_Class (Set);
      end Parse_Class;

      procedure Parse_Alternation (Depth : Natural) with Pre => P <= Last + 1, Post => P <= Last + 1;

      --  One atom, then its quantifier.
      procedure Parse_Repeat (Depth : Natural) with Pre => P <= Last, Post => P <= Last + 1 is
         Start : constant Program_Index := Next_Index;
         C : constant Character := Peek;
      begin
         P := P + 1;
         case C is
            when '.' => Emit ((Op => Any, others => <>));
            when '^' => Emit ((Op => Line_Start, others => <>));
            when '$' => Emit ((Op => Line_End, others => <>));
            when '[' => Parse_Class;
            when '(' =>
               if Depth = 0 then
                  Fail (Too_Long);
                  return;
               end if;
               Parse_Alternation (Depth - 1);
               if Peek /= ')' then
                  Fail (Unbalanced);
                  return;
               end if;
               P := P + 1;
            when ')' => Fail (Unbalanced); return;
            when '*' | '+' | '?' => Fail (Bad_Repeat); return;
            when '\' =>
               if At_End then
                  Fail (Bad_Repeat);
                  return;
               end if;
               declare
                  E : constant Character := Peek;
               begin
                  P := P + 1;
                  if E = 'b' then
                     Emit ((Op => Word_Boundary, others => <>));
                  elsif Named_Class (E) then
                     declare
                        Set : Byte_Set := [others => False];
                     begin
                        Add_Named (Set, E);
                        New_Class (Set);
                     end;
                  else
                     Emit ((Op => Literal, Value => (if Result.Fold then Fold (Escaped (E)) else Escaped (E)),
                            others => <>));
                  end if;
               end;
            when others =>
               Emit ((Op => Literal, Value => (if Result.Fold then Fold (C) else C), others => <>));
         end case;
         if Failed or else At_End or else Peek not in '*' | '+' | '?' then
            return;
         end if;
         if Start >= MAXIMUM_PROGRAM then
            Fail (Too_Long);
            return;
         end if;
         declare
            Q : constant Character := Peek;
            Lazy : Boolean := False;
         begin
            P := P + 1;
            if Peek = '?' then
               Lazy := True;
               P := P + 1;
            end if;
            case Q is
               when '*' =>
                  --  L: split body, out; body; jump L; out:
                  Insert_At (Start, (Op => Split, X => Start + 1, Y => Start + 1, others => <>));
                  Emit ((Op => Jump, X => Start, others => <>));
                  if not Failed then
                     Result.Code (Start).Y := Next_Index;
                     if Lazy then
                        Result.Code (Start).X := Next_Index;
                        Result.Code (Start).Y := Start + 1;
                     end if;
                  end if;
               when '+' =>
                  --  body; split body, out
                  Emit ((Op => Split, X => Start, Y => Start, others => <>));
                  if not Failed then
                     Result.Code (Result.Length).Y := Next_Index;
                     if Lazy then
                        Result.Code (Result.Length).X := Next_Index;
                        Result.Code (Result.Length).Y := Start;
                     end if;
                  end if;
               when others =>
                  --  split body, out; body; out:
                  Insert_At (Start, (Op => Split, X => Start + 1, Y => Start + 1, others => <>));
                  if not Failed then
                     Result.Code (Start).Y := Next_Index;
                     if Lazy then
                        Result.Code (Start).X := Next_Index;
                        Result.Code (Start).Y := Start + 1;
                     end if;
                  end if;
            end case;
            if not At_End and then Peek in '*' | '+' | '?' then
               Fail (Bad_Repeat);
            end if;
         end;
      end Parse_Repeat;

      procedure Parse_Concat (Depth : Natural) with Pre => P <= Last + 1, Post => P <= Last + 1 is
      begin
         while not Failed and then not At_End and then Peek not in '|' | ')' loop
            Parse_Repeat (Depth);
         end loop;
      end Parse_Concat;

      procedure Parse_Alternation (Depth : Natural) is
         Start : constant Program_Index := Next_Index;
      begin
         Parse_Concat (Depth);
         while not Failed and then not At_End and then Peek = '|' loop
            if Start >= MAXIMUM_PROGRAM then
               Fail (Too_Long);
               exit;
            end if;
            P := P + 1;
            Insert_At (Start, (Op => Split, X => Start + 1, Y => Start + 1, others => <>));
            Emit ((Op => Jump, X => 1, others => <>));
            exit when Failed;
            declare
               Jump_At : constant Program_Index := Result.Length;
            begin
               Result.Code (Start).Y := Next_Index;
               Parse_Concat (Depth);
               exit when Failed;
               Result.Code (Jump_At).X := Next_Index;
            end;
         end loop;
      end Parse_Alternation;
   begin
      Result := (Fold => not Case_Sensitive, others => <>);
      Status := Compiled;
      if Source'Length = 0 then
         Status := Empty_Pattern;
         return;
      end if;
      Parse_Alternation (MAXIMUM_DEPTH);
      if not Failed and then not At_End then
         Fail (Unbalanced);
      end if;
      Emit ((Op => Matched, others => <>));
      if Failed then
         Result.Length := 0;
      end if;
   end Compile;

   ---------------------------------------------------------------------------
   --  Matching.

   procedure Match
     (Item : Program; Line : String; From : Positive; Found : out Boolean; First : out Positive;
      Length : out Natural)
   is
      type Thread is record
         PC : Program_Index := 1;
         Start : Positive := 1;
      end record;
      --  Sized by this program, not the largest: a line costs O(its length
      --  times the program), never the maximum program's set-up.
      N : constant Program_Count := Item.Length;
      Text : constant String (1 .. Line'Length) := Line;
      LIST_SIZE : constant Program_Index := Program_Index'Max (N, 1);
      STACK_SIZE : constant Positive := 2 * LIST_SIZE + 1;
      type Thread_List is array (1 .. LIST_SIZE) of Thread;
      type Mark_Array is array (1 .. LIST_SIZE) of Natural;
      type Stack_Array is array (1 .. STACK_SIZE) of Thread;

      Current, Following : Thread_List := [others => (others => <>)];
      Current_Count, Following_Count : Program_Count := 0;
      Marks : Mark_Array := [others => 0];
      Generation : Natural := 0;
      Stack : Stack_Array := [others => (others => <>)];
      Last : constant Natural := Line'Length;

      --  Add the closure of PC (taking Split, Jump and assertions at
      --  Position) to List.
      procedure Add (List : in out Thread_List; Count : in out Program_Count; PC : Program_Index;
                     Start, Position : Positive)
        with Pre => Count <= LIST_SIZE, Post => Count <= LIST_SIZE
      is
         Top : Natural := 1;
      begin
         Stack (1) := (PC, Start);
         while Top > 0 loop
            pragma Loop_Invariant (Top in 1 .. STACK_SIZE and then Count <= LIST_SIZE);
            declare
               T : constant Thread := Stack (Top);
            begin
               Top := Top - 1;
               if T.PC <= N and then Marks (T.PC) /= Generation then
                  Marks (T.PC) := Generation;
                  declare
                     I : constant Instruction := Item.Code (T.PC);
                     Before : constant Character := (if Position > 1 and then Position - 1 <= Last
                                                     then Text (Position - 1) else ' ');
                     At_Char : constant Character := (if Position <= Last then Text (Position) else ' ');
                  begin
                     case I.Op is
                        when Jump =>
                           if Top < STACK_SIZE then
                              Top := Top + 1;
                              Stack (Top) := (I.X, T.Start);
                           end if;
                        when Split =>
                           if Top < STACK_SIZE - 1 then
                              Stack (Top + 1) := (I.Y, T.Start);
                              Stack (Top + 2) := (I.X, T.Start);
                              Top := Top + 2;
                           end if;
                        when Line_Start | Line_End | Word_Boundary =>
                           if (case I.Op is
                                  when Line_Start => Position = 1,
                                  when Line_End => Position > Last,
                                  when others => Is_Word (Before) /= Is_Word (At_Char)
                                                   and then (Position > 1 or else Position <= Last))
                             and then T.PC < N and then Top < STACK_SIZE
                           then
                              Top := Top + 1;
                              Stack (Top) := (T.PC + 1, T.Start);
                           end if;
                        when Literal | Any | In_Class | Matched =>
                           if Count < LIST_SIZE then
                              Count := Count + 1;
                              List (Count) := T;
                           end if;
                     end case;
                  end;
               end if;
            end;
         end loop;
      end Add;

      Position : Positive := Positive'Max (From, 1);
   begin
      Found := False;
      First := 1;
      Length := 0;
      if N = 0 or else Position > Last + 1 then
         return;
      end if;
      Generation := 1;
      loop
         pragma Loop_Invariant
           (Current_Count <= LIST_SIZE and then Position in 1 .. Last + 1
            and then Generation <= Position - Position'Loop_Entry + 1
            and then First in 1 .. Last + 1 and then Length <= Last + 1 - First);
         --  Current was built under this generation: a seed joins it.
         if not Found then
            Add (Current, Current_Count, 1, Position, Position);
         end if;
         if Current_Count = 0 and then Found then
            exit;
         end if;
         Generation := Generation + 1;
         Following_Count := 0;
         for K in 1 .. Current_Count loop
            pragma Loop_Invariant (Following_Count <= LIST_SIZE and then Current_Count <= LIST_SIZE
                                   and then First in 1 .. Last + 1 and then Length <= Last + 1 - First);
            declare
               T : constant Thread := Current (K);
               I : constant Instruction := Item.Code (T.PC);
               C : constant Character := (if Position <= Last then Text (Position) else ASCII.LF);
               Advance : Boolean := False;
            begin
               case I.Op is
                  when Matched =>
                     --  A thread's start is where it was seeded: in the line,
                     --  not after Position.
                     Found := True;
                     First := (if T.Start in 1 .. Position then T.Start else Position);
                     Length := Position - First;
                     exit;   --  lower-priority threads are cut
                  when Literal =>
                     Advance := Position <= Last and then (if Item.Fold then Fold (C) = I.Value else C = I.Value);
                  when Any =>
                     Advance := Position <= Last;
                  when In_Class =>
                     Advance := Position <= Last and then Item.Classes (I.Class) (C);
                  when others =>
                     null;
               end case;
               if Advance and then T.PC < N then
                  Add (Following, Following_Count, T.PC + 1, T.Start, Position + 1);
               end if;
            end;
         end loop;
         exit when Position > Last;
         Position := Position + 1;
         Current := Following;
         Current_Count := Following_Count;
         exit when Current_Count = 0 and then Found;
      end loop;
   end Match;
end Text_Search;
