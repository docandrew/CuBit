pragma Ada_2022;
package body Agent_Json with SPARK_Mode is
   use Interfaces;

   First_Printable : constant := 16#20#;
   Replacement : constant := 16#FFFD#;
   First_High_Surrogate : constant := 16#D800#;
   Last_High_Surrogate : constant := 16#DBFF#;
   First_Low_Surrogate : constant := 16#DC00#;
   Last_Low_Surrogate : constant := 16#DFFF#;
   Supplementary_Base : constant := 16#1_0000#;
   Surrogate_Bits : constant := 2**10;

   function Is_Space (C : Character) return Boolean is
     (C in ' ' | ASCII.HT | ASCII.LF | ASCII.CR);
   function Is_Digit (C : Character) return Boolean is (C in '0' .. '9');
   function Hex_Value (C : Character) return Integer is
     (case C is
         when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
         when 'a' .. 'f' => Character'Pos (C) - Character'Pos ('a') + 10,
         when 'A' .. 'F' => Character'Pos (C) - Character'Pos ('A') + 10,
         when others => -1);

   procedure Scan_String (Text : String; At_Index : Positive; Next : out Positive; OK : out Boolean)
   with Pre => At_Index in Text'Range and then Text'Last < Positive'Last,
        Post => Next in At_Index + 1 .. Text'Last + 1 and then
                (if OK then Next >= At_Index + 2 and then Text (Next - 1) = '"')
   is
      I : Positive := At_Index + 1;
   begin
      OK := False;
      Next := Text'Last + 1;
      while I <= Text'Last loop
         pragma Loop_Invariant (I in At_Index + 1 .. Text'Last);
         pragma Loop_Variant (Increases => I);
         declare
            C : constant Character := Text (I);
         begin
            if C = '"' then
               Next := I + 1;
               OK := True;
               return;
            elsif Character'Pos (C) < First_Printable then
               return;
            elsif C = '\' then
               if Text'Last - I < 2 then
                  return;
               end if;
               case Text (I + 1) is
                  when '"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' =>
                     I := I + 2;
                  when 'u' =>
                     if Text'Last - I < 6 or else Hex_Value (Text (I + 2)) < 0
                       or else Hex_Value (Text (I + 3)) < 0 or else Hex_Value (Text (I + 4)) < 0
                       or else Hex_Value (Text (I + 5)) < 0
                     then
                        return;
                     end if;
                     I := I + 6;
                  when others =>
                     return;
               end case;
            else
               if I = Text'Last then
                  return;
               end if;
               I := I + 1;
            end if;
         end;
      end loop;
   end Scan_String;

   procedure Scan_Number (Text : String; At_Index : Positive; Next : out Positive; OK : out Boolean)
   with Pre => At_Index in Text'Range and then Text'Last < Positive'Last,
        Post => Next in At_Index + 1 .. Text'Last + 1
   is
      I : Positive := At_Index;
      Seen : Boolean;
      procedure Digits_Run
      with Pre => Text'Last < Positive'Last and then At_Index >= Text'First and then
                  I in At_Index .. Text'Last + 1,
           Post => I in I'Old .. Text'Last + 1 and then (if Seen then I > I'Old),
           Always_Terminates
      is
      begin
         Seen := False;
         while I <= Text'Last and then Is_Digit (Text (I)) loop
            pragma Loop_Invariant (I in I'Loop_Entry .. Text'Last);
            pragma Loop_Invariant (if Seen then I > I'Loop_Entry);
            pragma Loop_Variant (Increases => I);
            Seen := True;
            I := I + 1;
         end loop;
      end Digits_Run;
   begin
      OK := False;
      Next := At_Index + 1;
      if Text (I) = '-' then
         I := I + 1;
      end if;
      if I > Text'Last then
         return;
      elsif Text (I) = '0' then
         I := I + 1;
      else
         Digits_Run;
         if not Seen then
            return;
         end if;
      end if;
      if I <= Text'Last and then Text (I) = '.' then
         I := I + 1;
         Digits_Run;
         if not Seen then
            return;
         end if;
      end if;
      if I <= Text'Last and then Text (I) in 'e' | 'E' then
         I := I + 1;
         if I <= Text'Last and then Text (I) in '+' | '-' then
            I := I + 1;
         end if;
         Digits_Run;
         if not Seen then
            return;
         end if;
      end if;
      if I = At_Index then
         return;
      end if;
      Next := I;
      OK := True;
   end Scan_Number;

   type Container_Stack is array (1 .. Maximum_Depth) of Node_Index;
   type Expectation is
     (Expect_Value, Expect_Value_Or_Close, Expect_Key, Expect_Key_Or_Close, Expect_Colon,
      Expect_Comma_Or_Close, Expect_Nothing);

   procedure Parse (Text : String; Doc : out Document; Result : out Parse_Result) is
      Stack : Container_Stack := [others => 1];
      Depth : Natural range 0 .. Maximum_Depth := 0;
      Expect : Expectation := Expect_Value;
      I : Positive;
      Next : Positive;
      OK : Boolean;

      --  Doc's nodes so far keep the post's shape (Next within the nodes).
      function Shaped return Boolean is
        (Doc.Count <= Maximum_Nodes and then
         (for all J in 1 .. Doc.Count =>
            Doc.Nodes (J).Next in J + 1 .. Doc.Count + 1 and then
            Doc.Nodes (J).First in Text'Range and then
            Doc.Nodes (J).Last in Doc.Nodes (J).First .. Text'Last and then
            (if Doc.Nodes (J).Kind = String_Node then
               Doc.Nodes (J).Last > Doc.Nodes (J).First and then
               Text (Doc.Nodes (J).First) = '"' and then Text (Doc.Nodes (J).Last) = '"')));

      function Stacked return Boolean is
        (for all K in 1 .. Depth => Stack (K) <= Doc.Count and then
           Doc.Nodes (Stack (K)).Kind in Object_Node | Array_Node);

      --  A new node for the value at Text (First .. Last).
      procedure Add (Kind : Node_Kind; First, Last : Positive; Counted : Boolean; Added : out Boolean)
      with Pre => Shaped and then Stacked and then First in Text'Range and then
                  Last in First .. Text'Last and then
                  (if Kind = String_Node then Last > First and then Text (First) = '"' and then Text (Last) = '"'),
           Post => Shaped and then Stacked and then
                   (if Added then Doc.Count = Doc.Count'Old + 1 and then Doc.Nodes (Doc.Count).Kind = Kind
                    else Doc.Count = Doc.Count'Old)
      is
      begin
         Added := False;
         if Doc.Count = Maximum_Nodes then
            return;
         end if;
         if Counted and then Depth > 0 and then Doc.Nodes (Stack (Depth)).Members < Maximum_Nodes then
            Doc.Nodes (Stack (Depth)).Members := Doc.Nodes (Stack (Depth)).Members + 1;
         end if;
         Doc.Count := Doc.Count + 1;
         Doc.Nodes (Doc.Count) := (Kind => Kind, First => First, Last => Last, Next => Doc.Count + 1,
                                   Members => 0);
         Added := True;
      end Add;

      procedure Close (At_Index : Positive)
      with Pre => Depth >= 1 and then Shaped and then Stacked and then At_Index in Text'Range,
           Post => Shaped and then Stacked and then Depth = Depth'Old - 1 and then Doc.Count = Doc.Count'Old
                   and then Doc.Count >= 1
      is
         Top : constant Node_Index := Stack (Depth);
      begin
         --  A container closes after it opens (its First is never past here).
         Doc.Nodes (Top).Last := Natural'Max (At_Index, Doc.Nodes (Top).First);
         Doc.Nodes (Top).Next := Doc.Count + 1;
         Depth := Depth - 1;
      end Close;

      procedure After_Value
      with Post => Expect = (if Depth = 0 then Expect_Nothing else Expect_Comma_Or_Close)
      is
      begin
         Expect := (if Depth = 0 then Expect_Nothing else Expect_Comma_Or_Close);
      end After_Value;

      Added : Boolean;
   begin
      Doc := (Count => 0, Nodes => [others => (others => <>)]);
      Result := Not_Json;
      if Text'Length = 0 then
         return;
      end if;
      I := Text'First;
      loop
         pragma Loop_Invariant (I in Text'Range and then Shaped and then Stacked);
         pragma Loop_Invariant (Expect in Expect_Value | Expect_Nothing or else Depth >= 1);
         pragma Loop_Invariant (if Expect /= Expect_Value or else Depth >= 1 then Doc.Count >= 1);
         pragma Loop_Variant (Increases => I);
         declare
            C : constant Character := Text (I);
         begin
            Next := I + 1;
            if not Is_Space (C) then
               case Expect is
                  when Expect_Nothing =>
                     return;
                  when Expect_Colon =>
                     if C /= ':' then
                        return;
                     end if;
                     Expect := Expect_Value;
                  when Expect_Comma_Or_Close =>
                     if C = ',' then
                        Expect := (if Doc.Nodes (Stack (Depth)).Kind = Object_Node then Expect_Key
                                   else Expect_Value);
                     elsif (C = '}' and then Doc.Nodes (Stack (Depth)).Kind = Object_Node)
                       or else (C = ']' and then Doc.Nodes (Stack (Depth)).Kind = Array_Node)
                     then
                        Close (I);
                        After_Value;
                     else
                        return;
                     end if;
                  when Expect_Key | Expect_Key_Or_Close =>
                     if C = '}' and then Expect = Expect_Key_Or_Close then
                        Close (I);
                        After_Value;
                     elsif C = '"' then
                        Scan_String (Text, I, Next, OK);
                        if not OK then
                           return;
                        end if;
                        Add (String_Node, I, Next - 1, False, Added);
                        if not Added then
                           Result := Too_Large;
                           return;
                        end if;
                        Expect := Expect_Colon;
                     else
                        return;
                     end if;
                  when Expect_Value | Expect_Value_Or_Close =>
                     if C = ']' and then Expect = Expect_Value_Or_Close then
                        Close (I);
                        After_Value;
                     elsif C = '{' or else C = '[' then
                        if Depth = Maximum_Depth then
                           Result := Too_Deep;
                           return;
                        end if;
                        Add ((if C = '{' then Object_Node else Array_Node), I, I, True, Added);
                        if not Added then
                           Result := Too_Large;
                           return;
                        end if;
                        Depth := Depth + 1;
                        Stack (Depth) := Doc.Count;
                        Expect := (if C = '{' then Expect_Key_Or_Close else Expect_Value_Or_Close);
                     else
                        declare
                           Kind : Node_Kind := Null_Node;
                        begin
                           OK := False;
                           Added := False;
                           if C = '"' then
                              Scan_String (Text, I, Next, OK);
                              if OK then
                                 Add (String_Node, I, Next - 1, True, Added);
                              end if;
                           elsif C = '-' or else Is_Digit (C) then
                              Scan_Number (Text, I, Next, OK);
                              if OK then
                                 Add (Number_Node, I, Next - 1, True, Added);
                              end if;
                           else
                              for Literal in True_Node .. Null_Node loop
                                 pragma Loop_Invariant (not OK and then Next = I + 1);
                                 declare
                                    Word : constant String :=
                                      (case Literal is
                                          when True_Node => "true",
                                          when False_Node => "false",
                                          when others => "null");
                                 begin
                                    if Text'Last - I + 1 >= Word'Length
                                      and then Text (I .. I + Word'Length - 1) = Word
                                    then
                                       OK := True;
                                       Kind := Literal;
                                       exit;
                                    end if;
                                 end;
                              end loop;
                              if OK then
                                 Next := I + (case Kind is when True_Node => 4, when False_Node => 5,
                                                           when others => 4);
                                 Add (Kind, I, Next - 1, True, Added);
                              end if;
                           end if;
                           if not OK then
                              return;
                           elsif not Added then
                              Result := Too_Large;
                              return;
                           end if;
                           After_Value;
                        end;
                     end if;
               end case;
            end if;
         end;
         pragma Assert (Next > I);
         exit when Next > Text'Last;
         I := Next;
      end loop;
      if Expect = Expect_Nothing then
         Result := Parsed;
      end if;
   end Parse;

   procedure Put_Point (Into : in out String; Last : in out Natural; Point : Natural; OK : in out Boolean)
   with Pre => Into'First = 1 and then Into'Last < Positive'Last and then Last <= Into'Last and then
               Point <= 16#10_FFFF#,
        Post => Last <= Into'Last
   is
      procedure Put (B : Natural)
      with Pre => B <= 255 and then Into'First = 1 and then Into'Last < Positive'Last and then Last <= Into'Last,
           Post => Last <= Into'Last
      is
      begin
         if Last < Into'Last then
            Last := Last + 1;
            Into (Last) := Character'Val (B);
         else
            OK := False;
         end if;
      end Put;
   begin
      if Point < 16#80# then
         Put (Point);
      elsif Point < 16#800# then
         Put (16#C0# + Point / 16#40#);
         Put (16#80# + Point mod 16#40#);
      elsif Point < 16#1_0000# then
         Put (16#E0# + Point / 16#1000#);
         Put (16#80# + (Point / 16#40#) mod 16#40#);
         Put (16#80# + Point mod 16#40#);
      else
         Put (16#F0# + Point / 16#4_0000#);
         Put (16#80# + (Point / 16#1000#) mod 16#40#);
         Put (16#80# + (Point / 16#40#) mod 16#40#);
         Put (16#80# + Point mod 16#40#);
      end if;
   end Put_Point;

   procedure Decode_String
     (Text : String; Item : Node; Into : out String; Last : out Natural; OK : out Boolean)
   is
      I : Natural := Item.First + 1;
      High : Natural := 0;
   begin
      Into := [others => ' '];
      Last := 0;
      OK := True;
      while I < Item.Last loop
         pragma Loop_Invariant (I in Item.First + 1 .. Item.Last - 1 and then Last <= Into'Last);
         pragma Loop_Invariant (High = 0 or else High in First_High_Surrogate .. Last_High_Surrogate);
         pragma Loop_Variant (Increases => I);
         declare
            C : constant Character := Text (I);
            Unit : Natural := Character'Pos (C);
            Step : Positive := 1;
            Escaped_Unit : Boolean := False;
         begin
            if C = '\' and then I + 1 < Item.Last then
               Step := 2;
               case Text (I + 1) is
                  when 'b' => Unit := 8;
                  when 'f' => Unit := 12;
                  when 'n' => Unit := 10;
                  when 'r' => Unit := 13;
                  when 't' => Unit := 9;
                  when 'u' =>
                     if Item.Last - I > 5 and then Hex_Value (Text (I + 2)) >= 0
                       and then Hex_Value (Text (I + 3)) >= 0 and then Hex_Value (Text (I + 4)) >= 0
                       and then Hex_Value (Text (I + 5)) >= 0
                     then
                        Unit := ((Hex_Value (Text (I + 2)) * 16 + Hex_Value (Text (I + 3))) * 16 +
                                 Hex_Value (Text (I + 4))) * 16 + Hex_Value (Text (I + 5));
                        Step := 6;
                        Escaped_Unit := True;
                     else
                        OK := False;
                        return;
                     end if;
                  when others => Unit := Character'Pos (Text (I + 1));
               end case;
            end if;
            if Escaped_Unit and then Unit in First_High_Surrogate .. Last_High_Surrogate then
               if High /= 0 then
                  Put_Point (Into, Last, Replacement, OK);
               end if;
               High := Unit;
            elsif Escaped_Unit and then Unit in First_Low_Surrogate .. Last_Low_Surrogate then
               if High /= 0 then
                  Put_Point (Into, Last,
                             Supplementary_Base + (High - First_High_Surrogate) * Surrogate_Bits +
                             (Unit - First_Low_Surrogate), OK);
                  High := 0;
               else
                  Put_Point (Into, Last, Replacement, OK);
               end if;
            else
               if High /= 0 then
                  Put_Point (Into, Last, Replacement, OK);
                  High := 0;
               end if;
               if Escaped_Unit then
                  Put_Point (Into, Last, Unit, OK);
               elsif Last < Into'Last then
                  Last := Last + 1;
                  Into (Last) := Character'Val (Unit);
               else
                  OK := False;
               end if;
            end if;
            if not OK then
               return;
            end if;
            exit when Item.Last - I <= Step;
            I := I + Step;
         end;
      end loop;
      if High /= 0 then
         Put_Point (Into, Last, Replacement, OK);
      end if;
   end Decode_String;

   procedure Integer_Value
     (Text : String; Item : Node; Value : out Integer_64; OK : out Boolean)
   is
      Negative : constant Boolean := Text (Item.First) = '-';
      I : Natural := (if Negative then Item.First + 1 else Item.First);
      Magnitude : Unsigned_64 := 0;
      Limit : constant Unsigned_64 :=
        (if Negative then Unsigned_64 (Integer_64'Last) + 1 else Unsigned_64 (Integer_64'Last));
   begin
      Value := 0;
      OK := False;
      if I > Item.Last then
         return;
      end if;
      while I <= Item.Last loop
         pragma Loop_Invariant (I in Item.First .. Item.Last and then Magnitude <= Limit);
         if not Is_Digit (Text (I)) then
            return;   --  a fraction or an exponent: not an integer
         end if;
         declare
            Digit : constant Unsigned_64 := Character'Pos (Text (I)) - Character'Pos ('0');
         begin
            if Magnitude > (Limit - Digit) / 10 then
               return;
            end if;
            Magnitude := Magnitude * 10 + Digit;
         end;
         exit when I = Item.Last;
         I := I + 1;
      end loop;
      if Negative then
         Value := (if Magnitude = Unsigned_64 (Integer_64'Last) + 1 then Integer_64'First
                   else -Integer_64 (Magnitude));
      else
         Value := Integer_64 (Magnitude);
      end if;
      OK := True;
   end Integer_Value;
end Agent_Json;
