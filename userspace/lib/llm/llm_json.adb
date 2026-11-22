pragma Ada_2022;
package body Llm_Json with SPARK_Mode is

   type Container is (In_Object, In_Array);
   type Container_Stack is array (1 .. Maximum_Depth) of Container;
   type Expectation is
     (Expect_Value, Expect_Value_Or_Close, Expect_Key, Expect_Key_Or_Close, Expect_Colon,
      Expect_Comma_Or_Close, Expect_Nothing);

   First_Printable : constant := 16#20#;

   function Is_Space (C : Character) return Boolean is
     (C in ' ' | ASCII.HT | ASCII.LF | ASCII.CR);
   function Is_Digit (C : Character) return Boolean is (C in '0' .. '9');
   function Is_Hex (C : Character) return Boolean is
     (C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F');

   --  The string starting at Text (At_Index) = '"': Next is the index
   --  after its closing quote; OK False when it is malformed.
   procedure Scan_String (Text : String; At_Index : Positive; Next : out Positive; OK : out Boolean)
   with Pre => Text'Length <= Maximum_Text and then At_Index in Text'Range
                 and then Text'Last < Positive'Last,
        Post => Next in At_Index + 1 .. Text'Last + 1
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
               if I = Text'Last then
                  return;
               end if;
               case Text (I + 1) is
                  when '"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' =>
                     if I + 1 = Text'Last then
                        return;
                     end if;
                     I := I + 2;
                  when 'u' =>
                     if Text'Last - I < 6 or else not (Is_Hex (Text (I + 2)) and then Is_Hex (Text (I + 3))
                       and then Is_Hex (Text (I + 4)) and then Is_Hex (Text (I + 5)))
                     then
                        return;
                     end if;
                     if I + 5 = Text'Last then
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

   --  A number (RFC 8259 section 6) at At_Index: Next is the index after it.
   procedure Scan_Number (Text : String; At_Index : Positive; Next : out Positive; OK : out Boolean)
   with Pre => Text'Length <= Maximum_Text and then At_Index in Text'Range
                 and then Text'Last < Positive'Last,
        Post => Next in At_Index + 1 .. Text'Last + 1
   is
      I : Positive := At_Index;
      procedure Digits_Run (Seen : out Boolean)
      with Pre => Text'Last < Positive'Last and then At_Index >= Text'First and then
                  I in At_Index .. Text'Last + 1,
           Post => I in I'Old .. Text'Last + 1,
           Always_Terminates
      is
      begin
         Seen := False;
         while I <= Text'Last and then Is_Digit (Text (I)) loop
            pragma Loop_Invariant (I in I'Loop_Entry .. Text'Last);
            pragma Loop_Variant (Increases => I);
            Seen := True;
            I := I + 1;
         end loop;
      end Digits_Run;
      Seen : Boolean;
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
         Digits_Run (Seen);
         if not Seen then
            return;
         end if;
      end if;
      if I <= Text'Last and then Text (I) = '.' then
         I := I + 1;
         Digits_Run (Seen);
         if not Seen then
            return;
         end if;
      end if;
      if I <= Text'Last and then Text (I) in 'e' | 'E' then
         I := I + 1;
         if I <= Text'Last and then Text (I) in '+' | '-' then
            I := I + 1;
         end if;
         Digits_Run (Seen);
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

   procedure Scan_Literal
     (Text : String; At_Index : Positive; Word : String; Next : out Positive; OK : out Boolean)
   with Pre => Text'Length <= Maximum_Text and then At_Index in Text'Range
                 and then Text'Last < Positive'Last and then Word'Length in 1 .. 5,
        Post => Next in At_Index + 1 .. Text'Last + 1
   is
   begin
      OK := Text'Last - At_Index + 1 >= Word'Length
        and then Text (At_Index .. At_Index + Word'Length - 1) = Word;
      Next := (if OK then At_Index + Word'Length else At_Index + 1);
   end Scan_Literal;

   function Check (Text : String; Object_Only : Boolean) return Boolean
   with Pre => Text'Length <= Maximum_Text
   is
      Stack : Container_Stack := [others => In_Object];
      Depth : Natural range 0 .. Maximum_Depth := 0;
      Expect : Expectation := Expect_Value;
      I : Positive;
      Next : Positive;
      OK : Boolean;
      First_Value : Boolean := True;

      procedure After_Value
      with Post => Expect = (if Depth = 0 then Expect_Nothing else Expect_Comma_Or_Close)
      is
      begin
         Expect := (if Depth = 0 then Expect_Nothing else Expect_Comma_Or_Close);
      end After_Value;
   begin
      if Text'Length = 0 or else Text'Last = Positive'Last then
         return False;
      end if;
      I := Text'First;
      while I <= Text'Last loop
         pragma Loop_Invariant (I in Text'Range);
         pragma Loop_Invariant (Expect in Expect_Value | Expect_Nothing or else Depth >= 1);
         pragma Loop_Variant (Increases => I);
         declare
            C : constant Character := Text (I);
         begin
            if Is_Space (C) then
               Next := I + 1;
            else
               case Expect is
                  when Expect_Nothing =>
                     return False;
                  when Expect_Colon =>
                     if C /= ':' then
                        return False;
                     end if;
                     Expect := Expect_Value;
                     Next := I + 1;
                  when Expect_Comma_Or_Close =>
                     if C = ',' then
                        Expect := (if Stack (Depth) = In_Object then Expect_Key else Expect_Value);
                     elsif (C = '}' and then Stack (Depth) = In_Object)
                       or else (C = ']' and then Stack (Depth) = In_Array)
                     then
                        Depth := Depth - 1;
                        After_Value;
                     else
                        return False;
                     end if;
                     Next := I + 1;
                  when Expect_Key | Expect_Key_Or_Close =>
                     if C = '}' and then Expect = Expect_Key_Or_Close then
                        Depth := Depth - 1;
                        After_Value;
                        Next := I + 1;
                     elsif C = '"' then
                        Scan_String (Text, I, Next, OK);
                        if not OK then
                           return False;
                        end if;
                        Expect := Expect_Colon;
                     else
                        return False;
                     end if;
                  when Expect_Value | Expect_Value_Or_Close =>
                     if First_Value and then Object_Only and then C /= '{' then
                        return False;
                     end if;
                     First_Value := False;
                     if C = ']' and then Expect = Expect_Value_Or_Close then
                        Depth := Depth - 1;
                        After_Value;
                        Next := I + 1;
                     elsif C = '{' or else C = '[' then
                        if Depth = Maximum_Depth then
                           return False;
                        end if;
                        Depth := Depth + 1;
                        Stack (Depth) := (if C = '{' then In_Object else In_Array);
                        Expect := (if C = '{' then Expect_Key_Or_Close else Expect_Value_Or_Close);
                        Next := I + 1;
                     else
                        if C = '"' then
                           Scan_String (Text, I, Next, OK);
                        elsif C = '-' or else Is_Digit (C) then
                           Scan_Number (Text, I, Next, OK);
                        elsif C = 't' then
                           Scan_Literal (Text, I, "true", Next, OK);
                        elsif C = 'f' then
                           Scan_Literal (Text, I, "false", Next, OK);
                        elsif C = 'n' then
                           Scan_Literal (Text, I, "null", Next, OK);
                        else
                           OK := False;
                           Next := I + 1;
                        end if;
                        if not OK then
                           return False;
                        end if;
                        After_Value;
                     end if;
               end case;
            end if;
         end;
         pragma Assert (Next > I);
         exit when Next > Text'Last;
         I := Next;
      end loop;
      return Expect = Expect_Nothing;
   end Check;

   function Valid_Value (Text : String) return Boolean is (Check (Text, Object_Only => False));
   function Valid_Object (Text : String) return Boolean is (Check (Text, Object_Only => True));
end Llm_Json;
