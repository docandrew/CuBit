------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  The v0 grammar is rustc's (RFC 2603, "Symbol name mangling v2"): paths
--  C (crate root), N (nested), M/X/Y (impls), I (generic arguments), B
--  (back references); types by letter; constants as hex.
------------------------------------------------------------------------------
pragma Ada_2022;

--  Not in SPARK yet: the recursive descent reads through a cursor its
--  helper functions advance. Tested against c++filt on every Rust symbol of
--  the demo (tests/debug-engine/run-rs.sh).
package body CuBit.Debug.Demangle with SPARK_Mode => Off is

   Base_62_Digits : constant := 62;
   Lower_Base     : constant := 10;
   Upper_Base     : constant := 36;
   Decimal_Base   : constant := 10;
   Hex_Base       : constant := 16;
   Hex_Digits_Ceiling : constant := 16;
   Legacy_Hash_Digits : constant := 16;
   Number_Ceiling : constant := 2 ** 24;   --  larger: malformed

   procedure Rust (Symbol : String; Result : out Output) is
      Pos   : Natural := Symbol'First;      --  the next character to read
      Steps : Natural := 0;
      Good  : Boolean := True;
      --  Where v0 back references count from: just after "_R".
      Origin : constant Natural := Symbol'First + 2;

      procedure Put (S : String) is
      begin
         for C of S loop
            if Result.Length < Output_Ceiling then
               Result.Length := Result.Length + 1;
               Result.Text (Result.Length) := C;
            else
               Result.Cut := True;
            end if;
         end loop;
      end Put;

      function At_End return Boolean is (Pos > Symbol'Last);
      function Peek return Character is (if Pos <= Symbol'Last then Symbol (Pos) else ASCII.NUL);
      function Take (C : Character) return Boolean is
      begin
         if Pos <= Symbol'Last and then Symbol (Pos) = C then
            Pos := Pos + 1;
            return True;
         end if;
         return False;
      end Take;
      procedure Step is
      begin
         Steps := Steps + 1;
         if Steps > Step_Ceiling then
            Good := False;
         end if;
      end Step;

      procedure Decimal (Value : out Natural) is
      begin
         Value := 0;
         if At_End or else Peek not in '0' .. '9' then
            Good := False;
            return;
         end if;
         --  "0" stands alone: a zero is never a leading digit.
         if Take ('0') then
            return;
         end if;
         while not At_End and then Peek in '0' .. '9' loop
            if Value > Number_Ceiling then
               Good := False;
               return;
            end if;
            Value := Value * Decimal_Base + (Character'Pos (Peek) - Character'Pos ('0'));
            Pos := Pos + 1;
         end loop;
      end Decimal;

      --  A base-62 number ending in "_": "_" is 0, else the value plus one.
      procedure Base_62 (Value : out Natural) is
         Digit : Natural;
      begin
         Value := 0;
         if Take ('_') then
            return;
         end if;
         loop
            if At_End then
               Good := False;
               return;
            end if;
            exit when Peek = '_';
            case Peek is
               when '0' .. '9' => Digit := Character'Pos (Peek) - Character'Pos ('0');
               when 'a' .. 'z' => Digit := Lower_Base + Character'Pos (Peek) - Character'Pos ('a');
               when 'A' .. 'Z' => Digit := Upper_Base + Character'Pos (Peek) - Character'Pos ('A');
               when others =>
                  Good := False;
                  return;
            end case;
            --  Disambiguators are 64-bit hashes: past the ceiling the value
            --  saturates (no back reference reaches that far).
            if Value <= Number_Ceiling then
               Value := Value * Base_62_Digits + Digit;
            end if;
            Pos := Pos + 1;
         end loop;
         Pos := Pos + 1;   --  the "_"
         if Value <= Number_Ceiling then
            Value := Value + 1;
         end if;
      end Base_62;

      procedure Skip_Disambiguator is
         Ignore : Natural;
      begin
         if Take ('s') then
            Base_62 (Ignore);
         end if;
      end Skip_Disambiguator;

      --  [disambiguator] ["u"] length ["_"] bytes: the disambiguator (0 when
      --  absent) and where the bytes are.
      procedure Identifier_Parts
        (Disambiguator : out Natural; First : out Natural; Length : out Natural; Punycode : out Boolean)
      is
         Value : Natural;
      begin
         Disambiguator := 0;
         First := Pos;
         Length := 0;
         Punycode := False;
         if Take ('s') then
            Base_62 (Value);
            Disambiguator := (if Value < Number_Ceiling then Value + 1 else Value);
         end if;
         Punycode := Take ('u');
         Decimal (Length);
         if not Good then
            return;
         end if;
         if Take ('_') then
            null;   --  the separator before bytes that start with a digit or "_"
         end if;
         if Length > Symbol'Last - Pos + 1 then
            Good := False;
            Length := 0;
            return;
         end if;
         First := Pos;
         Pos := Pos + Length;
      end Identifier_Parts;

      procedure Identifier (Shown : Boolean := True) is
         Length : Natural;
         Punycode : Boolean;
      begin
         Skip_Disambiguator;
         Punycode := Take ('u');
         Decimal (Length);
         if not Good then
            return;
         end if;
         if Take ('_') then
            null;   --  the separator before bytes that start with a digit or "_"
         end if;
         if Length > Symbol'Last - Pos + 1 then
            Good := False;
            return;
         end if;
         if Shown then
            Put (Symbol (Pos .. Pos + Length - 1));
            if Punycode then
               Put ("{punycode}");
            end if;
         end if;
         Pos := Pos + Length;
      end Identifier;

      procedure Path (Depth : Natural; In_Value : Boolean);
      procedure Type_Of (Depth : Natural);

      --  B base-62: parse what the earlier position names, then come back.
      procedure Back_Reference (Depth : Natural; As_Type : Boolean; In_Value : Boolean := False) is
         Offset : Natural;
         Saved : Natural;
      begin
         Base_62 (Offset);
         if not Good or else Offset > Symbol'Last - Origin or else Origin + Offset >= Pos - 1 then
            Good := False;   --  only backwards
            return;
         end if;
         Saved := Pos;
         Pos := Origin + Offset;
         if As_Type then
            Type_Of (Depth + 1);
         else
            Path (Depth + 1, In_Value);
         end if;
         Pos := Saved;
      end Back_Reference;

      procedure Const (Depth : Natural) is
         Kind : Character;
         Negative : Boolean;
         Value : Natural := 0;
         Digits_Read : Natural := 0;
      begin
         Step;
         if not Good then
            return;
         end if;
         if Take ('p') then
            Put ("_");
            return;
         elsif Take ('B') then
            Pos := Pos - 1;
            Back_Reference (Depth, As_Type => True);
            return;
         end if;
         --  A type letter, then ["n"] hex digits "_".
         if At_End then
            Good := False;
            return;
         end if;
         Kind := Peek;
         Pos := Pos + 1;
         Negative := Take ('n');
         while not At_End and then Peek /= '_' loop
            Digits_Read := Digits_Read + 1;
            if Digits_Read > Hex_Digits_Ceiling or else Value > Number_Ceiling then
               Good := False;
               return;
            end if;
            case Peek is
               when '0' .. '9' => Value := Value * Hex_Base + (Character'Pos (Peek) - Character'Pos ('0'));
               when 'a' .. 'f' => Value := Value * Hex_Base + Lower_Base + (Character'Pos (Peek) - Character'Pos ('a'));
               when others =>
                  Good := False;
                  return;
            end case;
            Pos := Pos + 1;
         end loop;
         if not Take ('_') then
            Good := False;
            return;
         end if;
         declare
            Image : constant String := Natural'Image (Value);
            Digits_Text : constant String := Image (Image'First + 1 .. Image'Last);
         begin
            case Kind is
               when 'b' => Put (if Value = 0 then "false" else "true");
               when 'c' =>
                  if Value in 32 .. 126 then
                     Put ("'" & Character'Val (Value) & "'");
                  else
                     Put ("'\u{" & Digits_Text & "}'");
                  end if;
               when others =>
                  Put ((if Negative then "-" else "") & Digits_Text);
                  --  An integer constant names its type, as c++filt shows it.
                  Put (case Kind is
                          when 'a' => ": i8", when 'h' => ": u8", when 'i' => ": isize",
                          when 'j' => ": usize", when 'l' => ": i32", when 'm' => ": u32",
                          when 'n' => ": i128", when 'o' => ": u128", when 's' => ": i16",
                          when 't' => ": u16", when 'x' => ": i64", when 'y' => ": u64",
                          when others => "");
            end case;
         end;
      end Const;

      procedure Generic_Arguments (Depth : Natural) is
         First : Boolean := True;
         Ignore : Natural;
      begin
         Put ("<");
         while Good and then not At_End and then Peek /= 'E' loop
            Step;
            if Take ('L') then
               Base_62 (Ignore);   --  lifetimes are not shown
            else
               if not First then
                  Put (", ");
               end if;
               First := False;
               if Take ('K') then
                  Const (Depth + 1);
               else
                  Type_Of (Depth + 1);
               end if;
            end if;
         end loop;
         if not Take ('E') then
            Good := False;
         end if;
         Put (">");
      end Generic_Arguments;

      procedure Binder is
         Ignore : Natural;
      begin
         if Take ('G') then
            Base_62 (Ignore);
         end if;
      end Binder;

      procedure Type_Of (Depth : Natural) is
         Ignore : Natural;
         C : Character;
      begin
         Step;
         if not Good or else Depth > Nesting_Ceiling or else At_End then
            Good := False;
            return;
         end if;
         C := Peek;
         case C is
            when 'a' => Pos := Pos + 1; Put ("i8");
            when 'b' => Pos := Pos + 1; Put ("bool");
            when 'c' => Pos := Pos + 1; Put ("char");
            when 'd' => Pos := Pos + 1; Put ("f64");
            when 'e' => Pos := Pos + 1; Put ("str");
            when 'f' => Pos := Pos + 1; Put ("f32");
            when 'h' => Pos := Pos + 1; Put ("u8");
            when 'i' => Pos := Pos + 1; Put ("isize");
            when 'j' => Pos := Pos + 1; Put ("usize");
            when 'l' => Pos := Pos + 1; Put ("i32");
            when 'm' => Pos := Pos + 1; Put ("u32");
            when 'n' => Pos := Pos + 1; Put ("i128");
            when 'o' => Pos := Pos + 1; Put ("u128");
            when 's' => Pos := Pos + 1; Put ("i16");
            when 't' => Pos := Pos + 1; Put ("u16");
            when 'u' => Pos := Pos + 1; Put ("()");
            when 'v' => Pos := Pos + 1; Put ("...");
            when 'x' => Pos := Pos + 1; Put ("i64");
            when 'y' => Pos := Pos + 1; Put ("u64");
            when 'z' => Pos := Pos + 1; Put ("!");
            when 'p' => Pos := Pos + 1; Put ("_");
            when 'A' =>
               Pos := Pos + 1;
               Put ("[");
               Type_Of (Depth + 1);
               Put ("; ");
               Const (Depth + 1);
               Put ("]");
            when 'S' =>
               Pos := Pos + 1;
               Put ("[");
               Type_Of (Depth + 1);
               Put ("]");
            when 'T' =>
               Pos := Pos + 1;
               Put ("(");
               declare
                  Count : Natural := 0;
               begin
                  while Good and then not At_End and then Peek /= 'E' loop
                     Step;
                     if Count > 0 then
                        Put (", ");
                     end if;
                     Count := Count + 1;
                     Type_Of (Depth + 1);
                  end loop;
                  if Count = 1 then
                     Put (",");   --  a one-element tuple
                  end if;
               end;
               if not Take ('E') then
                  Good := False;
               end if;
               Put (")");
            when 'R' | 'Q' =>
               Pos := Pos + 1;
               if Take ('L') then
                  Base_62 (Ignore);
               end if;
               Put (if C = 'R' then "&" else "&mut ");
               Type_Of (Depth + 1);
            when 'P' | 'O' =>
               Pos := Pos + 1;
               Put (if C = 'P' then "*const " else "*mut ");
               Type_Of (Depth + 1);
            when 'F' =>
               Pos := Pos + 1;
               Binder;
               if Take ('U') then
                  Put ("unsafe ");
               end if;
               if Take ('K') then
                  if Take ('C') then
                     Put ("extern ""C"" ");
                  else
                     Identifier (Shown => False);
                  end if;
               end if;
               Put ("fn(");
               declare
                  First : Boolean := True;
               begin
                  while Good and then not At_End and then Peek /= 'E' loop
                     Step;
                     if not First then
                        Put (", ");
                     end if;
                     First := False;
                     Type_Of (Depth + 1);
                  end loop;
               end;
               if not Take ('E') then
                  Good := False;
               end if;
               Put (")");
               if not At_End and then Peek /= 'u' then
                  Put (" -> ");
                  Type_Of (Depth + 1);
               elsif Take ('u') then
                  null;
               end if;
            when 'D' =>
               Pos := Pos + 1;
               Binder;
               Put ("dyn ");
               declare
                  First : Boolean := True;
               begin
                  while Good and then not At_End and then Peek /= 'E' loop
                     Step;
                     if not First then
                        Put (" + ");
                     end if;
                     First := False;
                     Path (Depth + 1, False);
                     --  Associated type bindings (p identifier type) join the
                     --  trait's generic arguments: FnOnce<(), Output = ()>.
                     while Good and then Take ('p') loop
                        if Result.Length > 0 and then Result.Text (Result.Length) = '>' then
                           Result.Length := Result.Length - 1;
                           Put (", ");
                        else
                           Put ("<");
                        end if;
                        Identifier;
                        Put (" = ");
                        Type_Of (Depth + 1);
                        Put (">");
                     end loop;
                  end loop;
               end;
               if not Take ('E') then
                  Good := False;
               end if;
               if Take ('L') then
                  Base_62 (Ignore);
               end if;
            when 'B' =>
               Pos := Pos + 1;
               Back_Reference (Depth, As_Type => True);
            when others =>
               Path (Depth + 1, False);
         end case;
      end Type_Of;

      procedure Path (Depth : Natural; In_Value : Boolean) is
         Namespace : Character;
      begin
         Step;
         if not Good or else Depth > Nesting_Ceiling or else At_End then
            Good := False;
            return;
         end if;
         if Take ('C') then
            Identifier;
         elsif Take ('N') then
            if At_End then
               Good := False;
               return;
            end if;
            Namespace := Peek;
            Pos := Pos + 1;
            Path (Depth + 1, In_Value);
            declare
               Disambiguator, First, Length : Natural;
               Punycode : Boolean;
               function Number return String is
                 (Natural'Image (Disambiguator) (2 .. Natural'Image (Disambiguator)'Last));
            begin
               Identifier_Parts (Disambiguator, First, Length, Punycode);
               if not Good then
                  return;
               end if;
               Put ("::");
               if Namespace in 'A' .. 'Z' then
                  --  Special namespaces: {closure#N}, {shim:name#N} ...
                  Put ("{" & (case Namespace is when 'C' => "closure", when 'S' => "shim",
                                                  when others => (1 => Namespace)));
                  if Length > 0 then
                     Put (":" & Symbol (First .. First + Length - 1));
                  end if;
                  Put ("#" & Number & "}");
               else
                  Put (Symbol (First .. First + Length - 1));
                  if Punycode then
                     Put ("{punycode}");
                  end if;
               end if;
            end;
         elsif Take ('M') then
            Skip_Disambiguator;
            declare
               Mark : constant Output_Length := Result.Length;
            begin
               Path (Depth + 1, False);
               Result.Length := Mark;   --  the impl's module is not shown
            end;
            Put ("<");
            Type_Of (Depth + 1);
            Put (">");
         elsif Take ('X') then
            Skip_Disambiguator;
            declare
               Mark : constant Output_Length := Result.Length;
            begin
               Path (Depth + 1, False);
               Result.Length := Mark;   --  the impl's module is not shown
            end;
            Put ("<");
            Type_Of (Depth + 1);
            Put (" as ");
            Path (Depth + 1, False);
            Put (">");
         elsif Take ('Y') then
            Put ("<");
            Type_Of (Depth + 1);
            Put (" as ");
            Path (Depth + 1, False);
            Put (">");
         elsif Take ('I') then
            Path (Depth + 1, In_Value);
            if In_Value then
               Put ("::");   --  a value path's generic arguments: turbofish
            end if;
            Generic_Arguments (Depth + 1);
         elsif Take ('B') then
            Back_Reference (Depth, As_Type => False, In_Value => In_Value);
         else
            Good := False;
         end if;
      end Path;

      procedure Legacy is
         Length : Natural;
         First : Boolean := True;
      begin
         Pos := Symbol'First + 3;
         while Good and then not At_End and then Peek /= 'E' loop
            Step;
            Decimal (Length);
            exit when not Good;
            if Length > Symbol'Last - Pos + 1 then
               Good := False;
               exit;
            end if;
            declare
               Part : constant String := Symbol (Pos .. Pos + Length - 1);
               Is_Hash : constant Boolean :=
                 Part'Length = Legacy_Hash_Digits + 1 and then Part (Part'First) = 'h'
                 and then (for all C of Part (Part'First + 1 .. Part'Last) => C in '0' .. '9' | 'a' .. 'f');
               I : Natural := Part'First;
            begin
               Pos := Pos + Length;
               if not Is_Hash or else (not At_End and then Peek /= 'E') then
                  if not First then
                     Put ("::");
                  end if;
                  First := False;
                  if Part'Length >= 2 and then Part (I .. I + 1) = "_$" then
                     I := I + 1;
                  end if;
                  while I <= Part'Last loop
                     declare
                        Rest : constant String := Part (I .. Part'Last);
                        function Starts (S : String) return Boolean is
                          (Rest'Length >= S'Length and then Rest (Rest'First .. Rest'First + S'Length - 1) = S);
                     begin
                        if Starts ("$LT$") then Put ("<"); I := I + 4;
                        elsif Starts ("$GT$") then Put (">"); I := I + 4;
                        elsif Starts ("$RF$") then Put ("&"); I := I + 4;
                        elsif Starts ("$BP$") then Put ("*"); I := I + 4;
                        elsif Starts ("$LP$") then Put ("("); I := I + 4;
                        elsif Starts ("$RP$") then Put (")"); I := I + 4;
                        elsif Starts ("$C$") then Put (","); I := I + 3;
                        elsif Starts ("$u20$") then Put (" "); I := I + 5;
                        elsif Starts ("$u27$") then Put ("'"); I := I + 5;
                        elsif Starts ("$u5b$") then Put ("["); I := I + 5;
                        elsif Starts ("$u5d$") then Put ("]"); I := I + 5;
                        elsif Starts ("$u7b$") then Put ("{"); I := I + 5;
                        elsif Starts ("$u7d$") then Put ("}"); I := I + 5;
                        elsif Starts ("$u7e$") then Put ("~"); I := I + 5;
                        elsif Starts ("$u3b$") then Put (";"); I := I + 5;
                        elsif Starts ("$u2b$") then Put ("+"); I := I + 5;
                        elsif Starts ("$u22$") then Put (""""); I := I + 5;
                        elsif Starts ("..") then Put ("::"); I := I + 2;
                        else
                           Put (Rest (Rest'First .. Rest'First));
                           I := I + 1;
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
         if not Take ('E') or else not At_End then
            Good := False;
         end if;
      end Legacy;
   begin
      Result := (others => <>);
      if Is_V0 (Symbol) then
         Pos := Symbol'First + 2;
         if not At_End and then Peek in '0' .. '9' then
            declare
               Ignore : Natural;
            begin
               Decimal (Ignore);   --  the encoding version
            end;
         end if;
         Path (0, In_Value => True);
         --  An instantiating crate may follow; it is not shown.
      elsif Is_Legacy (Symbol) then
         Legacy;
      else
         Good := False;
      end if;
      Result.Valid := Good and then Result.Length > 0;
      if not Result.Valid then
         Result.Length := 0;
      end if;
   end Rust;

end CuBit.Debug.Demangle;
