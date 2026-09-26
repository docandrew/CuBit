pragma Ada_2022;
package body CuBit.TLS_Scopes with SPARK_Mode is

   function Name_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-');

   function Valid_Name (Text : String) return Boolean is
      Label_Start : Integer := Text'First;
      Has_Letter_In_Last : Boolean := False;
   begin
      if Text'Length not in 1 .. Maximum_Name then
         return False;
      end if;
      for I in Text'Range loop
         pragma Loop_Invariant (Label_Start in Text'First .. I);
         if Text (I) = '.' then
            --  Empty labels and labels ending in '-' are invalid.
            if I = Label_Start or else Text (I - 1) = '-' then
               return False;
            end if;
            Label_Start := I + 1;
            Has_Letter_In_Last := False;
         elsif not Name_Character (Text (I)) then
            return False;
         else
            if I = Label_Start and then Text (I) = '-' then
               return False;
            end if;
            if I - Label_Start >= 63 then
               return False;
            end if;
            if Text (I) not in '0' .. '9' | '-' then
               Has_Letter_In_Last := True;
            end if;
         end if;
      end loop;
      --  No trailing dot or hyphen, and the last label (the top-level
      --  domain) is not all digits, so IPv4 literals are rejected.
      return Label_Start <= Text'Last and then Text (Text'Last) /= '-'
        and then Has_Letter_In_Last;
   end Valid_Name;

   function Lowercase_Only (Text : String) return Boolean;
   procedure Parse_Port
     (Text : String; Value : out Unsigned_16; Success : out Boolean)
   with Post => (if Success then Value >= 1);

   function Lowercase_Only (Text : String) return Boolean is
     (for all C of Text => C not in 'A' .. 'Z');

   procedure Parse_Port
     (Text : String; Value : out Unsigned_16; Success : out Boolean)
   is
      Number : Natural := 0;
   begin
      Value := 0;
      Success := False;
      if Text'Length not in 1 .. 5 then
         return;
      end if;
      for C of Text loop
         pragma Loop_Invariant (Number <= 65_535);
         if C not in '0' .. '9' then
            return;
         end if;
         Number := Number * 10 + (Character'Pos (C) - Character'Pos ('0'));
         if Number > 65_535 then
            return;
         end if;
      end loop;
      if Number = 0 or else (Text'Length > 1 and then Text (Text'First) = '0')
      then
         return;  --  zero, or a non-canonical leading zero
      end if;
      Value := Unsigned_16 (Number);
      Success := True;
   end Parse_Port;

   procedure Parse_Host (Host : String; Item : in out Scope; OK : out Boolean)
   with Pre => Host'Length >= 1 and then Host'Last < Integer'Last,
        Post => Item.First_Port = Item.First_Port'Old
                and then Item.Last_Port = Item.Last_Port'Old
                and then (if OK then Item.Kind = Any_Name
                                     or else Item.Length >= 1);

   procedure Parse_Host (Host : String; Item : in out Scope; OK : out Boolean)
   is
   begin
      OK := False;
      if Host = "*" then
         Item.Kind := Any_Name;
         OK := True;
      elsif Host'Length > 2 and then Host (Host'First) = '*' and then
        Host (Host'First + 1) = '.'
      then
         --  "*.com" would authorize a whole top-level domain; require the
         --  suffix itself to have at least two labels.
         if Valid_Name (Host (Host'First + 2 .. Host'Last))
           and then Lowercase_Only (Host (Host'First + 2 .. Host'Last))
           and then (for some C of Host (Host'First + 2 .. Host'Last) =>
                       C = '.')
         then
            Item.Kind := Suffix;
            Item.Length := Host'Length - 2;
            Item.Name (1 .. Item.Length) := Host (Host'First + 2 .. Host'Last);
            OK := True;
         end if;
      elsif Valid_Name (Host) and then Lowercase_Only (Host) then
         Item.Kind := Exact;
         Item.Length := Host'Length;
         Item.Name (1 .. Host'Length) := Host;
         OK := True;
      end if;
   end Parse_Host;

   procedure Parse (Text : String; Item : out Scope; Success : out Boolean)
   is
      Colon : Natural := 0;
      Dash : Natural := 0;
      OK : Boolean;
   begin
      Item := (others => <>);
      Success := False;
      if Text'Length < 3 or else Text'Last = Integer'Last then
         return;
      end if;
      for I in reverse Text'Range loop
         if Text (I) = ':' then
            Colon := I;
            exit;
         end if;
      end loop;
      if Colon <= Text'First or else Colon = Text'Last then
         return;
      end if;
      for I in Colon + 1 .. Text'Last loop
         if Text (I) = '-' then
            Dash := I;
            exit;
         end if;
      end loop;
      if Dash = 0 then
         Parse_Port (Text (Colon + 1 .. Text'Last), Item.First_Port, OK);
         Item.Last_Port := Item.First_Port;
      else
         Parse_Port (Text (Colon + 1 .. Dash - 1), Item.First_Port, OK);
         if OK then
            Parse_Port (Text (Dash + 1 .. Text'Last), Item.Last_Port, OK);
         end if;
      end if;
      if not OK or else Item.Last_Port < Item.First_Port then
         Item := (others => <>);
         return;
      end if;

      Parse_Host (Text (Text'First .. Colon - 1), Item, OK);
      if not OK then
         Item := (others => <>);
         return;
      end if;
      Success := True;
   end Parse;

   function Allows
     (Item : Scope; Name : String; Port : Unsigned_16) return Boolean is
   begin
      if Port < Item.First_Port or else Port > Item.Last_Port
        or else Item.First_Port = 0 or else not Valid_Name (Name)
      then
         return False;
      end if;
      case Item.Kind is
         when Any_Name =>
            return True;
         when Exact =>
            return Item.Length = Name'Length and then
              (for all I in 1 .. Item.Length =>
                 Lower (Name (Name'First + I - 1)) = Item.Name (I));
         when Suffix =>
            --  At least one label, then '.', then the suffix.
            if Item.Length = 0 or else Name'Length < Item.Length + 2 then
               return False;
            end if;
            declare
               Offset : constant Natural := Name'Length - Item.Length;
            begin
               return Name (Name'First + Offset - 1) = '.' and then
                 (for all I in 1 .. Item.Length =>
                    Lower (Name (Name'First + Offset + I - 1)) =
                      Item.Name (I));
            end;
      end case;
   end Allows;
end CuBit.TLS_Scopes;
