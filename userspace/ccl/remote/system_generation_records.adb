pragma Ada_2022;

package body System_Generation_Records is

   Hex_Digits : constant String (1 .. 16) := "0123456789abcdef";
   Quote : constant Character := '"';

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Value'Image;
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Hex (Item : Digest) return String is
      Text : String (1 .. Hex_Length);
   begin
      for I in Digest_Index loop
         Text (2 * I - 1) := Hex_Digits (Natural (Item (I) / 16) + 1);
         Text (2 * I) := Hex_Digits (Natural (Item (I) mod 16) + 1);
      end loop;
      return Text;
   end Hex;

   procedure From_Hex (Text : String; Item : out Digest; Valid : out Boolean) is
      function Nibble (C : Character) return Unsigned_8 is
        (if C in '0' .. '9' then Character'Pos (C) - Character'Pos ('0')
         else Character'Pos (C) - Character'Pos ('a') + 10);
   begin
      Item := [others => 0];
      Valid := Text'Length = Hex_Length
        and then (for all C of Text => C in '0' .. '9' | 'a' .. 'f');
      if Valid then
         for I in Digest_Index loop
            Item (I) := Nibble (Text (Text'First + 2 * I - 2)) * 16 + Nibble (Text (Text'First + 2 * I - 1));
         end loop;
      end if;
   end From_Hex;

   function Record_Text (G : Generation_Value) return String is
      function Pins_From (I : Positive) return String is
        (if I > G.Count then ""
         else (if I > 1 then " " else "") & "(Service_Pin service => " & Quote &
              G.Pins (I).Service (1 .. G.Pins (I).Length) & Quote & " executable => " & Quote &
              Hex (G.Pins (I).Program) & Quote & ")" & Pins_From (I + 1));
   begin
      return "(System_Generation number => " & Decimal (Unsigned_64 (G.Number)) &
        " previous => " & Decimal (Unsigned_64 (G.Previous)) & " pins => [" & Pins_From (1) & "])";
   end Record_Text;

   function Pin_Of (G : Generation_Value; Service : String) return Natural is
   begin
      for I in 1 .. G.Count loop
         if G.Pins (I).Service (1 .. G.Pins (I).Length) = Service then
            return I;
         end if;
      end loop;
      return 0;
   end Pin_Of;
end System_Generation_Records;
