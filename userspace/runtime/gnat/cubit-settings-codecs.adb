------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Locators;

package body CuBit.Settings.Codecs is

   package body Text_Codec is
      procedure Decode (Source : String; Item : out Value; Success : out Boolean) is
      begin
         Success := Source'Length <= Maximum_Text;
         Item := (if Success then To_Text (Source) else (others => <>));
      end Decode;
   end Text_Codec;

   package body Address_Codec is
      procedure Decode (Source : String; Item : out Value; Success : out Boolean) is
      begin
         Item := CuBit.Net_Address.Unspecified;
         Success := False;
         if Source'Length in 1 .. CuBit.Locators.Maximum_Length and then Source'Last < Natural'Last then
            CuBit.Locators.Parse_Address (Source, Item, Success);
         end if;
      end Decode;

      Hex : constant String := "0123456789abcdef";

      function Decimal (Byte : Unsigned_8) return String;
      function Decimal (Byte : Unsigned_8) return String is
         Image : constant String := Unsigned_8'Image (Byte);
      begin
         return Image (Image'First + 1 .. Image'Last);
      end Decimal;

      function Encode (Item : Value) return String is
      begin
         if CuBit.Net_Address.Is_Mapped (Item) then
            return "::ffff:" & Decimal (Item (12)) & "." & Decimal (Item (13)) & "." &
                   Decimal (Item (14)) & "." & Decimal (Item (15));
         end if;
         declare
            Result : String (1 .. 39);
            Next : Natural := 0;
         begin
            for Group in 0 .. 7 loop
               if Group > 0 then
                  Next := Next + 1;
                  Result (Next) := ':';
               end if;
               for Byte of Value'(Item) (2 * Group .. 2 * Group + 1) loop
                  Result (Next + 1) := Hex (Natural (Shift_Right (Byte, 4)) + 1);
                  Result (Next + 2) := Hex (Natural (Byte and 16#F#) + 1);
                  Next := Next + 2;
               end loop;
            end loop;
            return Result (1 .. Next);
         end;
      end Encode;
   end Address_Codec;

   package body Count_Codec is
      procedure Decode (Source : String; Item : out Value; Success : out Boolean) is
         Limit : constant := Natural'Last / 10;
      begin
         Item := 0;
         Success := Source'Length in 1 .. 9;
         for C of Source loop
            exit when not Success;
            if C not in '0' .. '9' or else Item > Limit then
               Success := False;
            else
               Item := Item * 10 + (Character'Pos (C) - Character'Pos ('0'));
            end if;
         end loop;
         if not Success then
            Item := 0;
         end if;
      end Decode;

      function Encode (Item : Value) return String is
         Image : constant String := Natural'Image (Item);
      begin
         return Image (Image'First + 1 .. Image'Last);
      end Encode;
   end Count_Codec;

end CuBit.Settings.Codecs;
