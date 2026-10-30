------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The built-in codecs of the setting catalog's Text, Address and Count
--  values (userspace/ccl/interfaces/setting-catalog.ccl, CAP-003); a CCL
--  typed setting names its own codec package instead.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Net_Address;

package CuBit.Settings.Codecs is

   --  Text: stored as it is.
   package Text_Codec is
      subtype Value is Text;
      procedure Decode (Source : String; Item : out Value; Success : out Boolean);
      function Encode (Item : Value) return String is (Image (Item));
   end Text_Codec;

   --  Address: one 16-byte IPv6 address (IPv4 held as ::ffff:a.b.c.d),
   --  stored as RFC 4291 text; a dotted IPv4 literal is read as mapped.
   package Address_Codec is
      subtype Value is CuBit.Net_Address.Address;
      procedure Decode (Source : String; Item : out Value; Success : out Boolean);
      --  "::ffff:a.b.c.d" for a mapped address, else eight hexadecimal groups.
      function Encode (Item : Value) return String;
   end Address_Codec;

   --  Count: a decimal natural number.
   package Count_Codec is
      subtype Value is Natural;
      procedure Decode (Source : String; Item : out Value; Success : out Boolean);
      function Encode (Item : Value) return String;
   end Count_Codec;

end CuBit.Settings.Codecs;
