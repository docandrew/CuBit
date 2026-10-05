------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The names netstack opens (docs/c-removal.md): "@net:tcp:<host>:<port>"
--  for a connection and "@net:tcp-listen:<address>:<port>" for a listener,
--  where the host is a dotted IPv4 address or a name netstack resolves in
--  the scope.
--
--  @description
--  Proved (tests/libc-ada): the result fits Target_Bytes, and a target
--  longer than netstack takes (Maximum_Target) is refused rather than cut.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Libc_Net_Addresses;

package CuBit.Libc_Net_Targets with Pure, SPARK_Mode is

   Maximum_Target : constant := 255;     --  CuBit.Net_Channel_Layout.Target_Maximum
   Target_Bytes   : constant := 300;
   subtype Target_Length is Natural range 0 .. Target_Bytes;
   subtype Target is String (1 .. Target_Bytes);

   Connect_Prefix : constant String := "@net:tcp:";
   Listen_Prefix  : constant String := "@net:tcp-listen:";

   --  a.b.c.d (at most 15 characters).
   Dotted_Bytes : constant := 15;
   procedure Dotted (IPv4 : CuBit.Libc_Net_Addresses.Octets;
                     Result : out String; Length : out Natural)
   with Pre => Result'First = 1 and then Result'Length >= Dotted_Bytes,
        Post => Length in 7 .. Dotted_Bytes;

   procedure Format
     (Prefix, Host : String; Port : Unsigned_16;
      Result : out Target; Length : out Target_Length; Fits : out Boolean)
   with Pre => Prefix'Length <= Listen_Prefix'Length and then Host'Length <= 256,
        Post => (if Fits then Length in 1 .. Maximum_Target else Length = 0);

end CuBit.Libc_Net_Targets;
