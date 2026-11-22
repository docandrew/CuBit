------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Domain names in DNS wire form (RFC 1035 3.1): encoding a host name into
--  a query, and reading the question name back from a response.
--
--  Encode accepts a host name of labels of 1 to 63 letters, digits or
--  hyphens (RFC 1123 2.1), separated by single dots, at most 255 bytes in
--  wire form; it writes lower case. Read_Name accepts only plain labels
--  (a compression pointer is refused) within the message and the same
--  length limits, and lowers case, so the two can be compared byte for
--  byte (DNS names are case-insensitive).
--
--  Proved (tests/net-tcp): no access outside the buffers; the wire length
--  never exceeds 255 and ends with the root label; every label length is
--  1 .. 63.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package DNS_Name with SPARK_Mode is

   Maximum_Wire  : constant := 255;
   Maximum_Label : constant := 63;

   type Bytes is array (Natural range <>) of Unsigned_8;
   subtype Wire_Length is Natural range 0 .. Maximum_Wire;
   subtype Wire is Bytes (0 .. Maximum_Wire - 1);

   function Host_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-');

   function Lower (B : Unsigned_8) return Unsigned_8 is
     (if B in Character'Pos ('A') .. Character'Pos ('Z') then B + 32 else B);

   procedure Encode (Host : String; Name : out Wire; Length : out Wire_Length; OK : out Boolean)
   with
     Pre  => Host'Last < Natural'Last,
     Post => (if OK then Length >= 2 and then Name (Length - 1) = 0);

   --  The name at Message (Offset ..); on success Offset is just past it.
   procedure Read_Name (Message : Bytes; Offset : in out Natural; Name : out Wire;
                        Length : out Wire_Length; OK : out Boolean)
   with
     Pre  => Message'First = 0 and then Message'Length <= 2 ** 16,
     Post => (if OK then Length >= 1 and then Name (Length - 1) = 0 and then
                         Offset <= Message'Length and then Offset > Offset'Old);

end DNS_Name;
