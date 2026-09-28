------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Locator syntax (docs/security-model.md, "Names and locators"):
--  "@<authority>:<rest>". Each authority's grammar is built from the pieces
--  here: ':'-separated fields (a field in brackets may hold ':'), decimal
--  ports, names, path segments and addresses. A locator selects among
--  capabilities its holder has; nothing here grants anything.
--
--  Proved (tests/locators, level 1): no access outside the text and no
--  overflow however long a run of digits; an authority holds only word
--  characters and is followed by ':'; an unbracketed field never contains
--  ':'; a port is 1 .. 65535; a dotted IPv4 literal yields a mapped
--  address.
--  Tested, not proved: which texts parse as addresses and to what value,
--  against Linux's inet_pton (RFC 4291 text, dotted IPv4 with no leading
--  zeros) over 2,000,000 random and 24 chosen texts, with no disagreement.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Net_Address;

package CuBit.Locators with Pure, SPARK_Mode is

   Maximum_Length    : constant := 255;
   Maximum_Authority : constant := 32;
   Maximum_Name      : constant := 64;

   subtype Text_Length is Natural range 0 .. Maximum_Length;
   subtype Port_Number is Natural range 1 .. 65_535;

   function Is_Digit (C : Character) return Boolean is (C in '0' .. '9');
   function Is_Hex (C : Character) return Boolean is
     (C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F');
   --  An authority or protocol: lower-case letters, digits, '-'.
   function Is_Word (C : Character) return Boolean is
     (C in 'a' .. 'z' | '0' .. '9' | '-');
   --  A host or other name: letters, digits, '-', '.', '_'.
   function Is_Name_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_');

   --  "@<authority>:": Authority_Last ends the authority, Rest is where the
   --  rest begins.
   procedure Split
     (Text : String; Authority_Last : out Natural; Rest : out Positive;
      OK : out Boolean)
   with
     Pre  => Text'Length <= Maximum_Length and then
             Text'First = 1,
     Post => (if OK then
                Authority_Last in 2 .. Maximum_Authority + 1 and then
                Authority_Last < Text'Last and then
                Text (Authority_Last + 1) = ':' and then
                Rest = Authority_Last + 2 and then
                (for all I in 2 .. Authority_Last => Is_Word (Text (I))));

   --  The field starting at From: up to the next ':' or the end, or, when
   --  it starts with '[', the text inside the brackets (which must be
   --  followed by ':' or the end; Bracketed). Next is where the following
   --  field starts; At_End says there is none.
   procedure Next_Field
     (Text : String; From : Positive; First : out Positive;
      Last : out Natural; Next : out Positive; Bracketed : out Boolean;
      At_End : out Boolean; OK : out Boolean)
   with
     Pre  => Text'First = 1 and then Text'Length <= Maximum_Length and then
             From <= Text'Last + 1,
     Post => (if OK then
                First >= From and then Last <= Text'Last and then
                Last + 1 >= First and then
                Next > Last and then Next <= Text'Last + 1 and then
                (if At_End then Next = Text'Last + 1) and then
                (if not Bracketed then
                   (for all I in First .. Last => Text (I) /= ':')));

   procedure Parse_Port
     (Field : String; Port : out Port_Number; OK : out Boolean)
   with Pre => Field'Last < Natural'Last;

   --  1 .. Maximum_Name name characters.
   function Valid_Name (Field : String) return Boolean is
     (Field'Length in 1 .. Maximum_Name and then
      (for all C of Field => Is_Name_Character (C)));

   --  A path segment: a name, never "." or "..".
   function Valid_Segment (Field : String) return Boolean is
     (Valid_Name (Field) and then Field /= "." and then Field /= "..");

   --  A dotted IPv4 literal, as a mapped address.
   procedure Parse_IPv4
     (Field : String; Result : out CuBit.Net_Address.Address; OK : out Boolean)
   with Pre  => Field'Last < Natural'Last,
        Post => (if OK then CuBit.Net_Address.Is_Mapped (Result));

   --  RFC 4291 2.2 text ("2001:db8::1", "::ffff:10.0.2.2"), or a dotted
   --  IPv4 literal (held mapped).
   procedure Parse_Address
     (Field : String; Result : out CuBit.Net_Address.Address; OK : out Boolean)
   with Pre => Field'Last < Natural'Last and then
               Field'Length <= Maximum_Length;

end CuBit.Locators;
