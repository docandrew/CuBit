------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The network authority's locator (docs/control-language.md, "Locators
--  and authority kinds"): "@net:<protocol>:<host>:<port>". The protocol is
--  tcp, udp or tcp-listen; the host is a name, a dotted IPv4 literal, or an
--  IPv6 literal in brackets ("[2001:db8::1]"); the port is 1 .. 65535, and
--  nothing follows it. Built from CuBit.Locators; the text is the client's,
--  and this parse is all netstack believes of it.
--
--  Proved (tests/locators): as CuBit.Locators, plus: a valid locator's host
--  is either a name of 1 .. Maximum_Name name characters that is not an
--  address, or an address; a bracketed host is never a mapped IPv4 address
--  (IPv4 is written dotted).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Locators;
with CuBit.Net_Address;

package CuBit.Net_Locator with Pure, SPARK_Mode is

   type Protocol is (TCP, UDP, TCP_Listen);
   subtype Name_Length is Natural range 0 .. CuBit.Locators.Maximum_Name;

   type Target is record
      Valid      : Boolean := False;
      Proto      : Protocol := TCP;
      Is_Address : Boolean := False;
      Address    : CuBit.Net_Address.Address := CuBit.Net_Address.Unspecified;
      Name       : String (1 .. CuBit.Locators.Maximum_Name) :=
        [others => ' '];
      Name_Len   : Name_Length := 0;
      Port       : CuBit.Locators.Port_Number := 1;
   end record;

   procedure Parse (Text : String; Result : out Target)
   with
     Pre  => Text'First = 1 and then
             Text'Length <= CuBit.Locators.Maximum_Length,
     Post => (if Result.Valid and then not Result.Is_Address then
                Result.Name_Len >= 1 and then
                CuBit.Locators.Valid_Name
                  (Result.Name (1 .. Result.Name_Len)));

end CuBit.Net_Locator;
