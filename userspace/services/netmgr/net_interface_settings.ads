------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One interface's setting, `net.interface.<name>` (the setting catalog's
--  codec, CAP-003): a typed CCL Net_Interface (CCL.Interfaces.Net_Interface)
--  stored as its canonical source, for example
--
--    Net_Interface.Automatic
--    (Net_Interface.Static (Static_Interface address => "::ffff:10.0.2.15"
--       prefix => 120 gateway => (Gateway.Via "::ffff:10.0.2.2") metric => 100))
--
--  and the interface's name (Name_For), which is the family's member.
--
--  netstack's configuration carries IPv4 so far: Decode accepts a Static
--  value only with a mapped address (and gateway, when given) and a prefix
--  of at least 96 (an IPv4 prefix length plus 96), and refuses anything
--  else, which netmgr then logs; IPv6 addresses come from SLAAC.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Net_Link;
with CuBit.Net_Management;

package Net_Interface_Settings is
   package NL renames CuBit.Net_Link;
   package NM renames CuBit.Net_Management;

   type Policy is (Automatic, Static, Disabled);

   type Interface_Setting (Kind : Policy := Automatic) is record
      case Kind is
         when Static => Settings : NM.Configuration;
         when Automatic | Disabled => null;
      end case;
   end record;
   --  What an absent setting means.
   Default : constant Interface_Setting := (Kind => Automatic);

   --  The setting catalog's codec.
   subtype Value is Interface_Setting;
   procedure Decode (Source : String; Item : out Value; Success : out Boolean);

   --  The interface's stable name, the family's member: CuBit.Net_Link's
   --  Name_Of (pci-<bus>-<device>-<function>, or mac-<12 hex digits> from
   --  the address seen when it attached), the name netstack logs it by.
   function Name_For (Identity : NL.Stable_Identity; Address : NL.Hardware_Address) return String is
     (NL.Name_Of (Identity, Address).Text (1 .. NL.Name_Of (Identity, Address).Length));
end Net_Interface_Settings;
