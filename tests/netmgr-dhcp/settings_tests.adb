--  Hosted tests of the typed net.interface.<name> setting: the CCL schema
--  accepts Automatic, Disabled and Static values within its bounds and
--  nothing else; netmgr's decoder reads the canonical form back and refuses
--  what netstack cannot use (an address that is not IPv4 mapped, a prefix
--  below 96); interface names are the stable ones.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Typed_Settings;
with CCL.Setting_Kinds;
with CuBit.Net_Link;
with CuBit.Net_Management;
with Net_Interface_Settings;

procedure Settings_Tests is
   package TS renames CCL.Typed_Settings;
   package S renames Net_Interface_Settings;
   package NL renames CuBit.Net_Link;
   package NM renames CuBit.Net_Management;
   use type TS.Setting_Kind, S.Policy, NM.Configuration;
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;
   function Canonical (Source : String) return String is
      Result : TS.Check_Result;
   begin
      TS.Check (CCL.Setting_Kinds.Net_Interface_Setting, Source, Result);
      return (if Result.Success then Result.Canonical (1 .. Result.Length) else "");
   end Canonical;
   function Accepted (Source : String) return Boolean is (Canonical (Source)'Length > 0);
   Item : S.Value;
   OK : Boolean;
   function IP (A, B, C, E : Unsigned_8) return NM.IPv4_Word is
     (Unsigned_32 (A) or Shift_Left (Unsigned_32 (B), 8) or
      Shift_Left (Unsigned_32 (C), 16) or Shift_Left (Unsigned_32 (E), 24));
   Static_Source : constant String :=
     "(Net_Interface.Static (Static_Interface address => ""::ffff:10.0.2.15"" prefix => 120 " &
     "gateway => (Gateway.Via ""::ffff:10.0.2.2"")))";
begin
   Check (TS.Kind_Of ("net.interface.pci-00-10-0") = CCL.Setting_Kinds.Net_Interface_Setting,
          "the interface key is typed");
   Check (TS.Kind_Of ("net.interface.") = CCL.Setting_Kinds.Untyped, "the bare prefix names no interface");
   Check (TS.Kind_Of ("net.dns.primary") = CCL.Setting_Kinds.Untyped, "DNS stays an address setting");

   Check (Accepted ("Net_Interface.Automatic"), "schema accepts Automatic");
   Check (Accepted ("Net_Interface.Disabled"), "schema accepts Disabled");
   Check (Accepted (Static_Source), "schema accepts a static value");
   Check (not Accepted ("(Net_Interface.Static (Static_Interface address => ""x"" prefix => 129))"),
          "prefix over 128 refused");
   Check (not Accepted ("(Net_Interface.Static (Static_Interface address => ""x"" prefix => 24 metric => 70000))"),
          "metric over 65535 refused");
   Check (not Accepted ("Net_Interface.Dhcp"), "unknown alternative refused");

   S.Decode (Canonical ("Net_Interface.Automatic"), Item, OK);
   Check (OK and then Item.Kind = S.Automatic, "automatic decoded");
   S.Decode (Canonical ("Net_Interface.Disabled"), Item, OK);
   Check (OK and then Item.Kind = S.Disabled, "disabled decoded");
   S.Decode (Canonical (Static_Source), Item, OK);
   Check (OK and then Item.Kind = S.Static and then
          Item.Settings = (Address => IP (10, 0, 2, 15), Prefix => 24, Gateway => IP (10, 0, 2, 2),
                           Metric => 100),
          "static decoded with the default metric: " & Canonical (Static_Source));
   S.Decode (Canonical ("(Net_Interface.Static (Static_Interface address => ""::ffff:192.168.1.7"" " &
                        "prefix => 112 metric => 5))"), Item, OK);
   Check (OK and then Item.Kind = S.Static and then
          Item.Settings = (Address => IP (192, 168, 1, 7), Prefix => 16, Gateway => 0, Metric => 5),
          "static without a gateway");
   S.Decode (Canonical ("(Net_Interface.Static (Static_Interface address => ""2001:db8::1"" prefix => 64))"),
             Item, OK);
   Check (not OK, "an IPv6 address is refused (configuration carries IPv4 only)");
   S.Decode (Canonical ("(Net_Interface.Static (Static_Interface address => ""::ffff:10.0.2.15"" prefix => 64))"),
             Item, OK);
   Check (not OK, "a prefix below 96 is refused for a mapped address");
   S.Decode (Canonical ("(Net_Interface.Static (Static_Interface address => ""10.0.2.15x"" prefix => 120))"),
             Item, OK);
   Check (not OK, "an address that does not parse is refused");
   S.Decode (Canonical ("(Net_Interface.Static (Static_Interface address => ""::ffff:10.0.2.15"" prefix => 120 " &
                        "gateway => (Gateway.Via ""2001:db8::1"")))"), Item, OK);
   Check (not OK, "an IPv6 gateway is refused");
   S.Decode ("Net_Interface.Automatic ", Item, OK);
   Check (not OK, "only the canonical form is read");

   Check (S.Name_For ((Kind => NL.PCI_Function, Location => 16#0080#), NL.No_Address) = "pci-00-10-0",
          "PCI name " & S.Name_For ((Kind => NL.PCI_Function, Location => 16#0080#), NL.No_Address));
   Check (S.Name_For ((Kind => NL.PCI_Function, Location => 16#3A1F#), NL.No_Address) = "pci-3a-03-7",
          "PCI name with every field");
   Check (S.Name_For ((Kind => NL.Factory_Address), [16#52#, 16#54#, 0, 16#AB#, 16#CD#, 16#EF#]) =
          "mac-525400abcdef", "factory address name");

   if Failures = 0 then
      Put_Line ("netmgr-settings: PASS");
   else
      Put_Line ("netmgr-settings: FAIL" & Failures'Image);
      raise Program_Error;
   end if;
end Settings_Tests;
