------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What netmgr writes into a watcher's lent page (CuBit.Net_Manager_Control
--  Watch): every interface it manages, by stable name, with its kind, link,
--  policy, the IPv4 configuration in force and its DHCP client's phase, and
--  the DNS servers netstack resolves with.
--
--  @description
--  The page, in 32-bit little-endian words from its start:
--    0  the report's generation (never 0 once written)
--    1  records written (0 .. 15), interfaces in total (16 .. 31)
--    2  primary DNS, 3 secondary DNS (IPv4 words; 0: none)
--    4  where they came from (DNS_Source), 5 .. 7 zero
--  then one record of Record_Words words per interface:
--    0  name length (0 .. 7), kind (8 .. 15), condition (16 .. 23),
--       policy (24 .. 31)
--    1 .. 4  the name, four characters a word, zero past its length
--    5  address   6  prefix (0 .. 7), metric (16 .. 31)   7  gateway
--    8  hardware address octets 0 .. 3   9  octets 4 .. 5 (0 .. 15)
--    10 DHCP phase (0 .. 7), applies so far (16 .. 31)
--    11 .. 15 zero
--  Interfaces past Maximum_Records are counted in the total only. Decode is
--  total and accepts exactly what Encode writes.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Net_Link;
with CuBit.Net_Management;
with CuBit.Net_Manager_Control;

package CuBit.Net_Manager_Report with Pure, SPARK_Mode is
   package NL renames CuBit.Net_Link;
   package NM renames CuBit.Net_Management;

   type Policy is (Automatic, Static, Disabled);
   for Policy use (Automatic => 1, Static => 2, Disabled => 3);

   --  An Automatic interface's DHCP client (Not_Used for the others).
   type DHCP_Phase is
     (Not_Used, Idle, Selecting, Requesting, Bound, Renewing, Rebinding, Rebooting);
   for DHCP_Phase use
     (Not_Used => 0, Idle => 1, Selecting => 2, Requesting => 3, Bound => 4, Renewing => 5,
      Rebinding => 6, Rebooting => 7);

   type DNS_Source is (None, Settings, Lease, Override);
   for DNS_Source use (None => 0, Settings => 1, Lease => 2, Override => 3);

   subtype Apply_Count is Unsigned_32 range 0 .. 16#FFFF#;

   type Interface_Status is record
      Name      : NL.Interface_Name;
      Kind      : NL.Link_Kind := NL.Ethernet;
      Condition : NL.Link_Condition := NL.Down;
      Setting   : Policy := Automatic;
      Settings  : NM.Configuration;
      MAC       : NL.Hardware_Address := NL.No_Address;
      DHCP      : DHCP_Phase := Not_Used;
      Applies   : Apply_Count := 0;
   end record;

   First_Name_Character : constant Character := '!';
   Last_Name_Character  : constant Character := '~';

   --  The name's characters are printable and nonblank up to its length,
   --  blanks after (CuBit.Net_Link's padding).
   function Normal (S : Interface_Status) return Boolean is
     ((for all I in 1 .. NL.Name_Characters =>
         (if I <= S.Name.Length then S.Name.Text (I) in First_Name_Character .. Last_Name_Character
          else S.Name.Text (I) = ' '))
      and then (S.Setting = Automatic or else S.DHCP = Not_Used));

   Header_Words : constant := 8;
   Record_Words : constant := 16;
   Word_Bytes   : constant := 4;
   Page_Words   : constant := CuBit.Net_Manager_Control.Page_Bytes / Word_Bytes;
   --  How many records one page holds (a named ceiling: more interfaces
   --  are counted, not listed).
   Maximum_Records : constant := (Page_Words - Header_Words) / Record_Words;
   subtype Record_Count is Natural range 0 .. Maximum_Records;
   subtype Record_Index is Positive range 1 .. Maximum_Records;
   subtype Interface_Total is Natural range 0 .. 16#FFFF#;

   type Status_List is array (Record_Index) of Interface_Status;
   No_Status : constant Interface_Status := (others => <>);

   type Report is record
      Generation : Unsigned_32 := 0;
      Count      : Record_Count := 0;
      Total      : Interface_Total := 0;
      DNS_1      : NM.IPv4_Word := NM.No_IPv4;
      DNS_2      : NM.IPv4_Word := NM.No_IPv4;
      DNS_From   : DNS_Source := None;
      Interfaces : Status_List := [others => No_Status];
   end record;

   function Normal (R : Report) return Boolean is
     (R.Count <= R.Total
      and then (for all I in Record_Index =>
                  (if I <= R.Count then Normal (R.Interfaces (I))
                   else R.Interfaces (I) = No_Status)));

   subtype Word_Index is Natural range 0 .. Page_Words - 1;
   type Words is array (Word_Index) of Unsigned_32;

   function Encode (R : Report) return Words
   with Pre => Normal (R), Post => Encode'Result (0) = R.Generation;

   type Decoded_Report (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : Report;
         when False => null;
      end case;
   end record;

   function Decode (W : Words) return Decoded_Report
   with Post => (if Decode'Result.Valid then
                   Normal (Decode'Result.Item) and then Encode (Decode'Result.Item) = W);
end CuBit.Net_Manager_Report;
