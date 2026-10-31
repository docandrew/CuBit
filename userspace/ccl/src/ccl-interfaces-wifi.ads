with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  What the Wi-Fi radios heard (NET-WIFI-001 W1): (wifi.scan) lists the
--  networks of every wireless interface's last passive scan, strongest
--  first, as typed records. It reads what the driver published through
--  netstack (CuBit.Net_Management.Scan_Results, CuBit.Wifi_Scan); it does
--  not start a scan. (wifi.join), (wifi.leave), (wifi.status) and
--  (wifi.passphrase) go to wifi.svc (CuBit.Wifi_Control). The types are CCL
--  source (TYPE_SOURCE), checked by the CCL type checker when the
--  interface is published; the keys are SHA-256 of TYPE_SOURCE, then "#"
--  and the type's name (tests/ccl-console checks them).
package CCL.Interfaces.Wifi with SPARK_Mode is

   --  What Publish adds to a catalog: one interface, this many data
   --  schemas (Wifi_Network, List<Wifi_Network>, Wifi_Status) and this many
   --  types in the catalog's registry (Wifi_Band, Wifi_Security, Signal_dBm,
   --  Wifi_Network, List<Wifi_Network>, Wifi_State, Wifi_Status);
   --  CCL_Host_Environment sums these
   --  to check the catalog's capacity at compile time.
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 3;
   PUBLISHED_TYPES : constant := 7;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Wifi_Band (enum Band_2_4_GHz Band_5_GHz)) (type Wifi_Security (enum Open " &
     "Legacy_Protected RSN)) (type Signal_dBm (range -128 0)) (type Wifi_Network (record " &
     "(interface String) (bssid String) (ssid String) (band Wifi_Band) (channel Integer) " &
     "(signal Signal_dBm) (security Wifi_Security) (capability Integer) (heard Integer))) " &
     "(type Wifi_State (enum No_Radio Idle Joining Handshaking Connected)) (type Wifi_Status " &
     "(record (state Wifi_State) (last_failure String) (bssid String) (band Wifi_Band) " &
     "(channel Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#5BE2_3C6A_B2C7_B638#,
      16#D2DE_0F4A_CC97_12F6#,
      16#D409_BF5C_7DA7_CABA#,
      16#9EE0_88E4_F7AD_AD2A#];
   NETWORK_KEY : constant CCL.Objects.Schema_Key :=
     [16#0B91_7D08_FFF3_D11C#,
      16#7C1F_FF84_55E1_E837#,
      16#2239_D4D5_195A_95B0#,
      16#7E82_91C9_2F3B_0E0A#];
   NETWORKS_KEY : constant CCL.Objects.Schema_Key :=
     [16#6EDF_C774_8596_232E#,
      16#6592_8701_49C9_1024#,
      16#5865_7659_9C78_C64C#,
      16#84D3_D98B_CFF2_5D99#];
   STATUS_KEY : constant CCL.Objects.Schema_Key :=
     [16#1EBC_4A04_15E1_1195#,
      16#7C13_0562_BC43_679D#,
      16#18D8_0638_06D7_CCA1#,
      16#B8E0_0E5A_AEFA_AAE5#];

   type Band is (Band_2_4_GHz, Band_5_GHz);
   --  What the beacon says about protection: no Privacy bit (Open), the
   --  Privacy bit without an RSN element (WEP or WPA1, which CuBit will
   --  refuse to join), or an RSN element (WPA2/WPA3).
   type Security is (Open, Legacy_Protected, RSN);

   NETWORK_FIELDS : constant := 9;
   --  A Wifi_Network takes its product cell, three String cells, two cells
   --  of band, an Integer, a Signal_dBm, two cells of security and two
   --  Integers; a listing's count cell comes first.
   NETWORK_CELLS : constant := 1 + 3 + 2 + 1 + 1 + 2 + 2;
   MAXIMUM_LISTED : constant := (CCL.Objects.Maximum_Cells - 1) / NETWORK_CELLS;

   --  The connection (W3/W4, through wifi.svc): Join an SSID from the last
   --  scan, Leave, the Status; Passphrase asks the console for a network's
   --  passphrase without echo or history and hands it to wifi.svc (the
   --  Secret setting wifi.secret.<SSID in hex>), never to the program.
   type State is (No_Radio, Idle, Joining, Handshaking, Connected);
   STATUS_FIELDS : constant := 5;
   MAXIMUM_SSID_ARGUMENT : constant := 32;
   type Operation is (Scan, Join, Leave, Status, Passphrase);
   function Name (Item : Operation) return String is
     (case Item is
        when Scan => "scan", when Join => "join", when Leave => "leave", when Status => "status",
        when Passphrase => "passphrase");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Wifi);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Network, Networks, Connection : CCL.Objects.Binding;
   end record;
   --  Check TYPE_SOURCE and bind Wifi_Network, List<Wifi_Network> and
   --  Wifi_Status.
   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Wifi;
