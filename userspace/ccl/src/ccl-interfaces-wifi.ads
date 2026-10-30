with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  What the Wi-Fi radios heard (NET-WIFI-001 W1): (wifi.scan) lists the
--  networks of every wireless interface's last passive scan, strongest
--  first, as typed records. It reads what the driver published through
--  netstack (CuBit.Net_Management.Scan_Results, CuBit.Wifi_Scan); it does
--  not start a scan and never makes a radio transmit. The types are CCL
--  source (TYPE_SOURCE), checked by the CCL type checker when the
--  interface is published; the keys are SHA-256 of TYPE_SOURCE, then "#"
--  and the type's name (tests/ccl-console checks them).
package CCL.Interfaces.Wifi with SPARK_Mode is

   --  What Publish adds to a catalog: one interface, this many data
   --  schemas (Wifi_Network, List<Wifi_Network>) and this many types in
   --  the catalog's registry (Wifi_Band, Wifi_Security, Signal_dBm,
   --  Wifi_Network, List<Wifi_Network>); CCL_Host_Environment sums these
   --  to check the catalog's capacity at compile time.
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 2;
   PUBLISHED_TYPES : constant := 5;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Wifi_Band (enum Band_2_4_GHz Band_5_GHz)) (type Wifi_Security (enum Open " &
     "Legacy_Protected RSN)) (type Signal_dBm (range -128 0)) (type Wifi_Network (record " &
     "(interface String) (bssid String) (ssid String) (band Wifi_Band) (channel Integer) " &
     "(signal Signal_dBm) (security Wifi_Security) (capability Integer) (heard Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#3497_BAF5_E3F7_187E#,
      16#3185_D8B3_2170_E5E5#,
      16#437B_E476_06F7_1C11#,
      16#69EC_D852_4FDF_A4E8#];
   NETWORK_KEY : constant CCL.Objects.Schema_Key :=
     [16#F8E6_1055_33CE_3E40#,
      16#61B3_2EB2_C867_D51A#,
      16#0B23_B85D_D73C_0A1D#,
      16#C35B_E683_9CB9_DA16#];
   NETWORKS_KEY : constant CCL.Objects.Schema_Key :=
     [16#6871_8021_7EFE_114E#,
      16#6081_CC3C_238D_0302#,
      16#7818_F258_4EE4_44E1#,
      16#08F6_C79F_97D5_90BA#];

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

   type Operation is (Scan);
   function Name (Item : Operation) return String is (case Item is when Scan => "scan");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Wifi);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Network, Networks : CCL.Objects.Binding;
   end record;
   --  Check TYPE_SOURCE and bind Wifi_Network and List<Wifi_Network>.
   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Wifi;
