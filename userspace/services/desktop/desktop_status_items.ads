------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What the Deskbar's status button can show (docs/development-backlog.md
--  UI-027), in the order it shows them.
--
--  @description
--  An item is shown only while the service or driver behind it exists and
--  Desktop holds a granted capability for it; nothing is polled ambiently
--  and nothing is faked. Volume follows the mixer (Desktop's
--  "mixer-control" startup handle); Wired and Wifi follow netmgr's and
--  wifi.svc's reports (Desktop_Network_Model: Wifi only while a Wi-Fi
--  adapter is bound, Wired only while an Ethernet interface exists).
--  Bluetooth, Battery and Power_Profile (KERN-004, LAP-001) and
--  Brightness are typed here so they slot in when their services exist;
--  until then they stay hidden. Wired and Wifi are drawn as glyphs (link,
--  signal bars), not icons. Screen_Capture (who is acting, REMOTE-001) is
--  shown for a few seconds after the remote session took a frame through
--  Desktop's display-capture endpoint (Desktop_Capture_Policy.Mark_Ms); a
--  glyph too (a screen with a recording dot).
------------------------------------------------------------------------------
with Desktop_Icons;

package Desktop_Status_Items with SPARK_Mode, Pure is
   type Item is
     (Screen_Capture, Wired, Wifi, Bluetooth, Battery, Power_Profile, Brightness, Volume);
   type Presence is array (Item) of Boolean;
   Nothing_Shown : constant Presence := [others => False];

   --  The item's icon; Muted selects Volume's muted form.
   function Icon (Of_Item : Item; Muted : Boolean) return Desktop_Icons.Icon_ID is
     (case Of_Item is
         when Wired | Wifi | Screen_Capture => Desktop_Icons.Status_Wifi,
         when Battery       => Desktop_Icons.Status_Battery,
         when Volume        =>
           (if Muted then Desktop_Icons.Status_Muted else Desktop_Icons.Status_Volume),
         when Bluetooth | Power_Profile | Brightness => Desktop_Icons.Settings);

   --  The item's name (its tooltip); Muted names Volume's muted form.
   function Name (Of_Item : Item; Muted : Boolean) return String is
     (case Of_Item is
         when Screen_Capture => "Screen captured by the remote session",
         when Wired         => "Wired network",
         when Wifi          => "Wi-Fi",
         when Bluetooth     => "Bluetooth",
         when Battery       => "Battery",
         when Power_Profile => "Power profile",
         when Brightness    => "Brightness",
         when Volume        => (if Muted then "Volume (muted)" else "Volume"));

   --  Drawn as a glyph rather than an icon.
   function Glyph (Of_Item : Item) return Boolean is (Of_Item in Screen_Capture | Wired | Wifi);

   --  How many items Shown holds.
   function Count (Shown : Presence) return Natural
     with Post => Count'Result <= Item'Pos (Item'Last) + 1;
end Desktop_Status_Items;
