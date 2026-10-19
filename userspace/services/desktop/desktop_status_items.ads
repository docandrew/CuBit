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
--  and nothing is faked. Today only Volume has one (the mixer, through
--  Desktop's "mixer" startup handle). Network (NET-WIFI-001), Bluetooth,
--  Battery and Power_Profile (KERN-004, LAP-001) and Brightness are typed
--  here so they slot in when their services exist: Desktop then sets their
--  presence from its handles and gives them panel rows.
------------------------------------------------------------------------------
with Desktop_Icons;

package Desktop_Status_Items with SPARK_Mode, Pure is
   type Item is (Network, Bluetooth, Battery, Power_Profile, Brightness, Volume);
   type Presence is array (Item) of Boolean;
   Nothing_Shown : constant Presence := [others => False];

   --  The item's icon; Muted selects Volume's muted form.
   function Icon (Of_Item : Item; Muted : Boolean) return Desktop_Icons.Icon_ID is
     (case Of_Item is
         when Network       => Desktop_Icons.Status_Wifi,
         when Battery       => Desktop_Icons.Status_Battery,
         when Volume        =>
           (if Muted then Desktop_Icons.Status_Muted else Desktop_Icons.Status_Volume),
         when Bluetooth | Power_Profile | Brightness => Desktop_Icons.Settings);

   --  The item's name (its tooltip); Muted names Volume's muted form.
   function Name (Of_Item : Item; Muted : Boolean) return String is
     (case Of_Item is
         when Network       => "Network",
         when Bluetooth     => "Bluetooth",
         when Battery       => "Battery",
         when Power_Profile => "Power profile",
         when Brightness    => "Brightness",
         when Volume        => (if Muted then "Volume (muted)" else "Volume"));

   --  How many items Shown holds.
   function Count (Shown : Presence) return Natural
     with Post => Count'Result <= Item'Pos (Item'Last) + 1;
end Desktop_Status_Items;
