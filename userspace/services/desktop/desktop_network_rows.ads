------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The rows of the Deskbar panel's Network section (UI-027), in order:
--  each wired interface (its addressing editor under it when open), then,
--  while a Wi-Fi adapter is bound, the Wi-Fi interface's row (its editor)
--  and the scanned networks, a secured network's passphrase field right
--  under it. Mouse, keyboard and drawing all walk this one list.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Net_Manager_Report;
with Desktop_Network_Model;
with CuBit.UI_Metrics;

package Desktop_Network_Rows with SPARK_Mode is
   package NMR renames CuBit.Net_Manager_Report;
   package DNM renames Desktop_Network_Model;

   type Row_Kind is
     (Wired_Row, Wifi_Row, Network_Row, Passphrase_Row,
      Method_Row, Address_Row, Gateway_Row, DNS_Row, CCL_Row, Action_Row);
   subtype Editor_Row is Row_Kind range Method_Row .. Action_Row;
   --  Rows that take typed text.
   subtype Field_Row is Row_Kind range Address_Row .. DNS_Row;

   --  Index: the interface's place in netmgr's report (Wired_Row,
   --  Wifi_Row, editor rows; 0 for a Wi-Fi row without an interface yet),
   --  or the network's place in the list (Network_Row, Passphrase_Row).
   type Row is record
      Kind  : Row_Kind := Wired_Row;
      Index : Natural := 0;
   end record;

   --  Named ceilings on what one panel lists (the rest are not shown).
   Visible_Wired    : constant := 2;
   Visible_Networks : constant := 6;
   Editor_Rows      : constant := Row_Kind'Pos (Action_Row) - Row_Kind'Pos (Method_Row) + 1;
   Maximum_Rows     : constant :=
     Visible_Wired + Editor_Rows + 1 + Visible_Networks + 1;
   subtype Row_Count is Natural range 0 .. Maximum_Rows;
   subtype Row_Index is Positive range 1 .. Maximum_Rows;
   type Row_List is array (Row_Index) of Row;

   type Layout is record
      Rows  : Row_List := [others => (others => <>)];
      Count : Row_Count := 0;
   end record;

   --  Net: netmgr's report (Have_Net: one arrived); Wifi_Shown: the Wi-Fi
   --  item is shown; Networks: how many the list has; Passphrase_For: the
   --  network whose field is open (0: none); Editor_For: the interface
   --  whose editor is open (0: none). One editor is open at a time.
   function Build (Net : NMR.Report; Have_Net, Wifi_Shown : Boolean;
                   Networks : DNM.Network_Count; Passphrase_For : Natural;
                   Editor_For : NMR.Record_Count) return Layout;

   --  Pixel heights, from the UI em: a row holds a control with Space_S
   --  above and below; the CCL row two code lines, Space_XS apart.
   Row_Height : constant := CuBit.UI_Metrics.Control_Height + 2 * CuBit.UI_Metrics.Space_S;
   CCL_Height : constant := 2 * CuBit.UI_Metrics.Line_Height + 3 * CuBit.UI_Metrics.Space_XS;
   function Height_Of (K : Row_Kind) return Positive is
     (if K = CCL_Row then CCL_Height else Row_Height);

   function Total_Height (L : Layout) return Natural
   with Post => Total_Height'Result <= Maximum_Rows * CCL_Height;
end Desktop_Network_Rows;
