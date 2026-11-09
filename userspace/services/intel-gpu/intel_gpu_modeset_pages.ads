with Interfaces; use Interfaces;
--  Page-granularity authority over the display registers a modeset and the
--  DDC/EDID reads need beyond the power, PHY and pipe pages: GMBUS (0xC5000),
--  the four transcoder pages (0x60000 + 0x1000 * transcoder: timings,
--  PIPESRC, TRANS_DDI_FUNC_CTL) and the combo PLL configuration page
--  (0x164000: DPLL CFGCR0/1, DPCLKA_CFGCR0). Not per-register isolation and
--  not a power reference. Requested only by the display owner (devmgr
--  grants them like the pipe pages), only for what this boot uses.
--
--  South_Hotplug (0xC4000, DISP-009): the PCH display page with the
--  hot-plug registers SDEISR/SDEIMR/SDEIIR/SDEIER (0xC4000..0xC400C) and
--  SHOTPLUG_CTL_DDI/TC (0xC4030/0xC4034). The page also holds other south
--  display registers (other SDE controls, SHPD filter, south chicken
--  bits); intel-gpu touches only the hot-plug ones. That is a style rule
--  of the driver (Intel_GPU_Hotplug_IRQ names every register it uses), not
--  isolation: the grant is the whole page.
package Intel_GPU_Modeset_Pages with SPARK_Mode, Pure is
   Request_Label : constant := 16#023B#;
   Virtual_Base : constant Unsigned_64 := 16#6170_0000#;
   Page_Bytes : constant := 4_096;
   type Page is (GMBUS, Transcoder_A, Transcoder_B, Transcoder_C, Transcoder_D, PLL_Configuration,
                 South_Hotplug);
   subtype Transcoder_Page is Page range Transcoder_A .. Transcoder_D;
   GMBUS_Offset : constant := 16#C_5000#;
   PLL_Configuration_Offset : constant := 16#16_4000#;
   South_Hotplug_Offset : constant := 16#C_4000#;
   function Offset (Item : Page) return Unsigned_64 is
     (case Item is
        when GMBUS => GMBUS_Offset,
        when Transcoder_A .. Transcoder_D =>
          16#6_0000# + 16#1000# * Unsigned_64 (Page'Pos (Item) - Page'Pos (Transcoder_A)),
        when PLL_Configuration => PLL_Configuration_Offset,
        when South_Hotplug => South_Hotplug_Offset)
     with Post => Offset'Result mod Page_Bytes = 0 and Offset'Result < 16#20_0000#;
   function Address (Item : Page) return Unsigned_64 is
     (Virtual_Base + Page_Bytes * Unsigned_64 (Page'Pos (Item)));
   --  The wire index of a page (request word 0).
   function Index (Item : Page) return Unsigned_64 is (Unsigned_64 (Page'Pos (Item)));
end Intel_GPU_Modeset_Pages;
