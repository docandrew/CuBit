------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Register access to the RTL8852BE's BAR2 window, plus the bounded polls
--  and the indirect paths (crystal serial interface, MDIO to the PCIe PHY,
--  DBI to device configuration space) every bring-up step uses.
--
--  @description
--  The window is mapped once by the driver (RTW89_Launch). Every wait is
--  bounded twice: by a monotonic-clock deadline and by an iteration cap, so
--  a stopped clock or a dead device cannot hang the driver. Poll timeouts
--  are rtw89's (read_poll_timeout arguments), always passed explicitly.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with RTW89_Registers; use RTW89_Registers;

package RTW89_MMIO is

   --  Virtual address of the mapped window (set once by Attach).
   procedure Attach (Virtual : Unsigned_64);
   function Attached return Boolean;

   function Read32 (Offset : Register) return Unsigned_32
     with Pre => Attached;
   function Read16 (Offset : Register) return Unsigned_16
     with Pre => Attached;
   function Read8 (Offset : Register) return Unsigned_8
     with Pre => Attached;
   procedure Write32 (Offset : Register; Value : Unsigned_32)
     with Pre => Attached;
   procedure Write16 (Offset : Register; Value : Unsigned_16)
     with Pre => Attached;
   procedure Write8 (Offset : Register; Value : Unsigned_8)
     with Pre => Attached;

   procedure Set32 (Offset : Register; Bits : Unsigned_32) with Pre => Attached;
   procedure Clear32 (Offset : Register; Bits : Unsigned_32) with Pre => Attached;
   procedure Set16 (Offset : Register; Bits : Unsigned_16) with Pre => Attached;
   procedure Clear16 (Offset : Register; Bits : Unsigned_16) with Pre => Attached;
   procedure Set8 (Offset : Register; Bits : Unsigned_8) with Pre => Attached;
   procedure Clear8 (Offset : Register; Bits : Unsigned_8) with Pre => Attached;
   --  Replace the field Mask (already shifted) with Value shifted by Shift.
   procedure Field32 (Offset : Register; Mask : Unsigned_32; Shift : Natural;
                      Value : Unsigned_32) with Pre => Attached;

   --  Microsecond budgets for polls, as rtw89 passes them.
   subtype Microseconds is Unsigned_64 range 1 .. 10_000_000;

   --  Wait until (Read32 (Offset) and Mask) = Expected.
   function Poll32 (Offset : Register; Mask, Expected : Unsigned_32;
                    Budget : Microseconds) return Boolean
     with Pre => Attached;
   function Poll16 (Offset : Register; Mask, Expected : Unsigned_16;
                    Budget : Microseconds) return Boolean
     with Pre => Attached;
   function Poll8 (Offset : Register; Mask, Expected : Unsigned_8;
                   Budget : Microseconds) return Boolean
     with Pre => Attached;

   --  Busy-wait at least Budget microseconds (udelay in rtw89).
   procedure Delay_For (Budget : Microseconds);

   --  Sleep at least Milliseconds (fsleep/mdelay in rtw89).
   subtype Milliseconds is Unsigned_64 range 1 .. 1_000;
   procedure Sleep (Duration : Milliseconds);

   --  Crystal serial interface (rtw89_mac_write/read_xtal_si_ax).
   procedure XTAL_Write (Offset : XTAL_Offset; Value, Mask : Unsigned_8;
                         OK : out Boolean) with Pre => Attached;
   procedure XTAL_Read (Offset : XTAL_Offset; Value : out Unsigned_8;
                        OK : out Boolean) with Pre => Attached;

   --  PCIe PHY through MDIO (rtw89_read16_mdio / rtw89_write16_mdio).
   type PHY_Speed is (Gen1, Gen2);
   procedure MDIO_Read (Address : Unsigned_8; Speed : PHY_Speed;
                        Value : out Unsigned_16; OK : out Boolean)
     with Pre => Attached and then Address < 16#40#;
   procedure MDIO_Write (Address : Unsigned_8; Speed : PHY_Speed;
                         Value : Unsigned_16; OK : out Boolean)
     with Pre => Attached and then Address < 16#40#;

   --  Device configuration bytes through the DBI window
   --  (rtw89_dbi_read8 / rtw89_dbi_write8).
   subtype Config_Offset is Unsigned_16 range 0 .. 16#0FFF#;
   procedure DBI_Read (Address : Config_Offset; Value : out Unsigned_8;
                       OK : out Boolean) with Pre => Attached;
   procedure DBI_Write (Address : Config_Offset; Value : Unsigned_8;
                        OK : out Boolean) with Pre => Attached;

end RTW89_MMIO;
