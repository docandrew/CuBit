------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  UFS host controller interface (JEDEC JESD223, UFSHCI 2.0 and later):
--  register map, register fields, UIC commands and attributes, and the
--  decoders for the identification registers. Pure: no MMIO here (HW-001).
--
--  The driver uses only what UFSHCI 2.0 has (the IdeaPad's 8086:54FF is
--  2.0): completions by doorbell-clear polling, the 2.0 register block
--  (everything below offset 16#A0#, so a 4 KiB BAR suffices) and 2.0's
--  interrupt sources. Later additions it does not use: UTRLCNR completion
--  notification and auto-hibernate (3.0), crypto (2.1), MCQ (4.0); the one
--  later interrupt source, the crypto engine's (2.1), is gated on version.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package UFSHCI with SPARK_Mode, Pure is
   ---------------------------------------------------------------------------
   --  Registers (UFSHCI 5.1). Offsets in bytes from BAR 0.
   ---------------------------------------------------------------------------
   type Register is
     (Capabilities, Version, Product_ID, Manufacturer_ID,
      Interrupt_Status, Interrupt_Enable,
      Controller_Status, Controller_Enable, UIC_Error_PHY_Adapter,
      UIC_Error_Data_Link, UIC_Error_Network, UIC_Error_Transport,
      UIC_Error_DME, Transfer_Interrupt_Aggregation,
      Transfer_List_Base_Low, Transfer_List_Base_High, Transfer_Doorbell,
      Transfer_List_Clear, Transfer_List_Run_Stop,
      Task_List_Base_Low, Task_List_Base_High, Task_Doorbell,
      Task_List_Clear, Task_List_Run_Stop,
      UIC_Command, UIC_Argument_1, UIC_Argument_2, UIC_Argument_3);
   for Register use
     (Capabilities                     => 16#00#,
      Version                          => 16#08#,
      Product_ID                       => 16#10#,
      Manufacturer_ID                  => 16#14#,
      Interrupt_Status                 => 16#20#,
      Interrupt_Enable                 => 16#24#,
      Controller_Status                => 16#30#,
      Controller_Enable                => 16#34#,
      UIC_Error_PHY_Adapter            => 16#38#,
      UIC_Error_Data_Link              => 16#3C#,
      UIC_Error_Network                => 16#40#,
      UIC_Error_Transport              => 16#44#,
      UIC_Error_DME                    => 16#48#,
      Transfer_Interrupt_Aggregation   => 16#4C#,
      Transfer_List_Base_Low           => 16#50#,
      Transfer_List_Base_High          => 16#54#,
      Transfer_Doorbell                => 16#58#,
      Transfer_List_Clear              => 16#5C#,
      Transfer_List_Run_Stop           => 16#60#,
      Task_List_Base_Low               => 16#70#,
      Task_List_Base_High              => 16#74#,
      Task_Doorbell                    => 16#78#,
      Task_List_Clear                  => 16#7C#,
      Task_List_Run_Stop               => 16#80#,
      UIC_Command                      => 16#90#,
      UIC_Argument_1                   => 16#94#,
      UIC_Argument_2                   => 16#98#,
      UIC_Argument_3                   => 16#9C#);

   --  The standard register block every controller implements.
   Register_Block_Bytes : constant := 16#100#;
   subtype Register_Offset is Natural range 0 .. Register_Block_Bytes - 4;

   function Offset (Item : Register) return Register_Offset is
     (Register'Enum_Rep (Item));

   type Register_Value is mod 2 ** 32;

   ---------------------------------------------------------------------------
   --  CAP: controller capabilities.
   ---------------------------------------------------------------------------
   Maximum_Transfer_Slots : constant := 32;
   Maximum_Task_Slots     : constant := 8;
   subtype Transfer_Slot_Count is Positive range 1 .. Maximum_Transfer_Slots;
   subtype Task_Slot_Count is Positive range 1 .. Maximum_Task_Slots;

   type Controller_Capabilities is record
      Transfer_Slots : Transfer_Slot_Count := 1;
      Task_Slots     : Task_Slot_Count := 1;
      Addressing_64  : Boolean := False;
   end record;

   function Decode_Capabilities (Value : Register_Value)
     return Controller_Capabilities;

   ---------------------------------------------------------------------------
   --  VER: interface version (major.minor, suffix).
   ---------------------------------------------------------------------------
   type Interface_Version is record
      Major  : Unsigned_8 := 0;
      Minor  : Unsigned_8 := 0;
      Suffix : Unsigned_8 := 0;
   end record;

   --  UFSHCI 1.x encodes VER differently (16#0001_0m00# for 1.m); it is
   --  decoded into the same major.minor (as Linux ufshcd_get_ufs_version).
   function Decode_Version (Value : Register_Value) return Interface_Version;

   function At_Least (Item : Interface_Version; Major, Minor : Unsigned_8)
     return Boolean is
     (Item.Major > Major or else (Item.Major = Major and then Item.Minor >= Minor));

   --  UFSHCI 2.0 and later: one command type (UFS storage) for every
   --  request, the transfer descriptor this driver writes. 1.x is declined.
   function Supported (Item : Interface_Version) return Boolean is
     (At_Least (Item, 2, 0));

   ---------------------------------------------------------------------------
   --  IS / IE: interrupt sources (UFSHCI 5.3.1).
   ---------------------------------------------------------------------------
   Transfer_Completed      : constant Register_Value := 2 ** 0;
   UIC_Error               : constant Register_Value := 2 ** 2;
   UIC_Power_Mode_Status   : constant Register_Value := 2 ** 4;
   UIC_Link_Lost           : constant Register_Value := 2 ** 7;
   UIC_Link_Startup        : constant Register_Value := 2 ** 8;
   Task_Completed          : constant Register_Value := 2 ** 9;
   UIC_Command_Completed   : constant Register_Value := 2 ** 10;
   Device_Fatal_Error      : constant Register_Value := 2 ** 11;
   UTP_Error               : constant Register_Value := 2 ** 12;
   Controller_Fatal_Error  : constant Register_Value := 2 ** 16;
   System_Bus_Fatal_Error  : constant Register_Value := 2 ** 17;
   --  UFSHCI 2.1 and later only (reserved before).
   Crypto_Engine_Fatal     : constant Register_Value := 2 ** 18;

   --  Errors after which the controller must be reset, on Item's version
   --  (Linux INTERRUPT_MASK_ALL_VER_11 / _VER_21).
   function Fatal_Errors (Item : Interface_Version) return Register_Value is
     (Device_Fatal_Error or Controller_Fatal_Error or System_Bus_Fatal_Error
      or UIC_Link_Lost
      or (if At_Least (Item, 2, 1) then Crypto_Engine_Fatal else 0));

   --  The sources this driver handles: completions and every error.
   function Handled_Interrupts (Item : Interface_Version)
     return Register_Value is
     (Transfer_Completed or Task_Completed or UIC_Error or UTP_Error
      or Fatal_Errors (Item));

   ---------------------------------------------------------------------------
   --  HCS: controller status (UFSHCI 5.3.3).
   ---------------------------------------------------------------------------
   Device_Present         : constant Register_Value := 2 ** 0;
   Transfer_List_Ready    : constant Register_Value := 2 ** 1;
   Task_List_Ready        : constant Register_Value := 2 ** 2;
   UIC_Command_Ready      : constant Register_Value := 2 ** 3;

   --  HCE: controller enable.
   Enable_Controller : constant Register_Value := 2 ** 0;
   --  UTRLRSR / UTMRLRSR: list run-stop.
   List_Running : constant Register_Value := 2 ** 0;

   ---------------------------------------------------------------------------
   --  UIC commands (UFSHCI 5.6.1, UniPro DME primitives).
   ---------------------------------------------------------------------------
   type UIC_Operation is
     (DME_Get, DME_Set, DME_Peer_Get, DME_Peer_Set, DME_Power_On,
      DME_Power_Off, DME_Enable, DME_Reset, DME_Endpoint_Reset,
      DME_Link_Startup, DME_Hibernate_Enter, DME_Hibernate_Exit,
      DME_Test_Mode);
   for UIC_Operation use
     (DME_Get             => 16#01#,
      DME_Set             => 16#02#,
      DME_Peer_Get        => 16#03#,
      DME_Peer_Set        => 16#04#,
      DME_Power_On        => 16#10#,
      DME_Power_Off       => 16#11#,
      DME_Enable          => 16#12#,
      DME_Reset           => 16#14#,
      DME_Endpoint_Reset  => 16#15#,
      DME_Link_Startup    => 16#16#,
      DME_Hibernate_Enter => 16#17#,
      DME_Hibernate_Exit  => 16#18#,
      DME_Test_Mode       => 16#1A#);

   function Command_Word (Operation : UIC_Operation) return Register_Value is
     (Register_Value (UIC_Operation'Enum_Rep (Operation)));

   --  UniPro attributes this driver reads or sets (MIPI UniPro, PHY
   --  adapter layer).
   type UIC_Attribute is
     (PA_Local_TX_LCC_Enable, PA_Connected_TX_Data_Lanes,
      PA_Connected_RX_Data_Lanes, PA_Max_RX_HS_Gear);
   for UIC_Attribute use
     (PA_Local_TX_LCC_Enable     => 16#155E#,
      PA_Connected_TX_Data_Lanes => 16#1561#,
      PA_Connected_RX_Data_Lanes => 16#1581#,
      PA_Max_RX_HS_Gear          => 16#1587#);

   --  UICCMDARG1: attribute in bits 31:16, generator selector in 15:0.
   function Attribute_Argument (Attribute : UIC_Attribute)
     return Register_Value is
     (Register_Value (UIC_Attribute'Enum_Rep (Attribute)) * 2 ** 16);

   --  UICCMDARG2 bits 7:0 after completion: the configuration result.
   Result_Code_Mask : constant Register_Value := 16#FF#;
   UIC_Success      : constant Register_Value := 0;

end UFSHCI;
