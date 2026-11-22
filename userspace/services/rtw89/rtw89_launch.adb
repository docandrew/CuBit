------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Launch_Configuration;
with CuBit.Launch_Configurations;
with CuBit.Handles.Devices;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Driver_Readiness;
with RTW89_Registers;
with RTW89_Layout;
with Startup;

package body RTW89_Launch is

   package LC renames Launch_Configuration;

   --  The register window's place in our address space (no fixed ABI; the
   --  driver picks, docs/device-matching.md).
   Registers_Virtual_Base : constant Unsigned_64 := 16#6000_0000_0000#;
   Page_Bytes : constant := 4096;
   --  The first device-memory request of the manifest: BAR2.
   Register_Window : constant LC.Window_Index := 1;
   --  How long devmgr may take to receive the readiness report.
   Report_Deadline_Ms : constant := 5_000;

   procedure Acquire (Device : out Resources; Status : out Acquire_Status) is
      Config : LC.Generic_PCI_Configuration;
      Valid  : Boolean;
      Window : LC.Physical_Span;
      Mapped_Bytes : Unsigned_64;
   begin
      Device := (others => <>);
      CuBit.Launch_Configurations.Get (Config, Valid);
      if not Valid then
         Status := No_Configuration;
         return;
      end if;
      Device.Location := Config.Location;
      Device.Vendor := Config.Vendor;
      Device.Device := Config.Device;
      Device.Revision := Config.Revision;
      Device.Interrupt_Mode :=
        (case Config.Delivery is
           when LC.Polled => Polled, when LC.Line => Line,
           when LC.MSI => MSI, when LC.MSI_X => MSI_X);
      Device.Vectors := Config.Vectors;

      if Config.Window_Total < Register_Window then
         Status := No_Register_Window;
         return;
      end if;
      Window := Config.Windows (Register_Window);
      Device.Registers_Physical := Window.First;
      Device.Registers_Bytes := Window.Bytes;
      if Window.Bytes < RTW89_Registers.Register_Window_Bytes then
         Status := Window_Too_Small;
         return;
      end if;

      if Config.DMA.Bytes = 0 or else Config.DMA_Virtual = 0 then
         Status := No_DMA_Region;
         return;
      end if;
      Device.DMA_Physical := Config.DMA.First;
      Device.DMA_Bytes := Config.DMA.Bytes;
      Device.DMA_Virtual := Config.DMA_Virtual;
      if Config.DMA.Bytes < RTW89_Layout.Region_Bytes then
         Status := DMA_Too_Small;
         return;
      end if;

      --  Map only the registers we use: the MAC, then the baseband and RF.
      Mapped_Bytes := RTW89_Registers.Register_Window_Bytes;
      if not CuBit.Handles.Devices.Map_Device
        (Startup.Registers, Window.First, Registers_Virtual_Base,
         Mapped_Bytes / Page_Bytes, Read_Only => False)
      then
         Status := Map_Refused;
         return;
      end if;
      Device.Registers_Virtual := Registers_Virtual_Base;
      Status := Acquired;
   end Acquire;

   procedure Report (Label : Unsigned_32) is
      Ignore : MessageTag;
   begin
      Ignore := capSend
        (Startup.Device_Manager,
         (tag => (label => Label, length => 0, protocol => 0, ipc => <>,
                  reserved => 0),
          authorityTag => 0,
          words => [others => 0]),
         Deadline_After (Report_Deadline_Ms));
   end Report;

   procedure Report_Ready is
   begin
      Report (CuBit.Driver_Readiness.Ready_Label);
   end Report_Ready;

   procedure Report_Not_Present is
   begin
      Report (CuBit.Driver_Readiness.Not_Present_Label);
   end Report_Not_Present;

end RTW89_Launch;
