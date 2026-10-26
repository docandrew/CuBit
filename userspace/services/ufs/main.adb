------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  UFS host controller driver (UFSHCI over PCI; HW-001 step 3).
--
--  devmgr launches it for a matched controller (manifest `drives`, the
--  generic PCI launch of docs/device-matching.md). Bring-up logs one line
--  per stage: controller version, link up, device descriptor, units,
--  INQUIRY, capacity, partition table. Then it serves the first enabled
--  logical unit as a read-only Block.Device.V1 device.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Busy_Poll;
with CuBit.Driver_Readiness;
with CuBit.Handles.Devices;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with Startup;
with UFSHCI; use UFSHCI;
with UFS_Controller; use UFS_Controller;
with UFS_Identify;
with UFS_Layout;
with UFS_Quirks;
with UFS_Log;
with UFS_Service;
with UFS_Text; use UFS_Text;

procedure Main is
   package LC renames Launch_Configuration;
   use type LC.Interrupt_Delivery;

   --  Where this driver maps the controller's register window.
   Register_Virtual : constant := 16#6000_0000_0000#;
   Page_Bytes       : constant := 4096;
   Four_GiB : constant := 2 ** 32;
   Link_Startup_Attempts : constant := 3;
   --  How long to sleep between polls while a polled request is in flight.
   Poll_Interval_Milliseconds : constant := 1;

   procedure Report (Label : Unsigned_32) is
      Ignored : MessageTag;
   begin
      Ignored := capSend
        (Startup.Ready,
         (tag => (label => Label, length => 0, protocol => 0, ipc => <>,
                  reserved => 0),
          authorityTag => 0, words => [others => 0]),
         Wait_Forever);
   end Report;

   --  Declines the controller after a failed stage was logged.
   procedure Decline is
      Ignored : Unsigned_64;
   begin
      UFS_Log.Summary (", controller declined");
      Report (CuBit.Driver_Readiness.Not_Present_Label);
      Ignored := syscall (SYSCALL_EXIT);
   end Decline;

   --  The registers that explain a failed enable, UIC command or list start.
   function Controller_State return String is
     (UFS_Log.Hex_Field ("hce", Unsigned_64 (Read (Controller_Enable)), 8) &
      UFS_Log.Hex_Field ("hcs", Unsigned_64 (Read (Controller_Status)), 8) &
      UFS_Log.Hex_Field ("is", Unsigned_64 (Read (Interrupt_Status)), 8));

   function UIC_Errors return String is
     (UFS_Log.Hex_Field ("uic", Unsigned_64 (Last_UIC_Result), 2) &
      UFS_Log.Hex_Field ("pa", Unsigned_64 (Read (UIC_Error_PHY_Adapter)), 8) &
      UFS_Log.Hex_Field ("dl", Unsigned_64 (Read (UIC_Error_Data_Link)), 8) &
      UFS_Log.Hex_Field ("nl", Unsigned_64 (Read (UIC_Error_Network)), 8) &
      UFS_Log.Hex_Field ("tl", Unsigned_64 (Read (UIC_Error_Transport)), 8) &
      UFS_Log.Hex_Field ("dme", Unsigned_64 (Read (UIC_Error_DME)), 8));

   function Step_Text (Result : Step_Result) return String is
     (case Result is
        when Step_Done      => "done",
        when Step_Timed_Out => "timeout",
        when Step_Failed    => "uic-error");

   --  DME_GET of a local attribute (zero if it fails).
   function Attribute (Item : UIC_Attribute) return Register_Value is
      Value : Register_Value;
      Result : Step_Result;
   begin
      UIC (DME_Get, Attribute_Argument (Item), 0, Value, Result);
      return (if Result = Step_Done then Value else 0);
   end Attribute;

   procedure Start_Link (Config : LC.Generic_PCI_Configuration; Up : out Boolean)
   is
      Value : Register_Value;
      Result : Step_Result := Step_Failed;
      LCC_Cleared : Boolean := False;
      Attempts : Natural := 0;
   begin
      Up := False;
      if UFS_Quirks.Disables_TX_LCC (Config.Vendor, Config.Device)
        and then Attribute (PA_Local_TX_LCC_Enable) /= 0
      then
         UIC (DME_Set, Attribute_Argument (PA_Local_TX_LCC_Enable), 0, Value,
              Result);
         LCC_Cleared := Result = Step_Done;
      end if;
      for Attempt in 1 .. Link_Startup_Attempts loop
         Attempts := Attempt;
         UIC (DME_Link_Startup, 0, 0, Value, Result);
         Up := Result = Step_Done
           and then (Read (Controller_Status) and Device_Present) /= 0;
         exit when Up;
      end loop;
      if not Up then
         UFS_Log.Failed
           (UFS_Log.Link_Up,
            (if Result = Step_Done then "no-device" else Step_Text (Result)),
            UFS_Log.Field ("tries", Unsigned_64 (Attempts)) & UIC_Errors &
            Controller_State);
         return;
      end if;
      --  Reading the UIC error registers clears what startup left there.
      Value := Read (UIC_Error_PHY_Adapter) or Read (UIC_Error_Data_Link);
      UFS_Log.Passed
        (UFS_Log.Link_Up,
         " lanes=" & Decimal (Unsigned_64 (Attribute (PA_Connected_TX_Data_Lanes))) &
         "/" & Decimal (Unsigned_64 (Attribute (PA_Connected_RX_Data_Lanes))) &
         " gear=PWM maxhs=" &
         Decimal (Unsigned_64 (Attribute (PA_Max_RX_HS_Gear))) &
         UFS_Log.Field ("tries", Unsigned_64 (Attempts)) &
         (if LCC_Cleared then " lcc=off" else ""));
   end Start_Link;

   function Delivery_Text (Item : LC.Interrupt_Delivery) return String is
     (case Item is
        when LC.Polled => "polled",
        when LC.Line   => "intx",
        when LC.MSI    => "msi",
        when LC.MSI_X  => "msix");

   --  bus:device.function from the routing ID.
   function Location_Text (Location : Unsigned_64) return String is
     (Hex (Location / 2 ** 8, 2) & ":" & Hex ((Location / 2 ** 3) mod 2 ** 5, 2)
      & "." & Hex (Location mod 2 ** 3, 1));

   Config : LC.Generic_PCI_Configuration;
   Configured, Mapped, Up, Ready : Boolean := False;
   Result : Step_Result;
   Caps : Controller_Capabilities;
   Raw_Capabilities, Raw_Version : Register_Value;
   Version_Info : Interface_Version;
   Device : UFS_Identify.Identity;
   Progressed : Boolean;
   Ignored_Activity : Activity_Result;
begin
   CuBit.Busy_Poll.Calibrate;
   CuBit.Launch_Configurations.Get (Config, Configured);
   if not Configured or else Config.Window_Total < 1
     or else Config.Windows (1).Bytes < Register_Block_Bytes
     or else Config.DMA.Bytes < UFS_Layout.Region_Bytes
     or else Config.DMA_Virtual = 0
   then
      UFS_Log.Failed
        (UFS_Log.Identify, "launch-config",
         UFS_Log.Field ("valid", Boolean'Pos (Configured)) &
         UFS_Log.Field ("windows", Unsigned_64 (Config.Window_Total)) &
         UFS_Log.Hex_Field ("dma", Config.DMA.Bytes, 8));
      Decline;
      return;
   end if;

   Mapped := CuBit.Handles.Devices.Map_Device
     (Startup.Registers, Config.Windows (1).First, Register_Virtual,
      (Config.Windows (1).Bytes + Page_Bytes - 1) / Page_Bytes,
      Read_Only => False);
   if not Mapped then
      UFS_Log.Failed (UFS_Log.Identify, "map-refused",
                      UFS_Log.Hex_Field ("bar0", Config.Windows (1).First, 12));
      Decline;
      return;
   end if;
   Attach (To_Address (Register_Virtual),
           To_Address (Integer_Address (Config.DMA_Virtual)), Config.DMA.First);

   Raw_Capabilities := Read (Capabilities);
   Raw_Version := Read (Version);
   Caps := Decode_Capabilities (Raw_Capabilities);
   Version_Info := Decode_Version (Raw_Version);
   declare
      Identity : constant String :=
        " pci=" & Hex (Config.Vendor, 4) & ":" & Hex (Config.Device, 4) &
        UFS_Log.Hex_Field ("rev", Config.Revision, 2) &
        " at=" & Location_Text (Config.Location) &
        " bar0=" & Size (Config.Windows (1).Bytes) &
        " irq=" & Delivery_Text (Config.Delivery) &
        " ufshci=" & Decimal (Unsigned_64 (Version_Info.Major)) & "." &
        Decimal (Unsigned_64 (Version_Info.Minor)) &
        " slots=" & Decimal (Unsigned_64 (Caps.Transfer_Slots)) & "/" &
        Decimal (Unsigned_64 (Caps.Task_Slots)) &
        UFS_Log.Field ("dma64", Boolean'Pos (Caps.Addressing_64)) &
        UFS_Log.Hex_Field ("cap", Unsigned_64 (Raw_Capabilities), 8) &
        UFS_Log.Hex_Field ("ver", Unsigned_64 (Raw_Version), 8);
   begin
      if not Supported (Version_Info) then
         UFS_Log.Failed (UFS_Log.Identify, "ufshci-too-old", Identity);
         Decline;
         return;
      elsif Caps.Transfer_Slots < UFS_Layout.Driver_Slots then
         UFS_Log.Failed (UFS_Log.Identify, "too-few-slots", Identity);
         Decline;
         return;
      elsif not Caps.Addressing_64
        and then Config.DMA.First + Config.DMA.Bytes > Four_GiB
      then
         UFS_Log.Failed (UFS_Log.Identify, "dma-above-4g",
                         Identity & UFS_Log.Hex_Field ("dma", Config.DMA.First, 12));
         Decline;
         return;
      end if;
      UFS_Log.Passed (UFS_Log.Identify, Identity);
   end;

   Enable (Result);
   if Result /= Step_Done then
      UFS_Log.Failed (UFS_Log.Enable, Step_Text (Result), Controller_State);
      Decline;
      return;
   end if;
   UFS_Log.Passed (UFS_Log.Enable);
   Start_Link (Config, Up);
   if not Up then
      Decline;
      return;
   end if;
   Start_Lists (Result);
   if Result /= Step_Done then
      UFS_Log.Failed (UFS_Log.Lists, Step_Text (Result), Controller_State);
      Decline;
      return;
   end if;
   UFS_Log.Passed (UFS_Log.Lists);

   --  Polled: the manifest requests no interrupt yet (the generic launch
   --  refuses a driver whose interrupt request it cannot meet, and QEMU's
   --  UFS model has no MSI). IE stays zero; the loop reads IS.
   Acknowledge (Pending_Interrupts);
   Report (CuBit.Driver_Readiness.Ready_Label);

   UFS_Identify.Identify (Device, Ready);
   if Ready then
      UFS_Identify.Report_Partitions (Device);
      UFS_Service.Start (Device);
      UFS_Log.Summary (", serving lun " & Decimal (Unsigned_64 (Device.Unit)) &
                       " read-only");
   else
      UFS_Log.Summary (", nothing served");
   end if;

   loop
      UFS_Service.Poll (Progressed);
      declare
         Sources : constant Register_Value :=
           Pending_Interrupts and Handled_Interrupts (Version_Info);
      begin
         if Sources /= 0 then
            Acknowledge (Sources);
            if (Sources and Fatal_Errors (Version_Info)) /= 0 then
               UFS_Log.Note ("fatal error" &
                             UFS_Log.Hex_Field ("is", Unsigned_64 (Sources), 8));
            end if;
         end if;
      end;
      if not Progressed then
         Ignored_Activity := Wait_For_Activity_Until
           (if UFS_Service.Outstanding
            then Deadline_After (Poll_Interval_Milliseconds)
            else Wait_Forever);
      end if;
   end loop;
end Main;
