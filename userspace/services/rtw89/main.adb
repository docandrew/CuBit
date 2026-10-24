------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  rtw89.drv: Realtek RTL8852BE (PCI 10ec:b852) bring-up driver, NET-WIFI-001
--  W0 (docs/wifi.md, "Bring-up milestone").
--
--  @description
--  Read-only bring-up: probe, chip identity, MAC power-on, efuse MAC
--  address, firmware download to "firmware ready". One line per stage, to
--  logstore and serial, in the bring-up convention
--  (coordination/release-stack.md):
--    rtw89: stage <name> ok|fail <reason> [key=value ...]
--    rtw89: bring-up reached <stage> (<n>/<total>)   or   ... complete
--  No RF is enabled and nothing is transmitted over the air. At the end
--  (or on the first failure) the MAC is powered off again and the driver
--  exits; devmgr logs the exit and boot continues.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
with Startup;
with RTW89_Registers; use RTW89_Registers;
with RTW89_MMIO;
with RTW89_Outcomes; use RTW89_Outcomes;
with RTW89_Launch;
with RTW89_Power;
with RTW89_Efuse;
with RTW89_Firmware;
with RTW89_Firmware_Blob;
with RTW89_Bringup;
with RTW89_Text; use RTW89_Text;

procedure Main is
   use type RTW89_Launch.Acquire_Status;

   Log : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);

   --  The stages, in order; the summary names the last one reached.
   type Stage is
     (Probe, Power_State, Chip_Stage, Power_On_Stage, Efuse_Stage, Secure_Boot,
      Firmware_File, DMA_Stage, DLE, PCIe, FWDL, Firmware_Ready);
   function Name (S : Stage) return String is
     (case S is
        when Probe          => "probe",
        when Power_State    => "power-state",
        when Chip_Stage     => "chip",
        when Power_On_Stage => "power-on",
        when Efuse_Stage    => "efuse",
        when Secure_Boot    => "secure-boot",
        when Firmware_File  => "firmware-file",
        when DMA_Stage      => "dma",
        when DLE            => "dle",
        when PCIe           => "pcie",
        when FWDL           => "fwdl",
        when Firmware_Ready => "firmware-ready");
   Total : constant := Stage'Pos (Stage'Last) + 1;
   Reached : Natural := 0;   --  stages passed

   Prefix : constant String := "rtw89: ";

   procedure Line (Text : String; Warning : Boolean := False) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
        (Prefix & Text,
         (if Warning then CuBit.Log_Records.Warning else CuBit.Log_Records.Information));
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint (Prefix & Text & ASCII.LF);
      if Made.Success then
         CuBit.Logging.Emit (Log, Made.Value, Submitted);
      end if;
   end Line;

   procedure Passed (S : Stage; Detail : String := "") is
   begin
      Reached := Stage'Pos (S) + 1;
      Line ("stage " & Name (S) & " ok" & (if Detail = "" then "" else " " & Detail));
   end Passed;

   function Values (Result : Outcome) return String is
     ("reg=0x" & Hex (Unsigned_64 (Result.Where), 4)
      & " val=0x" & Hex (Unsigned_64 (Result.Value), 8));

   procedure Failed (S : Stage; Result : Outcome) is
   begin
      Line ("stage " & Name (S) & " fail " & RTW89_Outcomes.Name (Result.Why)
            & " " & Values (Result), Warning => True);
   end Failed;

   procedure Failed (S : Stage; Reason : String) is
   begin
      Line ("stage " & Name (S) & " fail " & Reason, Warning => True);
   end Failed;

   Device   : RTW89_Launch.Resources;
   Acquired : RTW89_Launch.Acquire_Status;
   Result   : Outcome;
   Chip     : RTW89_Power.Identity;
   Powered  : Boolean := False;
   DMA      : RTW89_Bringup.Region;

   --  Summary, MAC off, logs flushed, exit.
   procedure Finish is
      Off : Outcome;
      Drained : Boolean;
      Ignore : Unsigned_64;
   begin
      if Reached = Total then
         Line ("bring-up complete (" & Dec (Total) & "/" & Dec (Total) & ")");
      elsif Reached = 0 then
         Line ("bring-up reached nothing (0/" & Dec (Total) & ")", Warning => True);
      else
         Line ("bring-up reached " & Name (Stage'Val (Reached - 1))
               & " (" & Dec (Unsigned_64 (Reached)) & "/" & Dec (Total) & ")",
               Warning => True);
      end if;
      if Powered then
         RTW89_Power.Power_Off (Off);
         if OK (Off) then
            Line ("mac powered off");
         else
            Line ("mac power-off fail " & RTW89_Outcomes.Name (Off.Why) & " " & Values (Off),
                  Warning => True);
         end if;
      end if;
      CuBit.Logging.Flush (Log, Drained);
      Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_EXIT);
   end Finish;

   --  The non-blank 32-byte rows of the physical efuse, on serial only (a
   --  fixture for hosted decoder tests; too long for a screen photo).
   procedure Dump_Efuse (Map : RTW89_Efuse.Physical_Map) is
      Row_Bytes : constant := 32;
      Row : String (1 .. Row_Bytes * 2);
      Blank : Boolean;
   begin
      for Start in 0 .. RTW89_Efuse.Physical_Size / Row_Bytes - 1 loop
         Blank := True;
         for I in 0 .. Row_Bytes - 1 loop
            declare
               B : constant Unsigned_8 := Map (Start * Row_Bytes + I);
            begin
               Row (I * 2 + 1 .. I * 2 + 2) := Hex2 (B);
               Blank := Blank and then B = RTW89_Efuse.Unprogrammed;
            end;
         end loop;
         if not Blank then
            CuBit.Messages.debugPrint
              (Prefix & "efuse " & Hex (Unsigned_64 (Start * Row_Bytes), 3) & ": " & Row & ASCII.LF);
         end if;
      end loop;
   end Dump_Efuse;

   function MAC_Text (Address : RTW89_Efuse.MAC_Address) return String is
     (Hex2 (Address (0)) & ":" & Hex2 (Address (1)) & ":" & Hex2 (Address (2)) & ":"
      & Hex2 (Address (3)) & ":" & Hex2 (Address (4)) & ":" & Hex2 (Address (5)));

   function Printable (Byte : Unsigned_8) return Character is
     (if Byte in Character'Pos ('A') .. Character'Pos ('Z')
      then Character'Val (Byte) else '?');

begin
   --  Startup's elaboration already checked the capability table.
   ---------------------------------------------------------------------------
   --  probe: launch resources, register window, sanity read, identity
   ---------------------------------------------------------------------------
   RTW89_Launch.Acquire (Device, Acquired);
   if Acquired /= RTW89_Launch.Acquired then
      Failed (Probe, RTW89_Launch.Name (Acquired));
      RTW89_Launch.Report_Not_Present;
      Finish;
      return;
   end if;
   RTW89_MMIO.Attach (Device.Registers_Virtual);
   if RTW89_MMIO.Read32 (SYS_CFG1) = All_Ones then
      Failed (Probe, Failure (Not_Responding, SYS_CFG1, All_Ones));
      RTW89_Launch.Report_Not_Present;
      Finish;
      return;
   end if;
   --  Everything polls: mask the device before devmgr unmasks the MSI.
   RTW89_Bringup.Mask_Interrupts;
   RTW89_Launch.Report_Ready;
   Passed (Probe,
           "pci=" & Hex (Device.Vendor, 4) & ":" & Hex (Device.Device, 4)
           & " rev=" & Hex (Device.Revision, 2)
           & " at=" & Hex (Shift_Right (Device.Location, 8), 2) & ":"
           & Hex (Shift_Right (Device.Location, 3) and 16#1F#, 2) & "."
           & Hex (Device.Location and 16#7#, 1)
           & " bar2=0x" & Hex (Device.Registers_Physical, 12)
           & "+" & Dec (Device.Registers_Bytes)
           & " irq=" & RTW89_Launch.Name (Device.Interrupt_Mode) & " vectors=" & Dec (Device.Vectors)
           & " dma=0x" & Hex (Device.DMA_Physical, 12) & "+" & Dec (Device.DMA_Bytes));
   DMA := (Physical => Device.DMA_Physical, Virtual => Device.DMA_Virtual,
           Bytes => Device.DMA_Bytes);

   ---------------------------------------------------------------------------
   --  power-state, chip, power-on
   ---------------------------------------------------------------------------
   if RTW89_Power.MAC_Was_On then
      Powered := True;
      RTW89_Power.Power_Off (Result);
      if not OK (Result) then
         Failed (Power_State, Result);
         Finish;
         return;
      end if;
      Passed (Power_State, "was=on powered-off-first");
   else
      Passed (Power_State, "was=off");
   end if;

   RTW89_Power.Read_Identity (Chip, Result);
   if not OK (Result) then
      Failed (Chip_Stage, Result);
      Finish;
      return;
   end if;
   Passed (Chip_Stage, "cv=" & Dec (Unsigned_64 (Chip.Raw_Cut))
           & "(" & RTW89_Power.Name (Chip.Cut) & ")"
           & " acv=" & Dec (Unsigned_64 (Chip.Analog_Cut))
           & " sys_cfg1=0x" & Hex (Unsigned_64 (RTW89_MMIO.Read32 (SYS_CFG1)), 8));

   Powered := True;
   RTW89_Power.Power_On (Result);
   if not OK (Result) then
      Failed (Power_On_Stage, Result);
      Finish;
      return;
   end if;
   Passed (Power_On_Stage, "ic_pwr_state=0x"
           & Hex (Unsigned_64 (RTW89_MMIO.Read32 (IC_PWR_STATE)), 8));

   ---------------------------------------------------------------------------
   --  efuse: MAC address, country, RFE type; secure-boot information
   ---------------------------------------------------------------------------
   declare
      Physical : RTW89_Efuse.Physical_Map;
      Logical  : RTW89_Efuse.Logical_Map;
      Records  : RTW89_Efuse.Record_Count;
      Status   : RTW89_Efuse.Decode_Status;
      Info     : RTW89_Power.Secure_Boot_Info;
      use type RTW89_Efuse.Decode_Status;
      use type RTW89_Power.Secure_Boot_Info;
   begin
      RTW89_Power.Read_Efuse (Physical, Result);
      if not OK (Result) then
         Failed (Efuse_Stage, Result);
         Finish;
         return;
      end if;
      Dump_Efuse (Physical);
      RTW89_Efuse.Decode (Physical, Logical, Records, Status);
      if Status /= RTW89_Efuse.Decoded then
         Failed (Efuse_Stage, RTW89_Efuse.Name (Status)
                 & " records=" & Dec (Unsigned_64 (Records)));
         Finish;
         return;
      end if;
      if not RTW89_Efuse.Usable (RTW89_Efuse.MAC (Logical)) then
         Failed (Efuse_Stage, "mac-unusable mac=" & MAC_Text (RTW89_Efuse.MAC (Logical)));
         Finish;
         return;
      end if;
      Passed (Efuse_Stage,
              "mac=" & MAC_Text (RTW89_Efuse.MAC (Logical))
              & " country=" & Printable (Logical (RTW89_Efuse.Country_Offset))
              & Printable (Logical (RTW89_Efuse.Country_Offset + 1))
              & " rfe=" & Dec (Unsigned_64 (Logical (RTW89_Efuse.RFE_Type_Offset)))
              & " xtal=0x" & Hex2 (Logical (RTW89_Efuse.Crystal_Trim_Offset))
              & " records=" & Dec (Unsigned_64 (Records))
              & " autoload=" & (if RTW89_Power.Efuse_Autoloaded then "yes" else "no"));

      RTW89_Power.Read_Secure_Boot_Info (Info, Result);
      if not OK (Result) then
         Failed (Secure_Boot, Result);
         Finish;
         return;
      end if;
      if Info /= [RTW89_Efuse.Unprogrammed, RTW89_Efuse.Unprogrammed] then
         Failed (Secure_Boot, "secure-boot-part-unsupported info=0x"
                 & Hex2 (Info (1)) & Hex2 (Info (2)));
         Finish;
         return;
      end if;
      Passed (Secure_Boot, "off");
   end;

   ---------------------------------------------------------------------------
   --  firmware: the embedded blob, image for this cut, plan; download
   ---------------------------------------------------------------------------
   declare
      Wide    : Boolean;
      Packets : Natural;
      --  A blob above Maximum_File_Bytes would fail this bound statically.
      File : constant RTW89_Firmware.Byte_Array (0 .. RTW89_Firmware_Blob.Bytes - 1)
        with Import, Address => RTW89_Firmware_Blob.Address;
      Image : constant RTW89_Firmware.Image :=
        RTW89_Firmware.Select_Image (File, RTW89_Firmware.Normal_CE, Chip.Raw_Cut);
      Plan : RTW89_Firmware.Plan;
      use type RTW89_Firmware.Plan_Status;
   begin
      if not Image.Found then
         Failed (Firmware_File, "no-image-for-cut cv=" & Dec (Unsigned_64 (Chip.Raw_Cut)));
         Finish;
         return;
      end if;
      Plan := RTW89_Firmware.Plan_Image (File, Image);
      if Plan.Status /= RTW89_Firmware.Planned then
         Failed (Firmware_File, RTW89_Firmware.Name (Plan.Status));
         Finish;
         return;
      end if;
      Passed (Firmware_File,
              "bytes=" & Dec (RTW89_Firmware_Blob.Bytes)
              & " image=0x" & Hex (Unsigned_64 (Image.Offset), 6)
              & "+" & Dec (Unsigned_64 (Image.Size))
              & " sections=" & Dec (Unsigned_64 (Plan.Sections)));

      RTW89_Bringup.Configure_DMA_Width (DMA, Wide, Result);
      if not OK (Result) then
         Failed (DMA_Stage, Result);
         Finish;
         return;
      end if;
      Passed (DMA_Stage, (if Wide then "width=36" else "width=32"));

      RTW89_Bringup.DMAC_Pre_Init (Result);
      if not OK (Result) then
         Failed (DLE, Result);
         Finish;
         return;
      end if;
      Passed (DLE);

      RTW89_Bringup.PCIe_Pre_Init (DMA, Result);
      if not OK (Result) then
         Failed (PCIe, Result);
         Finish;
         return;
      end if;
      Passed (PCIe);

      RTW89_Bringup.Download (DMA, File, Plan, Packets, Result);
      if not OK (Result) then
         Failed (FWDL, RTW89_Outcomes.Name (Result.Why) & " " & Values (Result)
                 & " status=" & RTW89_Registers.Name (RTW89_Bringup.Download_Status)
                 & " packets=" & Dec (Unsigned_64 (Packets))
                 & " boot_dbg=0x" & Hex (Unsigned_64 (RTW89_MMIO.Read32 (BOOT_DBG)), 8));
         Finish;
         return;
      end if;
      Passed (FWDL, "sections=" & Dec (Unsigned_64 (Plan.Sections))
              & " packets=" & Dec (Unsigned_64 (Packets)));
      Passed (Firmware_Ready, "wcpu_fw_ctrl=0x"
              & Hex (Unsigned_64 (RTW89_MMIO.Read32 (WCPU_FW_CTRL)), 8));
   end;

   Finish;
end Main;
