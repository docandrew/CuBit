------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  rtw89.drv: Realtek RTL8852BE (PCI 10ec:b852) driver, NET-WIFI-001 W0 and
--  W1 (docs/wifi.md): bring-up to "firmware ready", then the normal-mode
--  start (MAC init, BB/RF tables, antenna grant, calibration, receive
--  path) and a passive scan of the world regulatory domain's channels,
--  published to netstack and wifi.svc as typed data (CuBit.Wifi_Scan);
--  W3/W4: the station wifi.svc drives (RTW89_Station, CuBit.Wifi_Driver).
--
--  @description
--  One line per stage, to logstore and serial, in the bring-up convention
--  (coordination/release-stack.md):
--    rtw89: stage <name> ok|fail <reason> [key=value ...]
--    rtw89: bring-up reached <stage> (<n>/<total>)   or   ... complete
--  then one scan summary line per pass that changes what was heard.
--  Stages 1-12 are rtw89's probe (chip_info_setup), after which the MAC is
--  powered off as rtw89 does; stages 13-23 are rtw89_core_start, the
--  station interface and the scan. The scan only listens: no probe
--  request is ever sent (world regulatory domain: passive only). The
--  radio transmits only for a join wifi.svc asks for, on the channel the
--  BSS was heard on, at the worldwide power limits; one line per command
--  ("command 0x.... ok|fail"). On a failure the MAC is powered off and
--  the driver exits; devmgr logs the exit and boot continues.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Net_Link;
with CuBit.Wifi_Scan;
with Startup;
with RTW89_Registers; use RTW89_Registers;
with RTW89_MAC_Registers;
with RTW89_MMIO;
with RTW89_Outcomes; use RTW89_Outcomes;
with RTW89_Launch;
with RTW89_Power;
with RTW89_Efuse;
with RTW89_Firmware;
with RTW89_Firmware_Blob;
with RTW89_Bringup;
with RTW89_MAC_Init;
with RTW89_PHY;
with RTW89_Receive;
with RTW89_Link;
with RTW89_Wifi_Link;
with RTW89_Station;
with RTW89_Transmit;
with Wifi_Station_Path;
with CuBit.Monotonic;
with CuBit.Kernel_ABI;
with CuBit.Wifi_Driver;
with RTW89_Text; use RTW89_Text;
with Wifi_BSS_Table;
with Wifi_Channel_Plan;

procedure Main is
   use type RTW89_Launch.Acquire_Status;
   use type RTW89_Link.Attach_Status;
   use type RTW89_Wifi_Link.Attach_Status;

   package WS renames CuBit.Wifi_Scan;
   package MR renames RTW89_MAC_Registers;

   Log : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);

   --  The stages, in order; the summary names the last one reached.
   type Stage is
     (Probe, Power_State, Chip_Stage, Power_On_Stage, Efuse_Stage, Secure_Boot,
      Firmware_File, DMA_Stage, DLE, PCIe, FWDL, Firmware_Ready,
      Start_Power, Start_Firmware, MAC_Init, BB_RF, Coex, DM_Init, RFK,
      Station, RX, Scan, Attach);
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
        when Firmware_Ready => "firmware-ready",
        when Start_Power    => "start-power",
        when Start_Firmware => "start-firmware",
        when MAC_Init       => "mac-init",
        when BB_RF          => "bb-rf",
        when Coex           => "coex",
        when DM_Init        => "dm-init",
        when RFK            => "rfk",
        when Station        => "station",
        when RX             => "rx",
        when Scan           => "scan",
        when Attach         => "attach");
   Total : constant := Stage'Pos (Stage'Last) + 1;
   Reached : Natural := 0;   --  stages passed

   Prefix : constant String := "rtw89: ";

   procedure Line (Text : String; Warning : Boolean := False; Serial_Only : Boolean := False) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
        (Prefix & Text,
         (if Warning then CuBit.Log_Records.Warning else CuBit.Log_Records.Information));
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint (Prefix & Text & ASCII.LF);
      if Made.Success and then not Serial_Only then
         CuBit.Logging.Emit (Log, Made.Value, Submitted);
      end if;
   end Line;

   procedure Passed (S : Stage; Detail : String := "") is
   begin
      Reached := Stage'Pos (S) + 1;
      Line ("stage " & Name (S) & " ok" & (if Detail = "" then "" else " " & Detail));
   end Passed;

   function Values (Result : Outcome) return String is
     ("reg=0x" & Hex (Unsigned_64 (Result.Where), 5)
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
   Board    : RTW89_PHY.Board;
   MAC_Address : RTW89_Efuse.MAC_Address := [others => 0];
   Trim     : Boolean := False;

   procedure Summary_Line is
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
   end Summary_Line;

   procedure Power_Down is
      Off : Outcome;
   begin
      if Powered then
         RTW89_Power.Power_Off (Off);
         Powered := False;
         if OK (Off) then
            Line ("mac powered off");
         else
            Line ("mac power-off fail " & RTW89_Outcomes.Name (Off.Why) & " " & Values (Off),
                  Warning => True);
         end if;
      end if;
   end Power_Down;

   --  Summary, MAC off, logs flushed, exit.
   procedure Finish is
      Drained : Boolean;
      Ignore : Unsigned_64;
   begin
      Summary_Line;
      Power_Down;
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

   function Signed_Text (V : Integer) return String is
     (if V < 0 then "-" & Dec (Unsigned_64 (-V)) else Dec (Unsigned_64 (V)));

   --  An SSID for a photo: printable ASCII kept, anything else as '?',
   --  at most 32 characters, quoted; "<hidden>" when empty.
   function SSID_Text (E : WS.BSS_Entry) return String is
      Text : String (1 .. CuBit.Net_Link.SSID_Octets);
      First_Printable : constant := 16#20#;
      Last_Printable  : constant := 16#7E#;
   begin
      if E.Length = 0 then
         return "<hidden>";
      end if;
      for I in 1 .. E.Length loop
         Text (I) := (if E.SSID (I) in First_Printable .. Last_Printable
                      then Character'Val (E.SSID (I)) else '?');
      end loop;
      return """" & Text (1 .. E.Length) & """";
   end SSID_Text;

   ---------------------------------------------------------------------------
   --  Firmware download (probe and start share it)
   ---------------------------------------------------------------------------
   File : constant RTW89_Firmware.Byte_Array (0 .. RTW89_Firmware_Blob.Bytes - 1)
     with Import, Address => RTW89_Firmware_Blob.Address;

   --  DMAC pre-init, PCIe pre-init and the download; the stage for each
   --  failure is reported by the caller through Where.
   type Download_Step is (Step_DMA, Step_DLE, Step_PCIe, Step_FWDL);
   procedure Download_Firmware (Plan : RTW89_Firmware.Plan; Packets : out Natural;
                                Where : out Download_Step; Wide : out Boolean;
                                Result : out Outcome) is
   begin
      Packets := 0;
      Where := Step_DMA;
      RTW89_Bringup.Configure_DMA_Width (DMA, Wide, Result);
      if not OK (Result) then
         return;
      end if;
      Where := Step_DLE;
      RTW89_Bringup.DMAC_Pre_Init (Result);
      if not OK (Result) then
         return;
      end if;
      Where := Step_PCIe;
      RTW89_Bringup.PCIe_Pre_Init (DMA, Result);
      if not OK (Result) then
         return;
      end if;
      RTW89_Receive.Rings_Reset;
      Where := Step_FWDL;
      RTW89_Bringup.Download (DMA, File, Plan, Packets, Result);
   end Download_Firmware;

   function Download_Detail (Result : Outcome; Packets : Natural) return String is
     (RTW89_Outcomes.Name (Result.Why) & " " & Values (Result)
      & " status=" & RTW89_Registers.Name (RTW89_Bringup.Download_Status)
      & " packets=" & Dec (Unsigned_64 (Packets))
      & " boot_dbg=0x" & Hex (Unsigned_64 (RTW89_MMIO.Read32 (BOOT_DBG)), 8));

   ---------------------------------------------------------------------------
   --  Scanning
   ---------------------------------------------------------------------------
   Plan_Channels : constant := Wifi_Channel_Plan.Plan_Length;
   --  How often the RX ring is drained while listening on a channel.
   Poll_Interval_Ms : constant := 10;
   --  The receive check: how long the radio listens before stage rx.
   RX_Check_Ms : constant := 300;
   --  The pause between passes, and how often the ring is drained in it.
   Rescan_Interval_Ms : constant := 30_000;
   Idle_Poll_Ms : constant := 100;

   Counts : RTW89_Receive.Statistics;
   Passes : WS.Pass_Count := 0;
   Last_Total : Natural := Natural'Last;
   Halted_Reported : Boolean := False;

   ---------------------------------------------------------------------------
   --  What wakes the driver besides its polls (NET-WIFI-001 W3/W4): wifi.svc's
   --  answer to the parked Command_Wait (a command), netstack's answer to
   --  the parked Transmit_Wait (frames to send). Frames go out before the
   --  command that came with them; each command is reported with Done.
   ---------------------------------------------------------------------------
   procedure Service is
      Entry_Found : CuBit.Messages.CompletionEntry;
      Sender : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message;
      Found : Boolean;
      Ignore : Unsigned_64;
   begin
      while CuBit.Messages.Poll_Completion (Entry_Found'Address) = 1 loop
         if not RTW89_Wifi_Link.Take (Entry_Found) then
            Found := RTW89_Link.Take (Entry_Found);
         end if;
      end loop;
      --  Nobody calls the driver; anything that does is refused.
      loop
         CuBit.Messages.Poll_Service_Request (Sender, Request, Found);
         exit when not Found;
         Ignore := CuBit.Messages.reply
           (Sender, (tag => (label => CuBit.Kernel_ABI.Reply_Error, length => 0, protocol => 0, ipc => <>, reserved => 0),
                     authorityTag => 0, words => [others => 0]));
      end loop;
      RTW89_Station.Send_Queued (DMA);
      loop
         declare
            C : constant RTW89_Wifi_Link.Pending_Command := RTW89_Wifi_Link.Next_Command;
            Result : CuBit.Wifi_Driver.Outcome;
            use type CuBit.Wifi_Driver.Outcome;
         begin
            exit when not C.Present;
            RTW89_Station.Command (DMA, Board, C.Label, C.Words, Result);
            if C.Label /= CuBit.Wifi_Driver.Label (CuBit.Wifi_Driver.Transmit) then
               RTW89_Wifi_Link.Done (C.Label, Result);
               Line ("command 0x" & Hex (Unsigned_64 (C.Label), 4) & " "
                     & (if Result = CuBit.Wifi_Driver.Done then "ok" else "fail outcome="
                        & Dec (Unsigned_64 (CuBit.Wifi_Driver.Outcome'Enum_Rep (Result))))
                     & (if RTW89_Station.Joined then " joined ch=" & Dec (Unsigned_64 (RTW89_Station.Channel))
                        else ""),
                     Warning => Result /= CuBit.Wifi_Driver.Done, Serial_Only => False);
            end if;
            RTW89_Station.Send_Queued (DMA);
         end;
      end loop;
      RTW89_Wifi_Link.Park;
      RTW89_Link.Park;
   end Service;

   ---------------------------------------------------------------------------
   --  The data path while joined: one line a second when anything moved,
   --  and a line for each EAPOL frame in or out (NET-WIFI-001 W3/W4).
   ---------------------------------------------------------------------------
   Health_Interval_Ms : constant := 1_000;
   Last_Health_Ms : Unsigned_64 := 0;
   Last_Station : RTW89_Station.Statistics;
   Last_Transmit : RTW89_Transmit.Statistics;

   procedure Report_Data_Path is
      S : constant RTW89_Station.Statistics := RTW89_Station.Counts;
      T : constant RTW89_Transmit.Statistics := RTW89_Transmit.Counts;
      Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
      use type RTW89_Station.Statistics;
      use type RTW89_Transmit.Statistics;
      function Drops return String is
         Text : String (1 .. 400);
         Last : Natural := 0;
         procedure Add (Item : String) is
         begin
            if Last + Item'Length <= Text'Length then
               Text (Last + 1 .. Last + Item'Length) := Item;
               Last := Last + Item'Length;
            end if;
         end Add;
      begin
         for R in S.Dropped'Range loop
            if S.Dropped (R) /= Last_Station.Dropped (R) then
               Add (" drop-" & Wifi_Station_Path.Drop_Reason'Image (R) & "="
                    & Dec (Unsigned_64 (S.Dropped (R) - Last_Station.Dropped (R))));
            end if;
         end loop;
         return Text (1 .. Last);
      end Drops;
   begin
      if S.EAPOL_In /= Last_Station.EAPOL_In then
         Line ("eapol rx count=" & Dec (Unsigned_64 (S.EAPOL_In)) & " len="
               & Dec (Unsigned_64 (RTW89_Station.Last_EAPOL_Length)) & " key-info=0x"
               & Hex (Unsigned_64 (RTW89_Station.Last_EAPOL_Information), 4) & " to wifi.svc");
      end if;
      if S.EAPOL_Out /= Last_Station.EAPOL_Out then
         Line ("eapol tx count=" & Dec (Unsigned_64 (S.EAPOL_Out)) & " data-sent="
               & Dec (Unsigned_64 (S.Sent_Data)) & " data-dropped=" & Dec (Unsigned_64 (S.Dropped_Data)));
      end if;
      if Now - Last_Health_Ms >= Health_Interval_Ms and then (S /= Last_Station or else T /= Last_Transmit) then
         Line ("data rx=" & Dec (Unsigned_64 (S.Data_Frames - Last_Station.Data_Frames))
               & " to-wifi=" & Dec (Unsigned_64 (S.To_Wifi - Last_Station.To_Wifi))
               & " to-net=" & Dec (Unsigned_64 (S.To_Netstack - Last_Station.To_Netstack))
               & " tx-sent=" & Dec (Unsigned_64 (T.Sent - Last_Transmit.Sent))
               & " tx-no-room=" & Dec (Unsigned_64 (T.No_Room - Last_Transmit.No_Room))
               & " released=" & Dec (Unsigned_64 (T.Reports - Last_Transmit.Reports))
               & " done=" & Dec (Unsigned_64 (T.By_Status (0) - Last_Transmit.By_Status (0)))
               & " retry-limit=" & Dec (Unsigned_64 (T.By_Status (1) - Last_Transmit.By_Status (1)))
               & " lifetime=" & Dec (Unsigned_64 (T.By_Status (2) - Last_Transmit.By_Status (2)))
               & " macid-drop=" & Dec (Unsigned_64 (T.By_Status (3) - Last_Transmit.By_Status (3)))
               & Drops);
         Last_Health_Ms := Now;
         Last_Station := S;
         Last_Transmit := T;
      elsif S.EAPOL_In /= Last_Station.EAPOL_In or else S.EAPOL_Out /= Last_Station.EAPOL_Out then
         Last_Station.EAPOL_In := S.EAPOL_In;
         Last_Station.EAPOL_Out := S.EAPOL_Out;
      end if;
   end Report_Data_Path;

   --  Sleep Milliseconds, waking for wifi.svc's and netstack's answers.
   procedure Pause (Milliseconds : Natural) is
      Deadline : constant Unsigned_64 := CuBit.Messages.Deadline_After (Unsigned_64 (Milliseconds));
      Activity : CuBit.Messages.Activity_Result;
      use type CuBit.Messages.Activity_Result;
   begin
      loop
         Activity := CuBit.Messages.Wait_For_Activity_Until (Deadline);
         Service;
         exit when Activity = CuBit.Messages.Unavailable or else CuBit.Monotonic.Milliseconds >= Deadline;
      end loop;
   end Pause;

   procedure Listen (Band : WS.Band; Channel : WS.Channel_Number; Milliseconds : Natural;
                     Table : in out Wifi_BSS_Table.Table) is
   begin
      for Tick in 1 .. Milliseconds / Poll_Interval_Ms loop
         Pause (Poll_Interval_Ms);
         RTW89_Receive.Poll (DMA, Band, Channel, Table, Counts);
         exit when RTW89_Station.Joined;
      end loop;
   end Listen;

   --  One pass over the plan into Table. Unlocked counts channels whose
   --  synthesizer did not lock (they are still listened to).
   procedure Scan_Pass (Table : out Wifi_BSS_Table.Table; Unlocked : out Natural;
                        First_Failure : out Outcome) is
      Locked : Boolean;
      Switch : Outcome;
   begin
      Table := Wifi_BSS_Table.Empty;
      Unlocked := 0;
      First_Failure := Success;
      for P of Wifi_Channel_Plan.World_Passive loop
         --  A join (wifi.svc) took the radio: stop retuning.
         exit when RTW89_Station.Joined;
         RTW89_Receive.Restart;
         RTW89_PHY.Set_Channel (Board, P.On_Band, P.Channel, Locked, Switch);
         if not Locked then
            Unlocked := Unlocked + 1;
         end if;
         if not OK (Switch) and then OK (First_Failure) then
            First_Failure := Switch;
         end if;
         Listen (P.On_Band, P.Channel, Wifi_Channel_Plan.Dwell_Milliseconds, Table);
      end loop;
      Passes := Passes + 1;
   end Scan_Pass;

   procedure Report_Pass (Table : Wifi_BSS_Table.Table; To_Logstore : Boolean) is
      R : constant WS.Scan_Result := Wifi_BSS_Table.Publish (Table, Passes, Plan_Channels);
      Detail : constant String :=
        "aps=" & Dec (Unsigned_64 (R.Summary.Total))
        & " 2.4GHz=" & Dec (Unsigned_64 (R.Summary.On_2_4))
        & " 5GHz=" & Dec (Unsigned_64 (R.Summary.On_5));
   begin
      RTW89_Link.Publish (R);
      RTW89_Wifi_Link.Publish (R);
      RTW89_Station.Remember_Scan (R);
      if R.Summary.Count = 0 then
         Line ("scan summary pass=" & Dec (Unsigned_64 (Passes)) & " " & Detail & " strongest=none",
               Warning => R.Summary.Total = 0, Serial_Only => not To_Logstore);
      else
         Line ("scan summary pass=" & Dec (Unsigned_64 (Passes)) & " " & Detail
               & " strongest=" & SSID_Text (R.Entries (1))
               & " rssi=" & Signed_Text (Integer (R.Entries (1).Signal)) & "dBm"
               & " ch=" & Dec (Unsigned_64 (R.Entries (1).Channel)),
               Serial_Only => not To_Logstore);
      end if;
      --  The full list on serial (too long for a photo).
      for I in 1 .. R.Summary.Count loop
         Line ("bss " & SSID_Text (R.Entries (I))
               & " bssid=" & Hex2 (R.Entries (I).BSSID (0)) & ":" & Hex2 (R.Entries (I).BSSID (1))
               & ":" & Hex2 (R.Entries (I).BSSID (2)) & ":" & Hex2 (R.Entries (I).BSSID (3))
               & ":" & Hex2 (R.Entries (I).BSSID (4)) & ":" & Hex2 (R.Entries (I).BSSID (5))
               & " ch=" & Dec (Unsigned_64 (R.Entries (I).Channel))
               & " rssi=" & Signed_Text (Integer (R.Entries (I).Signal))
               & " cap=0x" & Hex (Unsigned_64 (R.Entries (I).Capability), 4)
               & (if R.Entries (I).RSN then " rsn" elsif WS.Privacy (R.Entries (I)) then " wep/wpa" else " open")
               & " heard=" & Dec (Unsigned_64 (R.Entries (I).Heard)),
               Serial_Only => True);
      end loop;
      Last_Total := R.Summary.Total;
   end Report_Pass;

   procedure Check_Firmware is
      Status : constant Unsigned_32 := RTW89_MMIO.Read32 (MR.HISR0);
   begin
      if (Status and MR.HALT_C2H_INT) /= 0 and then not Halted_Reported then
         Halted_Reported := True;
         Line ("firmware halted hisr0=0x" & Hex (Unsigned_64 (Status), 8)
               & " halt_c2h=0x" & Hex (Unsigned_64 (RTW89_MMIO.Read32 (HALT_C2H)), 8),
               Warning => True);
      end if;
   end Check_Firmware;

   function Counts_Text return String is
     ("buffers=" & Dec (Unsigned_64 (Counts.Buffers))
      & " frames=" & Dec (Unsigned_64 (Counts.Frames))
      & " beacons=" & Dec (Unsigned_64 (Counts.Beacons))
      & " ppdu=" & Dec (Unsigned_64 (Counts.PPDU_Reports))
      & " signals=" & Dec (Unsigned_64 (Counts.Signals))
      & " c2h=" & Dec (Unsigned_64 (Counts.C2H))
      & " crc=" & Dec (Unsigned_64 (Counts.CRC_Errors))
      & " refused=" & Dec (Unsigned_64 (Counts.Refused)));

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
      Power_Down;
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
   RTW89_Power.Power_On (Trim => False, Result => Result);
   if not OK (Result) then
      Failed (Power_On_Stage, Result);
      Finish;
      return;
   end if;
   Passed (Power_On_Stage, "ic_pwr_state=0x"
           & Hex (Unsigned_64 (RTW89_MMIO.Read32 (IC_PWR_STATE)), 8));

   ---------------------------------------------------------------------------
   --  efuse: MAC address, country, RFE type; secure-boot and phycap bytes
   ---------------------------------------------------------------------------
   declare
      Physical : RTW89_Efuse.Physical_Map;
      Records  : RTW89_Efuse.Record_Count;
      Status   : RTW89_Efuse.Decode_Status;
      Info     : RTW89_Power.Secure_Boot_Info;
      Autoloaded : Boolean;
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
      RTW89_Efuse.Decode (Physical, Board.Logical, Records, Status);
      if Status /= RTW89_Efuse.Decoded then
         Failed (Efuse_Stage, RTW89_Efuse.Name (Status)
                 & " records=" & Dec (Unsigned_64 (Records)));
         Finish;
         return;
      end if;
      MAC_Address := RTW89_Efuse.MAC (Board.Logical);
      if not RTW89_Efuse.Usable (MAC_Address) then
         Failed (Efuse_Stage, "mac-unusable mac=" & MAC_Text (MAC_Address));
         Finish;
         return;
      end if;
      Board.RFE := Board.Logical (RTW89_Efuse.RFE_Type_Offset);
      Board.Cut := Chip.Raw_Cut;
      Autoloaded := RTW89_Power.Efuse_Autoloaded;
      Passed (Efuse_Stage,
              "mac=" & MAC_Text (MAC_Address)
              & " country=" & Printable (Board.Logical (RTW89_Efuse.Country_Offset))
              & Printable (Board.Logical (RTW89_Efuse.Country_Offset + 1))
              & " rfe=" & Dec (Unsigned_64 (Board.RFE))
              & " xtal=0x" & Hex2 (Board.Logical (RTW89_Efuse.Crystal_Trim_Offset))
              & " records=" & Dec (Unsigned_64 (Records))
              & " autoload=" & (if Autoloaded then "yes" else "no"));

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
      --  The phycap area (gain compensation, power calibration flag).
      for I in RTW89_PHY.Phycap_Map'Range loop
         RTW89_Power.Read_Physical_Byte
           (RTW89_Power.Efuse_Address (RTW89_PHY.Phycap_First + I), Board.Phycap (I), Result);
         if not OK (Result) then
            Failed (Secure_Boot, Result);
            Finish;
            return;
         end if;
      end loop;
      declare
         Power_K : constant Unsigned_8 :=
           Board.Phycap (MR.POWER_K_CHECK_ADDRESS - RTW89_PHY.Phycap_First);
      begin
         --  rtw8852b_pwr_on_func: trims when the efuse autoloaded and the
         --  power calibration byte does not say calibrated.
         Trim := Autoloaded and then Power_K /= MR.POWER_K_VALID;
         Passed (Secure_Boot, "off power_k=0x" & Hex2 (Power_K)
                 & " trims=" & (if Trim then "yes" else "no"));
      end;
   end;

   ---------------------------------------------------------------------------
   --  firmware: the embedded blob, image for this cut, plan; download
   ---------------------------------------------------------------------------
   declare
      Wide    : Boolean;
      Packets : Natural;
      Where   : Download_Step;
      Image : constant RTW89_Firmware.Image :=
        RTW89_Firmware.Select_Image (File, RTW89_Firmware.Normal_CE, Chip.Raw_Cut);
      Plan : RTW89_Firmware.Plan;
      Tables : RTW89_PHY.Table_Counts;
      Timeouts : Natural;
      Table : Wifi_BSS_Table.Table;
      Unlocked : Natural;
      Switch_Failure : Outcome;
      Linked : RTW89_Link.Attach_Status;
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

      Download_Firmware (Plan, Packets, Where, Wide, Result);
      if not OK (Result) then
         case Where is
            when Step_DMA  => Failed (DMA_Stage, Result);
            when Step_DLE  => Passed (DMA_Stage, (if Wide then "width=36" else "width=32"));
                              Failed (DLE, Result);
            when Step_PCIe => Passed (DMA_Stage, (if Wide then "width=36" else "width=32"));
                              Passed (DLE);
                              Failed (PCIe, Result);
            when Step_FWDL => Passed (DMA_Stage, (if Wide then "width=36" else "width=32"));
                              Passed (DLE);
                              Passed (PCIe);
                              Failed (FWDL, Download_Detail (Result, Packets));
         end case;
         Finish;
         return;
      end if;
      Passed (DMA_Stage, (if Wide then "width=36" else "width=32"));
      Passed (DLE);
      Passed (PCIe);
      Passed (FWDL, "sections=" & Dec (Unsigned_64 (Plan.Sections))
              & " packets=" & Dec (Unsigned_64 (Packets)));
      Passed (Firmware_Ready, "wcpu_fw_ctrl=0x"
              & Hex (Unsigned_64 (RTW89_MMIO.Read32 (WCPU_FW_CTRL)), 8));

      ------------------------------------------------------------------------
      --  core start: power cycle with the trims, download again
      ------------------------------------------------------------------------
      Power_Down;
      Powered := True;
      RTW89_Power.Power_On (Trim => Trim, Result => Result);
      if not OK (Result) then
         Failed (Start_Power, Result);
         Finish;
         return;
      end if;
      Passed (Start_Power, "trims=" & (if Trim then "yes" else "no")
              & " ic_pwr_state=0x" & Hex (Unsigned_64 (RTW89_MMIO.Read32 (IC_PWR_STATE)), 8));

      Download_Firmware (Plan, Packets, Where, Wide, Result);
      if not OK (Result) then
         Failed (Start_Firmware, Download_Detail (Result, Packets));
         Finish;
         return;
      end if;
      Passed (Start_Firmware, "packets=" & Dec (Unsigned_64 (Packets))
              & " wcpu_fw_ctrl=0x" & Hex (Unsigned_64 (RTW89_MMIO.Read32 (WCPU_FW_CTRL)), 8));

      RTW89_MAC_Init.Initialize (DMA, Result);
      if not OK (Result) then
         Failed (MAC_Init, Result);
         Finish;
         return;
      end if;
      Passed (MAC_Init, "dmac=0x" & Hex (Unsigned_64 (RTW89_MMIO.Read32 (DMAC_FUNC_EN)), 8)
              & " cmac=0x" & Hex (Unsigned_64 (RTW89_MMIO.Read32 (CMAC_FUNC_EN)), 8));

      RTW89_PHY.Reset_BB_RF (Result);
      if OK (Result) then
         RTW89_PHY.Load_Tables (Board, Tables, Result);
      end if;
      if not OK (Result) then
         Failed (BB_RF, Result);
         Finish;
         return;
      end if;
      Passed (BB_RF, "bb=" & Dec (Unsigned_64 (Tables.BB))
              & " rf_a=" & Dec (Unsigned_64 (Tables.Radio_A))
              & " rf_b=" & Dec (Unsigned_64 (Tables.Radio_B))
              & " rfe=" & Dec (Unsigned_64 (Board.RFE)) & " cv=" & Dec (Unsigned_64 (Board.Cut)));

      RTW89_PHY.Grant_Antenna_To_WLAN (Result);
      if not OK (Result) then
         Failed (Coex, Result);
         Finish;
         return;
      end if;
      Passed (Coex, "gnt=wlan bt=off");

      RTW89_PHY.DM_Init (Board, Tables, Result);
      if not OK (Result) then
         Failed (DM_Init, Result);
         Finish;
         return;
      end if;
      Passed (DM_Init, "nctl=" & Dec (Unsigned_64 (Tables.NCTL))
              & " xtal=0x" & Hex2 (Board.Logical (RTW89_Efuse.Crystal_Trim_Offset)));

      RTW89_PHY.Calibrate (Timeouts, Result);
      if not OK (Result) then
         Failed (RFK, Result);
         Finish;
         return;
      end if;
      Passed (RFK, "rck dack rxdck timeouts=" & Dec (Unsigned_64 (Timeouts)));
      --  rtw89_phy_dm_init's tail: the BB power reference and the efuse
      --  power trims (W3: the radio may transmit from here on).
      RTW89_PHY.TX_Power_Control;
      RTW89_PHY.Power_Trim (Board);

      RTW89_PHY.Configure_RX_Paths (Board);
      RTW89_MAC_Init.Start (DMA, Result);
      if OK (Result) then
         RTW89_MAC_Init.Add_Station (DMA, MAC_Address, Result);
      end if;
      if not OK (Result) then
         Failed (Station, Result);
         Finish;
         return;
      end if;
      RTW89_MAC_Init.Scan_Filter;
      Passed (Station, "macid=0 port=0 rx_paths=ab filter=0x"
              & Hex (Unsigned_64 (RTW89_MMIO.Read32 (MR.RX_FLTR_OPT)), 8));

      ------------------------------------------------------------------------
      --  rx: does the ring move? (firmware acknowledgements, PPDU status
      --  and frames on 2.4 GHz channel 1)
      ------------------------------------------------------------------------
      declare
         Locked : Boolean;
         First : constant Wifi_Channel_Plan.Planned_Channel := Wifi_Channel_Plan.World_Passive (1);
      begin
         Table := Wifi_BSS_Table.Empty;
         RTW89_PHY.Set_Channel (Board, First.On_Band, First.Channel, Locked, Result);
         Listen (First.On_Band, First.Channel, RX_Check_Ms, Table);
         if Counts.Buffers = 0 then
            Failed (RX, "rx-silent lock=" & (if Locked then "yes" else "no") & " " & Counts_Text);
         else
            Passed (RX, "lock=" & (if Locked then "yes" else "no") & " " & Counts_Text);
         end if;
      end;

      ------------------------------------------------------------------------
      --  scan: one pass over the plan, then attach and publish
      ------------------------------------------------------------------------
      Scan_Pass (Table, Unlocked, Switch_Failure);
      Check_Firmware;
      declare
         Detail : constant String :=
           "aps=" & Dec (Unsigned_64 (Table.Count))
           & " channels=" & Dec (Unsigned_64 (Plan_Channels))
           & " unlocked=" & Dec (Unsigned_64 (Unlocked)) & " " & Counts_Text;
      begin
         if Table.Count = 0 then
            Failed (Scan, "no-aps " & Detail);
         elsif Reached = Stage'Pos (RX) + 1 then
            Passed (Scan, Detail);
         else
            Line ("stage scan ok " & Detail);
         end if;
      end;
      if not OK (Switch_Failure) then
         Line ("channel switch " & RTW89_Outcomes.Name (Switch_Failure.Why) & " " & Values (Switch_Failure),
               Warning => True);
      end if;

      RTW89_Link.Attach ([for I in CuBit.Net_Link.Octet_Index => MAC_Address (I)],
                         CuBit.Net_Link.PCI_Location (Device.Location and 16#FFFF#), Linked);
      if Linked = RTW89_Link.Attached then
         if Reached = Stage'Pos (Scan) + 1 then
            Passed (Attach, "if=" & RTW89_Link.Interface_Name & " kind=wireless link=down");
         else
            Line ("stage attach ok if=" & RTW89_Link.Interface_Name);
         end if;
      else
         Failed (Attach, RTW89_Link.Name (Linked));
      end if;
      RTW89_Station.Initialize ([for I in CuBit.Net_Link.Octet_Index => MAC_Address (I)]);
      declare
         Joined_Wifi : RTW89_Wifi_Link.Attach_Status;
      begin
         RTW89_Wifi_Link.Attach ([for I in CuBit.Net_Link.Octet_Index => MAC_Address (I)], Joined_Wifi);
         Line ("wifi.svc " & RTW89_Wifi_Link.Name (Joined_Wifi),
               Warning => Joined_Wifi /= RTW89_Wifi_Link.Attached);
      end;
      Report_Pass (Table, To_Logstore => True);
      Summary_Line;
      declare
         Drained : Boolean;
      begin
         CuBit.Logging.Flush (Log, Drained);
      end;

      ------------------------------------------------------------------------
      --  Keep listening: a pass every Rescan_Interval_Ms, published each
      --  time; a summary line reaches logstore when the count changes.
      ------------------------------------------------------------------------
      --  While joined to a BSS (wifi.svc's Join), only that channel:
      --  frames to and from the air every Poll_Interval_Ms, no scan.
      loop
         declare
            Idle : Wifi_BSS_Table.Table := Wifi_BSS_Table.Empty;
            Last : constant Wifi_Channel_Plan.Planned_Channel :=
              Wifi_Channel_Plan.World_Passive (Wifi_Channel_Plan.Plan_Length);
            Previous : constant Natural := Last_Total;
         begin
            if RTW89_Station.Joined then
               Pause (Poll_Interval_Ms);
               RTW89_Receive.Poll (DMA, RTW89_Station.Band, RTW89_Station.Channel, Idle, Counts);
               Report_Data_Path;
            else
               for Tick in 1 .. Rescan_Interval_Ms / Idle_Poll_Ms loop
                  Pause (Idle_Poll_Ms);
                  exit when RTW89_Station.Joined;
                  RTW89_Receive.Poll (DMA, Last.On_Band, Last.Channel, Idle, Counts);
               end loop;
               if not RTW89_Station.Joined then
                  Scan_Pass (Table, Unlocked, Switch_Failure);
                  Check_Firmware;
                  Report_Pass (Table, To_Logstore => Table.Count /= Previous);
               end if;
            end if;
         end;
      end loop;
   end;
end Main;
