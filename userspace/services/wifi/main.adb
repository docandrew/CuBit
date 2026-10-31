------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  wifi.svc: the Wi-Fi connection manager and supplicant (NET-WIFI-001
--  W3/W4, docs/wifi.md "Process structure"). A Wi-Fi driver (rtw89.drv)
--  attaches to it (CuBit.Wifi_Driver); a controller holding the "wifi"
--  endpoint (the console) joins, leaves, asks the status and stores a
--  passphrase (CuBit.Wifi_Control).
--
--  @description
--  Split by authority: this process alone holds passphrases (the Secret
--  setting family wifi.secret., which only its manifest names), PMKs, KCKs
--  and KEKs; the driver gets only the temporal keys it installs into the
--  hardware. The join itself is Wifi_Station (no I/O); this is its shell:
--  the driver's grant (frames both ways and its scan block), its parked
--  Command_Wait (answered with the next queued command), the controller's
--  calls, and the clock for the join's deadlines.
--
--  The IPC graph stays a DAG: wifi.svc never calls or submits to the
--  driver or the controller; it only answers.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Granted_Logs;
with CuBit.Hardware_Random;
with CuBit.Monotonic;
with CuBit.Net_Link;
with CuBit.Settings;
with CuBit.Wifi_Driver;
with CuBit.Wifi_Control;
with CuBit.Wifi_Scan;
with SPARKNaCl;
with WiFi_Crypto.Legacy_WPA2;
with Wifi_MLME;
with Wifi_EAPOL;
with Wifi_Handshake;
with Wifi_Station;
with Startup;

procedure Main is
   package NL renames CuBit.Net_Link;
   package D renames CuBit.Wifi_Driver;
   package C renames CuBit.Wifi_Control;
   package WS renames CuBit.Wifi_Scan;
   package ST renames Wifi_Station;
   package M renames Wifi_MLME;
   package H renames Wifi_Handshake;
   package E renames Wifi_EAPOL;
   package W2 renames WiFi_Crypto.Legacy_WPA2;
   package Ring renames CuBit.Wifi_Driver.Rings;
   use type System.Address;
   use type D.Frame_Kind;
   use type C.State;
   use type WS.Signal_dBm;
   use type ST.Security;
   use type M.Failure;
   use type ST.Output_Kind;
   use all type CuBit.Settings.Outcome;
   use all type CuBit.Settings.Change_Outcome;

   Ready_Label : constant Unsigned_32 := 16#FF00#;
   REPLY_OK  : constant Unsigned_32 := CuBit.Kernel_ABI.Reply_OK;
   REPLY_ERR : constant Unsigned_32 := CuBit.Kernel_ABI.Reply_Error;
   The_Handle : constant D.Handle := 1;

   --  The limits joins run under (docs/wifi.md "MLME state machine"):
   --  rtw89/mac80211-like retry budgets, spelled out here, not defaults.
   Join_Limits : constant M.Limits :=
     (Frame_Timeout => 200, Frame_Attempts => 3, Handshake_Timeout => 3_000);
   Beacon_Timeout : constant M.Milliseconds := 2_000;

   procedure Say (Text : String; Warn : Boolean := False) is
   begin
      CuBit.Granted_Logs.Line
        (Text, (if Warn then CuBit.Granted_Logs.Warning else CuBit.Granted_Logs.Information));
   end Say;

   function Image (V : Unsigned_64) return String is
      S : constant String := Unsigned_64'Image (V);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   Hex_Digits : constant String := "0123456789abcdef";

   function SSID_Hex (S : C.SSID) return String is
      R : String (1 .. 2 * S.Length);
   begin
      for I in 1 .. S.Length loop
         R (2 * I - 1) := Hex_Digits (Natural (Shift_Right (S.Octets (I), 4)) + 1);
         R (2 * I) := Hex_Digits (Natural (S.Octets (I) and 16#0F#) + 1);
      end loop;
      return R;
   end SSID_Hex;

   function Hex16 (V : Unsigned_16) return String is
     [for I in 1 .. 4 => Hex_Digits (Natural (Shift_Right (V, 4 * (4 - I)) and 16#F#) + 1)];

   function Note_Text (N : ST.EAPOL_Note) return String is
     (case N is
        when ST.None => "none",
        when ST.Not_Expected => "ignored (no handshake under way)",
        when ST.Sent_Message_2 => "message 1 accepted, message 2 sent",
        when ST.Sent_Message_4 => "message 3 accepted, message 4 sent, keys installed",
        when ST.Sent_Group_2 => "group message 1 accepted, group message 2 sent",
        when ST.Ignored => "ignored (not for this state)",
        when ST.Replay_Refused => "refused: replay counter not newer",
        when ST.MIC_Failed => "refused: MIC check failed (wrong passphrase?)",
        when ST.Nonce_Changed => "refused: ANonce changed",
        when ST.RSN_Mismatch => "refused: RSN element differs from the beacon's",
        when ST.Key_Data_Refused => "refused: key data did not unwrap",
        when ST.Malformed => "refused: malformed EAPOL-Key",
        when ST.Group_Cipher_Refused => "refused: group cipher not CCMP");

   function SSID_Text (S : C.SSID) return String is
      R : String (1 .. S.Length);
   begin
      for I in 1 .. S.Length loop
         R (I) := (if S.Octets (I) in 32 .. 126 then Character'Val (S.Octets (I)) else '?');
      end loop;
      return R;
   end SSID_Text;

   ---------------------------------------------------------------------------
   --  The driver
   ---------------------------------------------------------------------------
   Driver_PID : Process_ID := No_Process;
   Grant_Base : System.Address := System.Null_Address;
   Grant      : CuBit.Memory_Grants.Grant_Reference;
   Own_Address : NL.Hardware_Address := NL.No_Address;
   Receiving  : Ring.Consumer := Ring.New_Consumer;
   Sending    : Ring.Producer := Ring.New_Producer;

   type Parked_Reply is record
      Present : Boolean := False;
      Handle  : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
   end record;
   Command_Wait : Parked_Reply;

   --  Commands for the driver, oldest first (each answers one wait).
   Queue_Capacity : constant := 16;
   type Command_Queue is array (1 .. Queue_Capacity) of ST.Output;
   Queue : Command_Queue;
   Queued : Natural range 0 .. Queue_Capacity := 0;
   --  Frames were put in the transmit ring since the last answer.
   Frames_Waiting : Boolean := False;

   function Word_At (Offset : Natural) return System.Address is
     (Grant_Base + Storage_Offset (Offset));

   procedure Fence is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Fence;

   function Load32 (Offset : Natural) return Unsigned_32 is
      Cell : Unsigned_32 with Volatile, Import, Address => Word_At (Offset);
   begin
      return Cell;
   end Load32;

   procedure Store32 (Offset : Natural; Value : Unsigned_32) is
      Cell : Unsigned_32 with Volatile, Import, Address => Word_At (Offset);
   begin
      Cell := Value;
   end Store32;

   --  Put a frame in the transmit ring.
   procedure Send_Frame (Kind : D.Frame_Kind; Frame : ST.Frame_Buffer; Length : Natural) is
      OK : Boolean;
   begin
      if Grant_Base = System.Null_Address or else Length not in 1 .. D.Maximum_Frame then
         return;
      end if;
      Ring.Accept_Consumed (Sending, Ring.Index (Load32 (D.Transmit_Header_At + D.Consumed_At)), OK);
      if not OK or else Ring.Space (Sending) = 0 then
         Say ("wifi: transmit ring full, frame dropped", Warn => True);
         return;
      end if;
      declare
         Slot_At : constant Natural := D.Transmit_Slots_At + Ring.Next_Slot (Sending) * D.Slot_Bytes;
         Bytes : array (0 .. D.Slot_Bytes - 1) of Unsigned_8 with Import, Volatile, Address => Word_At (Slot_At);
      begin
         for I in 0 .. Length - 1 loop
            Bytes (D.Frame_At + I) := Frame (I);
         end loop;
         Store32 (Slot_At + D.Length_At, Unsigned_32 (Length));
         Store32 (Slot_At + D.Kind_At, Unsigned_32 (D.Frame_Kind'Enum_Rep (Kind)));
      end;
      Ring.Commit (Sending);
      Fence;
      Store32 (D.Transmit_Header_At + D.Produced_At, Unsigned_32 (Sending.Produced));
      Frames_Waiting := True;
   end Send_Frame;

   procedure Enqueue (O : ST.Output) is
   begin
      if Queued = Queue_Capacity then
         Say ("wifi: driver command queue full", Warn => True);
         return;
      end if;
      Queued := Queued + 1;
      Queue (Queued) := O;
   end Enqueue;

   --  Answer the parked wait with the next command, or with Transmit.
   procedure Answer_Wait is
      Msg : Message := NULL_MESSAGE;
      Ignore : Unsigned_64;
   begin
      if not Command_Wait.Present then
         return;
      end if;
      if Queued > 0 then
         declare
            O : constant ST.Output := Queue (1);
         begin
            Queue (1 .. Queued - 1) := Queue (2 .. Queued);
            Queued := Queued - 1;
            case O.Kind is
               when ST.Do_Join =>
                  declare
                     W : constant D.Words2 := D.Encode (O.Join);
                  begin
                     Msg.tag := (label => D.Label (D.Join), length => 2, protocol => 0, ipc => <>, reserved => 0);
                     Msg.words := [W (0), W (1), 0, 0];
                  end;
               when ST.Do_Associated =>
                  Msg.tag := (label => D.Label (D.Associated), length => 1, protocol => 0, ipc => <>, reserved => 0);
                  Msg.words := [D.Associated_Word (O.AID, O.Interval), 0, 0, 0];
               when ST.Do_Install_Key =>
                  declare
                     W : constant D.Words4 := D.Encode (O.Key);
                  begin
                     Msg.tag := (label => D.Label (D.Install_Key), length => 4, protocol => 0, ipc => <>,
                                 reserved => 0);
                     Msg.words := [W (0), W (1), W (2), W (3)];
                  end;
               when ST.Do_Port =>
                  Msg.tag := (label => D.Label (D.Port), length => 1, protocol => 0, ipc => <>, reserved => 0);
                  Msg.words := [Unsigned_64 (D.Port_State'Enum_Rep (O.Port)), 0, 0, 0];
               when ST.Do_Leave =>
                  Msg.tag := (label => D.Label (D.Leave), length => 0, protocol => 0, ipc => <>, reserved => 0);
               when ST.Send_Management | ST.Send_EAPOL =>
                  return;   --  never queued
            end case;
         end;
      elsif Frames_Waiting then
         Msg.tag := (label => D.Label (D.Transmit), length => 0, protocol => 0, ipc => <>, reserved => 0);
      else
         return;
      end if;
      Frames_Waiting := False;
      Command_Wait.Present := False;
      Ignore := replyCap (Command_Wait.Handle, Msg);
   end Answer_Wait;

   ---------------------------------------------------------------------------
   --  The station
   ---------------------------------------------------------------------------
   Station : ST.Station;
   Joined_SSID : C.SSID;

   procedure Perform (L : ST.Outputs) is
   begin
      for I in 1 .. L.Count loop
         case L.Items (I).Kind is
            when ST.Send_Management => Send_Frame (D.Management, L.Items (I).Frame, L.Items (I).Length);
            when ST.Send_EAPOL      => Send_Frame (D.EAPOL, L.Items (I).Frame, L.Items (I).Length);
            when others             => Enqueue (L.Items (I));
         end case;
      end loop;
      Answer_Wait;
   end Perform;

   function Now return M.Milliseconds is (CuBit.Monotonic.Milliseconds);

   --  The driver's scan block, read under its seqlock.
   procedure Read_Scan (Result : out WS.Scan_Result; Valid : out Boolean) is
      Block : WS.Block;
      Before, After : Unsigned_32;
   begin
      Valid := False;
      Result := (others => <>);
      if Grant_Base = System.Null_Address then
         return;
      end if;
      for Attempt in 1 .. 4 loop
         Before := Load32 (D.Scan_At + WS.Generation_Word * WS.Word_Bytes);
         Fence;
         for W in WS.Block_Word_Index loop
            Block (W) := Load32 (D.Scan_At + W * WS.Word_Bytes);
         end loop;
         Fence;
         After := Load32 (D.Scan_At + WS.Generation_Word * WS.Word_Bytes);
         if Before = After and then Before mod 2 = 0 and then Before /= 0 then
            declare
               R : constant WS.Decoded_Result := WS.Decode (Block);
            begin
               if R.Valid then
                  Result := R.Item;
                  Valid := True;
               end if;
               return;
            end;
         end if;
      end loop;
   end Read_Scan;

   --  The passphrase for SSID, turned into the PMK (then wiped).
   procedure Make_PMK (S : C.SSID; PMK : out H.PMK_Value; Found : out Boolean) is
      Stored : Startup.Settings.Wifi_Secret.Reading := Startup.Settings.Wifi_Secret.Read (SSID_Hex (S));
   begin
      PMK := [others => 0];
      Found := False;
      if Stored.Kind /= Present then
         return;
      end if;
      declare
         Text : constant String := CuBit.Settings.Image (Stored.Item);
      begin
         if Text'Length not in C.Passphrase_Length then
            return;
         end if;
         declare
            Pass : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (Text'Length) - 1);
            Name : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (S.Length) - 1);
            Key : W2.PMK;
         begin
            for I in Pass'Range loop
               Pass (I) := Character'Pos (Text (Text'First + Natural (I)));
            end loop;
            for I in Name'Range loop
               Name (I) := SPARKNaCl.Byte (S.Octets (Natural (I) + 1));
            end loop;
            W2.Passphrase_To_PMK (Key, Pass, Name);
            for I in PMK'Range loop
               PMK (I) := Unsigned_8 (Key (SPARKNaCl.N32 (I)));
            end loop;
            SPARKNaCl.Sanitize (Key);
            SPARKNaCl.Sanitize (Pass);
            Found := True;
         end;
      end;
      Stored := (Kind => Absent);
   end Make_PMK;

   function Matches (E : WS.BSS_Entry; S : C.SSID) return Boolean is
     (E.Length = S.Length and then (for all I in 1 .. S.Length => E.SSID (I) = S.Octets (I)));

   procedure Join (S : C.SSID; Why : out C.Refusal; Started : out Boolean) is
      Scan : WS.Scan_Result;
      Valid : Boolean;
      Best : Natural := 0;
      PMK : H.PMK_Value := [others => 0];
      Nonce_Bytes : CuBit.Hardware_Random.Byte_Array (1 .. E.Nonce_Bytes);
      S_Nonce : E.Nonce := [others => 0];
      Kind : ST.Security;
      L : ST.Outputs;
      Accepted, Found, Random_OK : Boolean;
   begin
      Started := False;
      Why := C.No_Radio;
      if Driver_PID = No_Process then
         return;
      end if;
      Read_Scan (Scan, Valid);
      Why := C.Not_Found;
      if not Valid then
         return;
      end if;
      for I in 1 .. Scan.Summary.Count loop
         if Matches (Scan.Entries (I), S)
           and then (Best = 0 or else Scan.Entries (I).Signal > Scan.Entries (Best).Signal)
         then
            Best := I;
         end if;
      end loop;
      if Best = 0 then
         return;
      end if;
      if Scan.Entries (Best).RSN then
         Kind := ST.WPA2_PSK;
      elsif WS.Privacy (Scan.Entries (Best)) then
         Why := C.Not_Joinable;   --  WEP or WPA1: never
         return;
      else
         Kind := ST.Open;
      end if;
      if Kind = ST.WPA2_PSK then
         Make_PMK (S, PMK, Found);
         if not Found then
            Why := C.No_Passphrase;
            return;
         end if;
         CuBit.Hardware_Random.Fill (Nonce_Bytes, Random_OK);
         if not Random_OK then
            Why := C.Not_Joinable;
            return;
         end if;
         S_Nonce := [for I in E.Nonce'Range => Nonce_Bytes (I + 1)];
         Nonce_Bytes := [others => 0];
      end if;
      ST.Join (Station, Scan.Entries (Best), Kind, PMK, S_Nonce, Now, L, Accepted);
      PMK := [others => 0];
      if not Accepted then
         Why := C.Busy;
         return;
      end if;
      Joined_SSID := S;
      Started := True;
      Say ("wifi: joining """ & SSID_Text (S) & """ channel "
           & Image (Unsigned_64 (Scan.Entries (Best).Channel))
           & (if Kind = ST.WPA2_PSK then " wpa2-psk" else " open"));
      Perform (L);
   end Join;

   Last_State : C.State := C.No_Radio;

   procedure Report_State is
      State : constant C.State := (if Driver_PID = No_Process then C.No_Radio else ST.State_Of (Station));
   begin
      if State /= Last_State then
         Last_State := State;
         Say ("wifi: " & (case State is
                            when C.No_Radio => "no radio",
                            when C.Idle => "idle (" & M.Failure'Image (ST.Last_Failure (Station)) & ")",
                            when C.Joining => "joining",
                            when C.Handshaking => "4-way handshake",
                            when C.Connected => "connected to """ & SSID_Text (Joined_SSID) & """"),
              Warn => State = C.Idle and then ST.Last_Failure (Station) /= M.None
                      and then ST.Last_Failure (Station) /= M.Left);
      end if;
   end Report_State;

   --  Frames the driver put in the receive ring.
   procedure Drain_Receive is
      OK : Boolean;
      L : ST.Outputs;
   begin
      if Grant_Base = System.Null_Address then
         return;
      end if;
      Fence;
      Ring.Accept_Produced (Receiving, Ring.Index (Load32 (D.Receive_Header_At + D.Produced_At)), OK);
      if not OK then
         Say ("wifi: driver receive index refused", Warn => True);
         return;
      end if;
      while Receiving.Available > 0 loop
         declare
            Slot_At : constant Natural := D.Receive_Slots_At + Ring.Head_Slot (Receiving) * D.Slot_Bytes;
            Length : constant Unsigned_32 := Load32 (Slot_At + D.Length_At);
            Kind_Word : constant Unsigned_32 := Load32 (Slot_At + D.Kind_At);
         begin
            if D.Fits (Length) then
               declare
                  Bytes : array (0 .. D.Slot_Bytes - 1) of Unsigned_8 with Import, Volatile, Address => Word_At (Slot_At);
                  Frame : ST.Frame_Bytes (0 .. Natural (Length) - 1);
               begin
                  for I in Frame'Range loop
                     Frame (I) := Bytes (D.Frame_At + I);
                  end loop;
                  if Kind_Word = D.Frame_Kind'Enum_Rep (D.Management) and then Frame'Length <= 4_096 then
                     ST.Management (Station, Frame, Now, L);
                     Perform (L);
                  elsif Kind_Word = D.Frame_Kind'Enum_Rep (D.EAPOL) and then Frame'Length <= E.Maximum_Frame then
                     ST.EAPOL (Station, Frame, Now, L);
                     Say ("wifi: eapol rx len=" & Image (Unsigned_64 (Frame'Length)) & " key-info=0x"
                          & Hex16 (if Frame'Length > E.Key_Information_At + 1
                                   then Shift_Left (Unsigned_16 (Frame (E.Key_Information_At)), 8)
                                        or Unsigned_16 (Frame (E.Key_Information_At + 1)) else 0)
                          & " -> " & Note_Text (ST.Counts (Station).Last_EAPOL),
                          Warn => ST.Counts (Station).Last_EAPOL not in
                                    ST.Sent_Message_2 | ST.Sent_Message_4 | ST.Sent_Group_2);
                     Perform (L);
                  end if;
               end;
            end if;
         end;
         Ring.Release (Receiving);
      end loop;
      Store32 (D.Receive_Header_At + D.Consumed_At, Unsigned_32 (Receiving.Consumed));
   end Drain_Receive;

   ---------------------------------------------------------------------------
   --  Requests
   ---------------------------------------------------------------------------
   procedure Answer (To : Process_ID; Label : Unsigned_32; W0, W1 : Unsigned_64 := 0) is
      Ignore : Unsigned_64;
   begin
      Ignore := reply (To, (tag => (label => Label, length => 2, protocol => 0, ipc => <>, reserved => 0),
                            authorityTag => 0, words => [W0, W1, 0, 0]));
   end Answer;

   procedure Refuse (To : Process_ID; Why : C.Refusal) is
   begin
      Answer (To, REPLY_ERR, Unsigned_64 (C.Refusal'Enum_Rep (Why)));
   end Refuse;

   Grant_Pages : constant := D.Grant_Pages;

   procedure Attach (From : Process_ID; Msg : Message) is
      Base : Unsigned_64;
      OK : Boolean;
   begin
      if Driver_PID /= No_Process or else not NL.Valid_Address_Word (Msg.words (0)) then
         Answer (From, REPLY_ERR);
         return;
      end if;
      Base := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Allocate_Owned_Memory, Unsigned_64 (D.Grant_Bytes));
      if Base = CuBit.Kernel_ABI.Failed then
         Answer (From, REPLY_ERR);
         return;
      end if;
      Grant_Base := To_Address (Integer_Address (Base));
      CuBit.Memory_Grants.Create_For_Process
        (grantee => From, localAddr => Grant_Base, numPages => Grant_Pages, readWrite => True,
         reference => Grant, success => OK, notify => True);
      if not OK then
         Grant_Base := System.Null_Address;
         Answer (From, REPLY_ERR);
         return;
      end if;
      Driver_PID := From;
      Own_Address := NL.Address_Of (Msg.words (0));
      Receiving := Ring.New_Consumer;
      Sending := Ring.New_Producer;
      ST.Reset (Station, Own_Address, Join_Limits, Beacon_Timeout);
      Answer (From, REPLY_OK, The_Handle, CuBit.Grant_References.Encode (Grant));
      Say ("wifi: radio attached");
   end Attach;

   procedure Driver_Request (From : Process_ID; Msg : Message) is
      L : ST.Outputs;
   begin
      case D.Operation_Of (Msg.tag.label) is
         when D.Attach =>
            Attach (From, Msg);
         when D.Receive_Doorbell =>
            if From = Driver_PID then
               Drain_Receive;
            end if;
         when D.Command_Wait =>
            if From /= Driver_PID then
               Answer (From, REPLY_ERR);
               return;
            end if;
            declare
               Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
            begin
               if Saved.Saved then
                  Command_Wait := (Present => True, Handle => Saved.Handle);
               end if;
            end;
            Answer_Wait;
         when D.Command_Done =>
            if From = Driver_PID
              and then Msg.words (2) /= Unsigned_64 (D.Outcome'Enum_Rep (D.Done))
            then
               Say ("wifi: driver refused command 0x" & Image (Msg.words (1)) & " outcome "
                    & Image (Msg.words (2)), Warn => True);
               ST.Radio_Fault (Station, Now, L);
               Perform (L);
            end if;
      end case;
   end Driver_Request;

   procedure Store_Passphrase (From : Process_ID; Msg : Message) is
      Reference : constant CuBit.Memory_Grants.Grant_Reference :=
        (slot => Msg.words (0), generation => Msg.words (1));
      SSID_Bytes : constant Unsigned_64 := Msg.words (2) and 16#FF#;
      Pass_Bytes : constant Unsigned_64 := Shift_Right (Msg.words (2), C.Passphrase_Shift) and 16#FF#;
      Address : System.Address;
      Acquired, Returned : Boolean;
      Result : CuBit.Settings.Change_Outcome;
   begin
      if SSID_Bytes not in 1 .. C.Maximum_SSID
        or else Pass_Bytes not in Unsigned_64 (C.Passphrase_Length'First) .. Unsigned_64 (C.Passphrase_Length'Last)
        or else Msg.words (0) > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT
        or else Msg.words (1) not in CuBit.Memory_Grants.Grant_Generation
      then
         Refuse (From, C.Malformed);
         return;
      end if;
      CuBit.Memory_Grants.Acquire (Reference, From, 0, C.Page_Bytes, CuBit.Memory_Grants.Read_Access,
                                   Address, Acquired);
      if not Acquired then
         Refuse (From, C.Malformed);
         return;
      end if;
      declare
         Page : String (1 .. C.Page_Bytes) with Import, Volatile, Address => Address;
         S : C.SSID;
         Pass : CuBit.Settings.Text;
         Printable : Boolean := True;
      begin
         S.Length := Natural (SSID_Bytes);
         for I in 1 .. S.Length loop
            S.Octets (I) := Character'Pos (Page (I));
         end loop;
         Pass.Length := Natural (Pass_Bytes);
         for I in 1 .. Pass.Length loop
            Pass.Data (I) := Page (S.Length + I);
            Printable := Printable and then Character'Pos (Pass.Data (I)) in C.First_Printable .. C.Last_Printable;
         end loop;
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         if not Printable then
            Pass := (others => <>);
            Refuse (From, C.Malformed);
            return;
         end if;
         Startup.Settings.Wifi_Secret.Write (SSID_Hex (S), Pass, Result);
         Pass := (others => <>);
         if Result = Done then
            Say ("wifi: passphrase stored for """ & SSID_Text (S) & """");
            Answer (From, REPLY_OK);
         else
            Refuse (From, C.Not_Stored);
         end if;
      end;
   end Store_Passphrase;

   function Failure_Of (F : M.Failure) return C.Failure is
     (case F is
        when M.None => C.None,
        when M.Authentication_Refused => C.Authentication_Refused,
        when M.Authentication_Timeout => C.Authentication_Timeout,
        when M.Association_Refused => C.Association_Refused,
        when M.Association_Timeout => C.Association_Timeout,
        when M.Handshake_Failed => C.Handshake_Failed,
        when M.Handshake_Timeout => C.Handshake_Timeout,
        when M.Deauthenticated => C.Deauthenticated,
        when M.Disassociated => C.Disassociated,
        when M.Left => C.Left);

   procedure Control_Request (From : Process_ID; Msg : Message) is
      L : ST.Outputs;
   begin
      case C.Operation_Of (Msg.tag.label) is
         when C.Join =>
            declare
               S : constant C.Decoded_SSID := C.Decode ([Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)]);
               Why : C.Refusal;
               Started : Boolean;
            begin
               if not S.Valid then
                  Refuse (From, C.Malformed);
                  return;
               end if;
               Join (S.Item, Why, Started);
               if Started then
                  Answer (From, REPLY_OK);
               else
                  Refuse (From, Why);
               end if;
            end;
         when C.Leave =>
            ST.Leave (Station, Now, L);
            Perform (L);
            Answer (From, REPLY_OK);
         when C.Status =>
            declare
               State : constant C.State :=
                 (if Driver_PID = No_Process then C.No_Radio else ST.State_Of (Station));
               T : constant WS.BSS_Entry := ST.Target_Of (Station);
               Active : constant Boolean := State in C.Joining .. C.Connected;
            begin
               Answer (From, REPLY_OK,
                       Unsigned_64 (C.State'Enum_Rep (State))
                       or Shift_Left (Unsigned_64 (C.Failure'Pos (Failure_Of (ST.Last_Failure (Station)))),
                                      C.Failure_Shift)
                       or (if Active then Shift_Left (Unsigned_64 (WS.Band'Enum_Rep (T.On_Band)), C.Band_Shift)
                           or Shift_Left (Unsigned_64 (T.Channel), C.Channel_Shift) else 0),
                       (if Active then NL.Address_Word (T.BSSID) else 0));
            end;
         when C.Set_Passphrase =>
            Store_Passphrase (From, Msg);
      end case;
   end Control_Request;

   Ignore : Unsigned_64;
begin
   Say ("wifi: starting");
   Ignore := registerDriver (DRIVER_WIFI, Startup.Registration);
   ST.Reset (Station, NL.No_Address, Join_Limits, Beacon_Timeout);
   declare
      Ready : Message := (tag => (label => Ready_Label, length => 0, protocol => 0, ipc => <>, reserved => 0),
                          authorityTag => 0, words => (others => 0));
   begin
      Ignore := Unsigned_64 (capCall (Startup.Ready, Ready, Wait_Forever).label);
   end;

   loop
      declare
         Due : constant M.Milliseconds := ST.Deadline (Station);
         Activity : constant Activity_Result :=
           Wait_For_Activity_Until (if Due = 0 then Wait_Forever else Due);
         Entry_Found : CompletionEntry;
         Sender : Process_ID;
         Request : Message;
         Found : Boolean;
         L : ST.Outputs;
      begin
         exit when Activity = Unavailable;
         while Poll_Completion (Entry_Found'Address) = 1 loop
            if not CuBit.Granted_Logs.Take (Entry_Found) then
               null;
            end if;
         end loop;
         CuBit.Granted_Logs.Request (Startup.Ready);
         loop
            Poll_Service_Request (Sender, Request, Found);
            exit when not Found;
            if D.Is_Operation (Request.tag.label) then
               Driver_Request (Sender, Request);
            elsif C.Is_Operation (Request.tag.label) then
               Control_Request (Sender, Request);
            else
               Answer (Sender, REPLY_ERR);
            end if;
         end loop;
         if ST.Deadline (Station) /= 0 and then Now >= ST.Deadline (Station) then
            ST.Tick (Station, Now, L);
            Perform (L);
         end if;
         Report_State;
      end;
   end loop;
   Say ("wifi: activity wait unavailable", Warn => True);
end Main;
