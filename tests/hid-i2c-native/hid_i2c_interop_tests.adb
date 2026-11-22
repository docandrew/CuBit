pragma Ada_2022;
with Ada.Text_IO; with I2C_Channel_Controller; with I2C_Channel; with I2C_Channel_Wire;
with HID_I2C_Buffered_Client; with HID_I2C_Descriptors; with I2C_Transfer_Wire;
with CuBit.Process_IDs; with I2C_Channel_Badges;
with DW_I2C_Transfer; with DW_I2C_Registers; with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset; with Intel_LPSS_I2C_Profile; with System;
procedure HID_I2C_Interop_Tests is
   package T renames DW_I2C_Transfer;
   package R renames DW_I2C_Registers;
   package C renames DW_I2C_Configuration;
   package L renames Intel_LPSS_I2C_Reset;
   package P renames Intel_LPSS_I2C_Profile;
   use type T.Word;
   use type T.Count;
   use type T.Byte;
   use type C.Register_ID, R.Outcome;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   Registers : array (Natural range 0 .. 191) of T.Word := [others => 0];
   Raw_IRQ, Abort_Source, RX_Count, Cursor : T.Word := 0;
   IO_Count, Data_Calls, Commands, Fail_Command : Natural := 0;
   Fail_Status : R.Access_Status := R.Access_OK;
   Clock_OK, Lose_Clock_On_Data : Boolean := True;
   Inject_Abort : Boolean := False;
   Unknown_Cleanup, Dirty_TX, Late_Abort : Boolean := False;
   Current_Time : T.Tick := 1;
   Command_Log : array (1 .. 32) of T.Word := [others => 0];
   function Index (Offset : Natural) return Natural is (Offset / 4);
   procedure Reset_Model is
   begin
      Registers := [others => 0]; Registers (Index (16#204#)) := L.Release_Resets;
      Registers (Index (16#FC#)) := R.Component_Signature;
      Registers (Index (16#F4#)) := 16#10100#;
      Registers (Index (16#F8#)) := C.Minimum_Hold_Version;
      Raw_IRQ := 0; Abort_Source := 0; RX_Count := 0; Cursor := 0;
      IO_Count := 0; Data_Calls := 0; Commands := 0; Fail_Command := 0;
      Fail_Status := R.Access_OK; Clock_OK := True; Lose_Clock_On_Data := False;
      Inject_Abort := False; Current_Time := 1; Command_Log := [others => 0];
      Unknown_Cleanup := False; Dirty_TX := False; Late_Abort := False;
   end Reset_Model;
   procedure Clock (Now : out T.Tick; Available : out Boolean) is
   begin Now := Current_Time; Available := Clock_OK; end Clock;
   procedure Reset_Read (ID : L.Register_ID; Value : out L.Word; Status : out L.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Value := Registers (Index (Natural (L.Offset (ID))));
      Status := L.Access_OK;
   end Reset_Read;
   procedure Reset_Write (ID : L.Register_ID; Value : L.Word; Status : out L.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Registers (Index (Natural (L.Offset (ID)))) := Value;
      Status := L.Access_OK;
   end Reset_Write;
   procedure Config_Read (ID : C.Register_ID; Value : out C.Word; Status : out C.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Value := 0; Status := C.Access_OK;
      case ID is
         when C.Enable_Status => Value := Registers (Index (16#6C#)) and C.Enabled_Bit;
         when C.Controller_Status => null;
         when C.TX_Level => Value := (if Dirty_TX and then Commands > 0 then 1 else 0);
         when C.RX_Level => Value := RX_Count;
         when C.Abort_Source => Value := Abort_Source;
         when C.Clear_Interrupts => Raw_IRQ := 0; Abort_Source := 0;
         when C.Data_Command =>
            Check (RX_Count > 0, "stale drain nonempty"); RX_Count := RX_Count - 1;
         when others => Value := Registers (Index (Natural (C.Offset (ID))));
      end case;
   end Config_Read;
   procedure Config_Write (ID : C.Register_ID; Value : C.Word; Status : out C.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Registers (Index (Natural (C.Offset (ID)))) := Value;
      if ID = C.Enable and then Value = C.Enabled_Bit then Cursor := 0; end if;
      Status := C.Access_OK;
      if ID = C.Enable and then Value = 0 and then Commands > 0 then
         if Unknown_Cleanup then Status := C.Unknown_Effect; end if;
         if Late_Abort then Abort_Source := 8; Raw_IRQ := Raw_IRQ or R.Abort_Interrupt; end if;
      end if;
   end Config_Write;
   procedure Data_Read (ID : R.Read_Register; Value : out R.Word; Status : out R.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Status := R.Access_OK;
      case ID is
         when R.Raw_Interrupts => Value := Raw_IRQ;
         when R.Abort_Source => Value := Abort_Source;
         when R.TX_Level => Value := 0;
         when R.RX_Level => Value := RX_Count;
         when R.Read_Data_Command =>
            Check (RX_Count > 0, "owned input read nonempty");
            RX_Count := RX_Count - 1; Cursor := Cursor + 1; Value := 16#A0# + Cursor;
         when others => Value := Registers (Index (Natural (R.Offset (ID))));
      end case;
   end Data_Read;
   procedure Data_Write (ID : R.Write_Register; Value : R.Word; Status : out R.Access_Status) is
      pragma Unreferenced (ID);
   begin
      IO_Count := IO_Count + 1; Data_Calls := Data_Calls + 1;
      Status := (if Data_Calls = Fail_Command then Fail_Status else R.Access_OK);
      if Status in R.Denied | R.Unavailable then return; end if;
      Commands := Commands + 1; Command_Log (Commands) := Value;
      if (Value and T.Read_Command_Bit) /= 0 then
         RX_Count := RX_Count + 1; Check (RX_Count <= 2, "RX credit respected by composed owner");
      end if;
      if (Value and T.Stop_Command_Bit) /= 0 then Raw_IRQ := Raw_IRQ or R.Stop_Interrupt; end if;
      if Inject_Abort then Raw_IRQ := Raw_IRQ or R.Abort_Interrupt; Abort_Source := 8; end if;
      if Lose_Clock_On_Data then Clock_OK := False; end if;
   end Data_Write;

   package W renames I2C_Transfer_Wire;
   package CW renames I2C_Channel_Wire;
   package D renames HID_I2C_Descriptors;
   package PID renames CuBit.Process_IDs;
   package B renames I2C_Channel_Badges;
   use type W.Word, W.Count, W.Completion_Status, CW.Decode_Status;
   procedure Run_Case (Which : Positive) is
      package Bridge is new I2C_Channel_Controller
        (Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write);
      package O renames Bridge.Controller;
      package RC renames I2C_Channel.RC;
      use type O.Phase, O.Admission, O.Collection, O.Failure, Bridge.Submission, Bridge.Releasing, T.Ticket;
      type Area is array (Natural range <>) of D.Byte with Component_Size => 8;
      Payload_Bytes : constant := I2C_Channel.Payload_Pages * RC.Page_Bytes;
      Input : aliased Area (0 .. Payload_Bytes - 1) := [others => 16#CD#] with Alignment => 4096;
      Output : aliased Area (0 .. Payload_Bytes - 1) := [others => 16#CD#] with Alignment => 4096;
      Input_View : RC.Payload_View := (Input'Address, Payload_Bytes, False);
      Output_View : RC.Payload_View := (Output'Address, Payload_Bytes, True);
      Peer : constant PID.Process_ID := PID.From_Word (16#E234_0000_1234_5678#);
      Opens, Stores, Loads, Submissions, Answers, Closes : Natural := 0;
      Queued, Available, Lost, Retired : Boolean := False;
      Header : CW.Raw_Request;
      Saved_Token : W.Word := 0;
      Response : W.Completion := (W.Invalid_Frame, 0, False, False, 0, 0);
      procedure Open_Channel (Expected : PID.Process_ID; Deadline : W.Millisecond; Ready : out Boolean) is
         use type PID.Process_ID;
      begin
         Check (Expected = Peer and Deadline = 1000, "full peer and finite open deadline");
         Opens := Opens + 1; Ready := True;
      end Open_Channel;
      procedure Store (Data : D.Bytes; Stored : out Boolean) is
      begin
         Check (not Queued and not Bridge.Has_Payload, "client writes only before publication");
         for I in Data'Range loop Input (I - Data'First) := Data (I); end loop;
         Stores := Stores + 1; Stored := True;
      end Store;
      procedure Load (Data : out D.Bytes; Loaded : out Boolean) is
      begin
         Check (not Bridge.Has_Payload, "snapshot only after owner releases payload");
         for I in Data'Range loop Data (I) := Output (I - Data'First); end loop;
         Output := [others => 16#EE#]; Loads := Loads + 1; Loaded := True;
      end Load;
      procedure Submit (Raw : CW.Raw_Request; Token : W.Word; Accepted : out Boolean) is
      begin
         Check (not Queued and Token > 0, "one deferred request");
         Header := Raw; Saved_Token := Token; Queued := True;
         Submissions := Submissions + 1; Accepted := True;
      end Submit;
      procedure Poll (Token : W.Word; Words : out W.Completion_Words; Ready, Ended : out Boolean) is
      begin
         Check (Token = Saved_Token, "exact channel token"); Words := W.Encode_Completion (Response);
         Ready := Available; Ended := Lost;
         if Ready then Available := False; end if;
      end Poll;
      procedure Close_Channel is
      begin Closes := Closes + 1; Check (Closes = 1, "close exactly once"); end Close_Channel;
      function Retirement_Confirmed return Boolean is (Retired and not Bridge.Has_Payload);
      package Client is new HID_I2C_Buffered_Client
        (16, Open_Channel, Store, Load, Submit, Poll, Close_Channel, Retirement_Confirmed);
      package Life renames Client.Transactions;
      use type Client.Setup_Result, Client.Start_Result, Client.Close_Result, Life.Phase;
      S : Client.State; Setup : Client.Setup_Result; Started : Client.Start_Result; Closed : Client.Close_Result;
      Writes : constant Natural := (if Which in 3 | 12 then 0 elsif Which = 11 then 16 elsif Which = 2 then 5 else 2);
      Reads : constant W.Count := (if Which in 2 | 11 then 0 elsif Which = 12 then 16 elsif Which = 13 then 14 elsif Which = 3 then 5 else 3);
      Data : D.Bytes (1 .. Writes); Result : D.Bytes (1 .. Natural (Reads)); OK : Boolean;
      Admission : O.Admission; Submitted : Bridge.Submission; Released : Bridge.Releasing;
      Collection : O.Collection; Item : O.Completion; ID, Refused : T.Ticket;
      Badge : B.Word := B.Canonical ((B.Seven_Bit, 16#15#, 400_000));
      Steps : Natural := 0;
      Hardware_Fault : constant Boolean := Which in 8 | 14 .. 19;
      Clean_Refusal : constant Boolean := Which in 4 .. 6 | 9 .. 10 | 20 .. 24;
      Success : constant Boolean := not Hardware_Fault and not Clean_Refusal and Which /= 7;
      Before : Natural;
   begin
      Reset_Model;
      Bridge.Start ((P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096, 30, 40, 100), Admission);
      Check (Admission = O.Accepted, "actual controller startup admitted");
      while Bridge.Observe.At_Phase not in O.Available | O.Faulted loop
         Steps := Steps + 1; Check (Steps < 100, "bounded startup"); Bridge.Advance;
      end loop;
      Check (Bridge.Observe.At_Phase = O.Available and not Bridge.Observe.Controller_May_Be_Enabled,
         "startup confirms disabled controller");
      for I in Data'Range loop Data (I) := D.Byte (16#10# + I); end loop;
      Client.Initialize (S, Peer, 1000, Setup); Check (Setup = Client.Buffers_Ready, "client session ready");
      Client.Start (S, Data, Reads, W.Word (Which), 1, 100, True, Started);
      Check (Started = Client.Submitted and Submissions = 1, "one asynchronous request");
      Client.Read (S, Result, OK); Check (not OK and Loads = 0, "no early snapshot");
      Client.Close (S, Closed); Check (Closed = Client.Still_Owned and Closes = 0, "no close while active");
      case Which is
         when 4 => Output_View.Bytes := Payload_Bytes - 1;
         when 5 => Output_View.Base := Input_View.Base;
         when 6 => Badge := 0;
         when 8 => Inject_Abort := True;
         when 9 => Output_View.Writable := False;
         when 10 => Input_View.Writable := True;
         when 14 => Unknown_Cleanup := True;
         when 15 => Dirty_TX := True;
         when 16 => Late_Abort := True;
         when 17 => Lose_Clock_On_Data := True;
         when 18 => Fail_Command := 1; Fail_Status := R.Denied;
         when 19 => Fail_Command := 1; Fail_Status := R.Unknown_Effect;
         when 20 => Current_Time := 100;
         when 21 => Header.Reserved_A := 1;
         when 22 => Badge := B.Canonical ((B.Ten_Bit, 1023, 400_000));
         when 23 => Input_View.Base := System.Null_Address;
         when 24 => Badge := B.Canonical ((B.Seven_Bit, 16#15#, B.Hertz'Last));
         when others => null;
      end case;
      Before := IO_Count;
      Check (Queued and CW.Decode (Header, 16).Status = (if Which = 21 then CW.Reserved_Header else CW.Decoded), "copied metadata validated");
      Queued := False;
      Bridge.Submit (Badge, Header, Input_View, Output_View, ID, Submitted, Admission);
      if Clean_Refusal then
         Check (Submitted /= Bridge.Accepted and not Bridge.Has_Payload and IO_Count = Before,
            "refusal before register or payload access");
         Response := ((case Submitted is when Bridge.Invalid_Metadata => W.Invalid_Frame,
            when Bridge.Invalid_Channel => W.Invalid_Channel, when Bridge.Unsupported_Channel => W.Unsupported_Channel,
            when Bridge.Invalid_Views => W.Input_Refused, when others => W.Admission_Refused), 0, False, False, 0, 0);
         Check (ID = T.No_Ticket and Response.Status =
            (case Which is when 4 | 5 | 9 | 10 | 23 => W.Input_Refused,
             when 6 => W.Invalid_Channel, when 20 => W.Admission_Refused,
             when 21 => W.Invalid_Frame, when 22 | 24 => W.Unsupported_Channel,
             when others => W.Invalid_Frame), "exact clean refusal kind without hardware ticket");
         Available := True; Answers := Answers + 1;
      else
         Check (Submitted = Bridge.Accepted and Bridge.Has_Payload, "actual owner borrows persistent views");
         Steps := 0;
         while not Bridge.Observe.Completion_Pending loop
            Steps := Steps + 1; Check (Steps < 200, "bounded composed transfer"); Bridge.Advance;
         end loop;
         Bridge.Take_Completion (Item, Collection);
         Check (Collection = O.Collected and Item.ID = ID, "exact actual ticket");
         Bridge.Release (Released);
         if Hardware_Fault then
            Check (not Item.Successful and Released = Bridge.Retained and Bridge.Has_Payload,
               "hardware failure never permits buffer reuse");
            Check (Bridge.Observe.At_Phase = O.Faulted, "fault remains sticky after collection");
            Check (Item.Reason = (case Which is
               when 14 => O.Register_Indeterminate, when 15 => O.Controller_Not_Quiescent,
               when 16 => O.Abort_During_Quiescence,
               when others => O.Transfer_Failed), "exact retained failure kind " & Which'Image & " " & Item.Reason'Image);
            if Which in 17 .. 19 then
               Check (Bridge.Observe.Port_Status.Status =
                  (case Which is when 17 => R.Clock_Unavailable, when 18 => R.Register_Refused,
                   when others => R.Register_Indeterminate), "precise port diagnostic retained");
            end if;
            Before := IO_Count;
            Bridge.Submit (Badge, Header, Input_View, Output_View, Refused, Submitted, Admission);
            Check (Submitted = Bridge.Retained and Refused = T.No_Ticket and IO_Count = Before,
               "retained owner refuses next command without IO");
         else
            Check (Item.Successful and Released = Bridge.Released and not Bridge.Has_Payload,
               "success returns views before publishing completion");
            Check (Bridge.Observe.At_Phase = O.Available and not Bridge.Observe.Controller_May_Be_Enabled,
               "successful controller verified disabled and idle");
            Response := (W.Completed, W.Word (Item.ID), True, Item.Effects_Possible,
               W.Count (Item.Write_Count), W.Count (Item.Read_Count));
            Available := Which /= 7; Lost := Which = 7; Answers := Answers + 1;
            Check (Commands = Writes + Natural (Reads), "exact command count");
            Check ((Command_Log (Commands) and T.Stop_Command_Bit) /= 0, "STOP on final command");
            for I in Data'Range loop Check ((Command_Log (I) and 16#FF#) = T.Word (Data (I)), "exact client write bytes"); end loop;
            if Writes > 0 and Reads > 0 then
               Check ((Command_Log (Writes + 1) and T.Restart_Command_Bit) /= 0, "repeated START on combined read");
            end if;
            Check (Registers (Index (16#04#)) = 16#15#, "target comes only from admitted badge");
         end if;
      end if;
      Check (Input (Input'Last) = 16#CD# and Output (Output'Last) = 16#CD#,
         "controller does not clear or touch unused payload tail");
      Client.Step (S, 10, True);
      if Hardware_Fault then
         Check (Answers = 0 and Client.Observe (S).At_Phase = Life.In_Flight, "no recycling completion on fault");
         Client.Step (S, 100, True);
         Check (Client.Observe (S).At_Phase = Life.Retiring and Closes = 1, "client deadline initiates retirement once");
         Retired := True;
         for Turn in 1 .. 4 loop Client.Step (S, 101, True); end loop;
         Check (Client.Observe (S).At_Phase = Life.Retiring and Bridge.Has_Payload, "physical retained payload prevents false retirement");
         Client.Read (S, Result, OK); Check (not OK and Loads = 0, "failed input never exposed");
         Client.Close (S, Closed); Check (Closed = Client.Still_Owned and Closes = 1, "retained buffers cannot close");
         return;
      elsif Which = 7 then
         Check (Client.Observe (S).At_Phase = Life.Retiring and not Client.Succeeded (S), "lost channel does not trust successful hardware");
         Client.Read (S, Result, OK); Check (not OK and Loads = 0, "lost completion exposes no output");
         Retired := True; Client.Step (S, 11, True);
      end if;
      Check (Client.Observe (S).At_Phase = Life.Finished and Client.Succeeded (S) = Success, "client and controller outcome agree");
      Client.Read (S, Result, OK); Check (OK = (Success and Reads > 0), "only completed input snapshot allowed");
      if OK then
         for I in Result'Range loop Check (Result (I) = D.Byte (16#A0# + I), "FIFO bytes preserved after shared output mutates"); end loop;
      end if;
      Before := IO_Count;
      for Turn in 1 .. 3 loop Client.Step (S, 12, True); end loop;
      Check (IO_Count = Before and Submissions = 1 and Answers = 1, "no command replay");
      Client.Acknowledge (S); Client.Close (S, Closed);
      if Which /= 7 then
         Check (Closed = Client.Still_Owned and Closes = 1, "close waits for exact session retirement");
         Retired := True; Client.Close (S, Closed);
      end if;
      Check (Closed = Client.Closed and Closes = 1 and Opens = 1, "one session lifetime");
      Check (Stores = (if Writes = 0 then 0 else 1), "no whole payload clear or extra store");
   end Run_Case;
begin
   for Which in 1 .. 24 loop Run_Case (Which); end loop;
   Ada.Text_IO.Put_Line ("HID I2C channel interoperability checks" & Checks'Image);
end HID_I2C_Interop_Tests;
