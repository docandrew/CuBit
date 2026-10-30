pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces;
with System.Storage_Elements;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Devices;
with CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with I2C_Native_Controller;
with I2C_Native_Loop;
with I2C_Mapping_Policy;
with I2C_Channel_Badges;
with I2C_Configure_Replies;
with DW_I2C_Transfer;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with DW_I2C_Request_Types;
with Intel_LPSS_I2C_Reset;
procedure Loop_Tests is
   package T renames DW_I2C_Transfer;
   package R renames DW_I2C_Registers;
   package C renames DW_I2C_Configuration;
   package L renames Intel_LPSS_I2C_Reset;
   package Q renames DW_I2C_Request_Types;
   package W renames I2C_Configure_Replies;
   package B renames I2C_Channel_Badges;
   package PID renames CuBit.Process_IDs;
   package G renames CuBit.Grant_References;
   package M renames CuBit.Messages;
   package I renames CuBit.Handles.IPC;
   package Devices renames CuBit.Handles.Devices;
   package N is new I2C_Native_Controller (16);
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   use type T.Count;
   use type T.Byte;
   use type T.Ticket;
   use type PID.Process_ID;
   use type CuBit.Handles.Reply;
   use type G.Reference;
   use type Q.Copy_Result;
   use type Q.Reason;
   use type W.Reply_Status;
   use type N.Controller.Phase;
   use type R.Read_Register;
   use type C.Stage;
   use type C.Register_ID;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   Client : constant PID.Process_ID := PID.From_Word (16#FFFF_1234_5678_9999#);
   Input : constant G.Reference := (slot => 1, generation => 16#1234_5678#);
   Output : constant G.Reference := (slot => 2, generation => 16#8765_4321#);
   Badge : Interfaces.Unsigned_64 := B.Canonical ((B.Seven_Bit, 16#15#, 400_000));
   Input_Outcome, Output_Outcome : Q.Copy_Result := Q.Copied;
   Input_Copies, Output_Copies, Client_Replies, Startup_Reports : Natural := 0;
   Last_Client : Q.Response;
   Last_Configure : W.Reply;
   Last_Client_Reply : CuBit.Handles.Reply := 0;
   Configure_Delivered : Boolean := True;
   function Authorized (Caller : PID.Process_ID; Scope : Interfaces.Unsigned_64) return Boolean is
     (Caller = Client and then Scope = Badge);
   procedure Copy_Input (Caller : PID.Process_ID; Grant : G.Reference;
      Data : out N.Controller.Bytes; Result : out Q.Copy_Result) is
   begin
      Check (Caller = Client and then Grant = Input, "exact opaque owner/input generation reference");
      Input_Copies := Input_Copies + 1; Data := [others => 16#55#]; Result := Input_Outcome;
   end Copy_Input;
   procedure Copy_Output (Caller : PID.Process_ID; Grant : G.Reference;
      Data : N.Controller.Bytes; Result : out Q.Copy_Result) is
   begin
      Check (Caller = Client and then Grant = Output, "original opaque owner/output generation reference");
      Check (Data'Length = 2 and then Data (Data'First) = 16#A1# and then
             Data (Data'Last) = 16#A2#, "exact owned native-MMIO read prefix");
      Output_Copies := Output_Copies + 1; Result := Output_Outcome;
   end Copy_Output;
   procedure Answer_Client (Reply : CuBit.Handles.Reply; Item : Q.Response; Delivered : out Boolean);
   procedure Report_Startup (Item : W.Reply; Delivered : out Boolean);
   package D is new I2C_Native_Loop
     (N, Authorized, Copy_Input, Copy_Output, Answer_Client, Report_Startup);
   use type D.Phase;
   use type D.Stop_Reason;
   use type D.Start_Result;
   use type D.Submit_Result;
   use type D.Stop_Result;
   State, Second_State : D.State;
   Active_Second : Boolean := False;
   type Page_Array is array (Natural range 0 .. 1023) of Interfaces.Unsigned_32;
   Page : aliased Page_Array := [others => 0] with Alignment => 4096;
   Virtual : constant I2C_Mapping_Policy.Address :=
     I2C_Mapping_Policy.Address (System.Storage_Elements.To_Integer (Page'Address));
   Physical : constant := 16#1234_5000#;
   Facts : N.Controller.Startup :=
     (16#8086#, 16#54E9#, Physical, 4096, 64, 64, 256);
   Request : D.Requests.Request := (Input, Output, 1, 2, 1000, 256);
   Start_Result : D.Start_Result;
   Stop_Result : D.Stop_Result;
   Submitted : D.Submit_Result;
   Commands, Pending_RX, Received : Natural := 0;
   Data_Before : T.Word := 0;
   Which : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   function Index (Offset : Natural) return Natural is (Offset / 4);
   function RI (ID : R.Read_Register) return Natural is (Index (Natural (R.Offset (ID))));
   function CI (ID : C.Register_ID) return Natural is (Index (Natural (C.Offset (ID))));
   procedure Check_Reentry_For (Current : in out D.State) is
      Begin_Result : D.Start_Result;
      Submit_Result : D.Submit_Result;
      Stop_Result : D.Stop_Result;
      Before : constant Page_Array := Page;
      Clock_Before : constant Natural := M.Calls;
   begin
      Check (not D.Ready (Current), "client gate closed inside callbacks");
      D.Step (Current);
      D.Begin_Startup (Current, 1, Facts, Virtual, Begin_Result);
      D.Submit (Current, Client, Badge, 78, Request, Submit_Result);
      D.Request_Stop (Current, Stop_Result);
      Check (Begin_Result = D.Start_Reentry and then Submit_Result = D.Submit_Reentry
             and then Stop_Result = D.Stop_Reentry
             and then Page = Before and then M.Calls = Clock_Before,
             "callback reentry does no hardware work and takes no new authority");
   end Check_Reentry_For;
   procedure Check_Reentry is
   begin
      if Active_Second then Check_Reentry_For (Second_State);
      else Check_Reentry_For (State); end if;
   end Check_Reentry;
   procedure Answer_Client (Reply : CuBit.Handles.Reply; Item : Q.Response; Delivered : out Boolean) is
   begin
      Client_Replies := Client_Replies + 1; Last_Client := Item; Last_Client_Reply := Reply;
      Delivered := True; Check_Reentry;
   end Answer_Client;
   procedure Report_Startup (Item : W.Reply; Delivered : out Boolean) is
   begin
      Startup_Reports := Startup_Reports + 1; Last_Configure := Item;
      Delivered := Configure_Delivered; Check_Reentry;
   end Report_Startup;
   procedure Tick is
      Before : constant D.Snapshot := D.Observe (State);
      After : D.Snapshot;
      Value : T.Word;
   begin
      -- Explicit hosted device model: status mirrors enable; command writes
      -- schedule bytes/STOP; data reads consume one byte; CLR_INTR clears IRQ.
      -- These are modeled side effects, not evidence of actual hardware.
      Page (CI (C.Enable_Status)) := Page (CI (C.Enable)) and C.Enabled_Bit;
      Page (CI (C.Controller_Status)) := 0; Page (CI (C.TX_Level)) := 0;
      Page (CI (C.RX_Level)) := T.Word (Pending_RX);
      Data_Before := (if Pending_RX = 0 then 16#FFFF_0000# else T.Word (16#A1# + Received));
      Page (CI (C.Data_Command)) := Data_Before;
      D.Step (State); After := D.Observe (State);
      if Before.Hardware.At_Phase = N.Controller.Transferring and then
        After.Hardware.Port_Status.Access_Attempted and then
        After.Hardware.Port_Status.Register_ID = R.Read_Data_Command
      then
         Value := Page (CI (C.Data_Command));
         if Value /= Data_Before then
            Commands := Commands + 1;
            if (Value and T.Read_Command_Bit) /= 0 then Pending_RX := Pending_RX + 1; end if;
            if (Value and T.Stop_Command_Bit) /= 0 then
               Page (RI (R.Raw_Interrupts)) := Page (RI (R.Raw_Interrupts)) or R.Stop_Interrupt;
            end if;
            Check (Pending_RX <= 2, "actual native owner respects modeled FIFO credit");
         else
            Check (Pending_RX > 0, "native data read only with pending byte");
            Pending_RX := Pending_RX - 1; Received := Received + 1;
         end if;
      end if;
      if (Before.Hardware.At_Phase = N.Controller.Clear_Status and then
           After.Hardware.Owner_Access_Attempted) or else
        (Before.Hardware.At_Phase = N.Controller.Configuring and then
         Before.Hardware.Configuration.At_Stage = C.Clear_IRQ and then
         After.Hardware.Configuration.Access_Attempted)
      then Page (RI (R.Raw_Interrupts)) := 0; end if;
   end Tick;
   procedure Finish_Startup is
      Steps : Natural := 0;
   begin
      while D.Observe (State).Loop_Status.At_Phase = D.Starting loop
         Steps := Steps + 1; Check (Steps < 100, "native startup loop bounded"); Tick;
      end loop;
   end Finish_Startup;
   procedure Idle_No_Replay is
      Before : constant Page_Array := Page;
      Clocks : constant Natural := M.Calls;
      Replies : constant Natural := Client_Replies + Startup_Reports;
   begin
      D.Step (State); D.Step (State);
      Check (Page = Before and then M.Calls = Clocks and then
             Client_Replies + Startup_Reports = Replies, "idle/stopped pump never polls hardware or repeats reply");
   end Idle_No_Replay;
   procedure Stop_Case is
      Result : D.Stop_Result;
      Before : Page_Array;
      Clocks : Natural;
   begin
      if Which /= 12 then
         D.Submit (State, Client, Badge, 16#FFFF_3333_0000_0003#, Request, Submitted);
         Check (Submitted = D.Reply_Taken and then Input_Copies = 1, "one retained transfer before stop");
         declare Steps : Natural := 0; begin
            while D.Observe (State).Hardware.At_Phase /= N.Controller.Transferring loop
               Steps := Steps + 1; Check (Steps < 100 and then D.Needs_Step (State), "reach active controller");
               Tick;
            end loop;
         end;
         Check (D.Observe (State).Hardware.Controller_May_Be_Enabled, "stop requested with controller enabled");
      end if;
      Before := Page; Clocks := M.Calls;
      D.Request_Stop (State, Result);
      Check (Result = D.Stop_Started and then not D.Ready (State)
             and then Page = Before and then M.Calls = Clocks, "stop closes admission without MMIO/clock");
      Check (D.Observe (State).Loop_Status.At_Phase =
              (if Which = 12 then D.Stopped else D.Draining), "idle stop versus active drain");
      D.Request_Stop (State, Result);
      Check (Result = D.Stop_Already_Requested and then Page = Before and then M.Calls = Clocks,
             "duplicate stop cannot restart work");
      D.Submit (State, Client, Badge, 99, Request, Submitted);
      Check (Last_Client.Status = Q.Busy and then Input_Copies = (if Which = 12 then 0 else 1),
             "no new grant acquisition during drain or stopped state");
      if Which = 14 then M.Sample := Interfaces.Unsigned_64'Last; end if;
      declare Steps : Natural := 0; begin
         while D.Needs_Step (State) loop
            Steps := Steps + 1; Check (Steps < 200, "bounded transfer drain"); Tick;
         end loop;
      end;
      if Which = 14 then
         Check (not D.Stopped_Quiescent (State) and then
                D.Observe (State).Loop_Status.Stopped_Because = D.Client_Fault,
                "clock loss during drain is not reported as quiescent stop");
         Check (not Last_Client.Hardware_Succeeded and then Output_Copies = 0,
                "faulted drain copies no output");
      else
         Check (D.Stopped_Quiescent (State) and then
                D.Observe (State).Loop_Status.Stopped_Because = D.Requested_Stop,
                "successful stop has independent disabled-idle evidence");
         if Which = 13 then
            Check (Commands = 3 and then Received = 2 and then Output_Copies = 1
                   and then Last_Client.Hardware_Succeeded, "accepted transfer finishes once during drain");
         end if;
      end if;
      if Which /= 12 then
         Check (Last_Client_Reply = 16#FFFF_3333_0000_0003# and then Client_Replies = 3,
                "drain answers original saved reply once, without replacing it");
      end if;
      Idle_No_Replay;
   end Stop_Case;
begin
   M.Sample := (if Which = 4 then Interfaces.Unsigned_64'Last else 10);
   I.Metadata := (Current => True, Kind => CuBit.Handles.ABI.Device_Memory,
      Held => (True, True), Object => Physical, Parameter => 4096);
   Devices.Expected_Physical := Physical; Devices.Expected_Virtual := Interfaces.Unsigned_64 (Virtual);
   Page (Index (Natural (L.Offset (L.Reset_Control)))) := L.Release_Resets;
   Page (RI (R.Component_Type)) := R.Component_Signature;
   Page (RI (R.Component_Parameters)) := 16#10100#;
   Page (RI (R.Component_Version)) := C.Minimum_Hold_Version;
   if Which = 12 then
      D.Request_Stop (State, Stop_Result);
      Check (Stop_Result = D.Stop_Not_Ready and then I.Calls = 0 and then
             Devices.Calls = 0 and then M.Calls = 0, "cold stop refusal performs no work");
   end if;
   D.Submit (State, Client, Badge, 16#FFFF_1111_0000_0001#, Request, Submitted);
   Check (Submitted = D.Reply_Taken and then Last_Client.Status = Q.Busy and then Input_Copies = 0
          and then I.Calls = 0 and then Devices.Calls = 0, "cold client refused without grants/map/hardware");
   Idle_No_Replay;
   if Which = 1 then Facts.Device := 0; end if;
   if Which = 2 then I.Metadata := (Current => False); end if;
   Configure_Delivered := Which /= 3;
   D.Begin_Startup (State, 1, Facts, Virtual, Start_Result);
   Check (Start_Result = D.Start_Accepted, "configure owns one full-width saved authority");
   if Which = 12 then
      D.Request_Stop (State, Stop_Result);
      Check (Stop_Result = D.Stop_Not_Ready and then not D.Observe (State).Loop_Status.Stop_Requested
             and then M.Calls = 0, "startup stop refusal does not claim a stop or sample clock");
   end if;
   D.Begin_Startup (State, 1, Facts, Virtual, Start_Result);
   Check (Start_Result = D.Already_Started, "repeat configure has no reset/reply ownership");
   Finish_Startup;
   Check (Startup_Reports = 1 and then M.Calls = 0,
          "one generic startup report with zero GETTIME calls");
   if Which in 1 .. 3 then
      Check (not D.Ready (State) and then not D.Needs_Step (State) and then
             D.Observe (State).Loop_Status.At_Phase = D.Stopped, "startup failure/delivery keeps loop stopped");
      Check (D.Observe (State).Loop_Status.Stopped_Because =
               (if Which = 3 then D.Report_Delivery_Failed elsif Which <= 2 then D.Startup_Refused else D.Startup_Failed),
             "specific startup stop reason retained");
      Check (Last_Configure.Status = (if Which <= 2 then W.Refused elsif Which = 3 then W.Ready
                                    else W.Failed_Effects_Possible), "typed startup result retains effects/delivery distinction");
      D.Submit (State, Client, Badge, 3, Request, Submitted);
      Check (Last_Client.Status = Q.Busy and then Input_Copies = 0, "stopped loop cannot admit client grants");
      Idle_No_Replay;
   else
      Check (D.Ready (State) and then not D.Needs_Step (State), "delivered Ready activates idle service");
      Check (Page (CI (C.Target)) = 0 and then Page (CI (C.Fast_High)) = 0,
             "native startup never invents target/timing");
      Idle_No_Replay;
      if Which in 12 .. 14 then
         Stop_Case;
         Ada.Text_IO.Put_Line ("native loop case" & Which'Image & " checks" & Checks'Image);
         return;
      end if;
      if Which = 5 then Input_Outcome := Q.Uncertain; end if;
      if Which = 6 then Output_Outcome := Q.Uncertain; end if;
      if Which = 8 then Output_Outcome := Q.Refused; end if;
      if Which = 10 then Badge := B.Canonical ((B.Ten_Bit, 1023, 100_000)); end if;
      if Which = 11 then Request.Deadline := 10; end if;
      if Which = 7 then
         declare Before : constant Page_Array := Page; Clocks : constant Natural := M.Calls;
         begin
            Active_Second := True;
            D.Begin_Startup (Second_State, 1, Facts, Virtual, Start_Result);
            Active_Second := False;
            Check (Start_Result = D.Start_Accepted and then not D.Ready (Second_State)
                   and then Last_Configure.Status = W.Refused and then Page = Before and then M.Calls = Clocks,
                   "second loop state cannot borrow existing mapping admission");
            Check (D.Ready (State), "second initialization cannot poison original owner");
         end;
      end if;
      declare Caller : constant PID.Process_ID :=
          (if Which = 9 then PID.From_Word (16#FFFF_1111_0000_9999#) else Client);
      begin D.Submit (State, Caller, Badge, 16#FFFF_3333_0000_0003#, Request, Submitted); end;
      Check (Submitted = D.Reply_Taken, "parsed client request owns one reply");
      if Which in 4 | 5 | 9 | 10 | 11 then
         Check (Last_Client.Status = (case Which is when 5 => Q.Input_Uncertain,
            when 9 => Q.Unauthorized, when 10 => Q.Unsupported_Channel, when others => Q.Admission_Refused),
            "clean policy errors distinguished from uncertain grant copy");
         Check (Commands = 0 and then Output_Copies = 0, "refused client cannot address bus or output");
         Check (D.Ready (State) = (Which /= 5), "only terminal uncertainty poisons admission");
         Idle_No_Replay;
      else
         Check (D.Needs_Step (State) and then Input_Copies = 1, "accepted transfer requires driver steps");
         D.Submit (State, Client, Badge, 4, Request, Submitted);
         Check (Last_Client.Status = Q.Busy and then Input_Copies = 1, "overlap cannot replace retained client");
         declare Steps : Natural := 0;
         begin
            while D.Needs_Step (State) loop
               Steps := Steps + 1; Check (Steps < 200, "native client transfer bounded"); Tick;
            end loop;
         end;
         Check (Commands = 3 and then Received = 2 and then Output_Copies = 1,
                "actual native composition performs one write/two reads and one owned output copy");
         Check (Last_Client_Reply = 16#FFFF_3333_0000_0003# and then Last_Client.Hardware_Succeeded
                and then Last_Client.ID /= T.No_Ticket, "original saved client reply survives contention");
         Check (Last_Client.Status = (if Which = 6 then Q.Output_Uncertain elsif Which = 8 then Q.Output_Refused
                                      else Q.Completed), "typed final client delivery outcome");
         Check (D.Ready (State) = (Which /= 6), "uncertain output freezes entire instance");
         Idle_No_Replay;
      end if;
   end if;
   Ada.Text_IO.Put_Line ("native loop case" & Which'Image & " checks" & Checks'Image);
end Loop_Tests;
