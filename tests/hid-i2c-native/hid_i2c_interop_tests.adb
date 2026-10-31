pragma Ada_2022;
with Ada.Text_IO;
with I2C_Channel_Badges;
with I2C_Transfer_Wire;
with I2C_Native_Results;
with HID_I2C_Buffered_Client;
with HID_I2C_Grant_Creation;
with HID_I2C_Descriptors;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with DW_I2C_Requests;
with DW_I2C_Request_Types;
with DW_I2C_Owner;
with DW_I2C_Transfer;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset;
with Intel_LPSS_I2C_Profile;
procedure HID_I2C_Interop_Tests is
   package T renames DW_I2C_Transfer;
   package R renames DW_I2C_Registers;
   package C renames DW_I2C_Configuration;
   package L renames Intel_LPSS_I2C_Reset;
   package P renames Intel_LPSS_I2C_Profile;
   use type T.Word;
   use type T.Count;
   use type T.Byte;
   use type C.Register_ID;
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
   package O is new DW_I2C_Owner
     (16, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write);
   use type O.Phase;
   use type O.Admission;
   Parameters : constant O.Startup :=
     (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096,
      30, 40, 100);
   procedure Startup (S : in out O.State) is
      Result : O.Admission;
      Iterations : Natural := 0;
   begin
      O.Start (S, Parameters, Result); Check (Result = O.Accepted, "owner startup accepted");
      while O.Observe (S).At_Phase not in O.Available | O.Faulted loop
         Iterations := Iterations + 1; Check (Iterations < 100, "startup progress");
         O.Advance (S);
      end loop;
      Check (O.Observe (S).At_Phase = O.Available, "actual composition reaches available");
      Check (Registers (Index (16#6C#)) = 0 and then not O.Observe (S).Controller_May_Be_Enabled,
             "available means disabled controller");
      Check (Registers (Index (16#1C#)) = 0 and then Registers (Index (16#20#)) = 0,
             "startup no target timing");
      Check (Registers (Index (16#7C#)) = 0 and then Registers (Index (16#04#)) = 0,
             "startup no invented target");
   end Startup;
   package Q renames DW_I2C_Request_Types;
   package W renames I2C_Transfer_Wire;
   package G renames CuBit.Grant_References;
   package PID renames CuBit.Process_IDs;
   package D renames HID_I2C_Descriptors;
   package BC renames I2C_Channel_Badges;
   use type G.Reference, PID.Process_ID, W.Word, W.Count, W.Decode_Status, W.Request, W.Completion_Status;
   use type Q.Copy_Result, Q.Reason;
   Caller : constant PID.Process_ID := PID.From_Word (16#F123_0000_4567_9876#);
   Server : constant PID.Process_ID := PID.From_Word (16#E234_0000_1234_5678#);
   Badge : constant W.Word := BC.Canonical ((BC.Seven_Bit, 16#15#, 400_000));
   Input_Address : constant W.Word := 16#1000_0000_0000#;
   Output_Address : constant W.Word := 16#2000_0000_0000#;
   type Page is array (Natural range 0 .. G.Page_Bytes - 1) of T.Byte;
   Input_Page, Output_Page : Page := [others => 0];
   Input_Reference : G.Reference := (24, 1);
   Output_Reference : G.Reference := (25, 1);
   Allocated, Released, Revokes, Replies, Submissions, Input_Copies, Output_Copies : Natural := 0;
   Retired : Boolean := True;
   Authority_OK, Reply_OK : Boolean := True;
   Input_Status, Output_Status : Q.Copy_Result := Q.Copied;
   Have_Response : Boolean := False;
   Response : Q.Response;
   Saved_Token : W.Word := 0;
   function Authorized (Who : PID.Process_ID; Tag : W.Word) return Boolean is
     (Authority_OK and then Who = Caller and then Tag = Badge);
   procedure Copy_Input (Who : PID.Process_ID; Grant : G.Reference; Data : out O.Bytes;
      Result : out Q.Copy_Result) is
   begin
      Check (Who = Caller and then Grant = Input_Reference, "exact full-width input owner/reference");
      Input_Copies := Input_Copies + 1; Data := [others => 0]; Result := Input_Status;
      if Result = Q.Copied then
         for I in Data'Range loop Data (I) := Input_Page (Natural (I - Data'First)); end loop;
      end if;
   end Copy_Input;
   procedure Copy_Output (Who : PID.Process_ID; Grant : G.Reference; Data : O.Bytes;
      Result : out Q.Copy_Result) is
   begin
      Check (Who = Caller and then Grant = Output_Reference, "same original full-width output owner/reference");
      Output_Copies := Output_Copies + 1; Result := Output_Status;
      if Result in Q.Copied | Q.Uncertain then
         for I in Data'Range loop Output_Page (Natural (I - Data'First)) := Data (I); end loop;
      end if;
   end Copy_Output;
   procedure Answer (Token : W.Word; Item : Q.Response; Delivered : out Boolean) is
   begin
      Check (not Have_Response and then Token = Saved_Token, "one retained reply, exact asynchronous token");
      Replies := Replies + 1; Have_Response := True; Response := Item; Delivered := Reply_OK;
      -- Queue only. Never call the peer back inside its Submit callback.
   end Answer;
   package Broker is new DW_I2C_Requests (O, 16, PID.Process_ID, G.Reference,
      W.Word, Authorized, Copy_Input, Copy_Output, Answer);
   procedure Allocate (Bytes : W.Word; Address : out W.Word; OK : out Boolean) is
   begin
      Check (Bytes = G.Page_Bytes, "whole owned page allocation");
      Allocated := Allocated + 1; OK := True;
      Address := (if Allocated = 1 then Input_Address else Output_Address);
      Check (Allocated <= 2, "buffers allocated once");
   end Allocate;
   procedure Release (Address, Bytes : W.Word; OK : out Boolean) is
   begin
      Check (Retired and then Bytes = G.Page_Bytes and then
        Address in Input_Address | Output_Address, "release only known own retired spans");
      Released := Released + 1; OK := True;
   end Release;
   procedure Clear (Address, Bytes : W.Word) is
   begin
      Check (Bytes = G.Page_Bytes, "bounded own-page clear");
      if Address = Input_Address then Input_Page := [others => 0];
      elsif Address = Output_Address then Output_Page := [others => 0];
      else raise Program_Error with "foreign clear"; end if;
   end Clear;
   procedure Store (Address : W.Word; Data : D.Bytes) is
   begin
      Check (Address = Input_Address, "client writes own input page");
      for I in Data'Range loop Input_Page (I - Data'First) := T.Byte (Data (I)); end loop;
   end Store;
   procedure Load (Address : W.Word; Data : out D.Bytes) is
   begin
      Check (Retired and then Address = Output_Address, "client reads only retired own output page");
      for I in Data'Range loop Data (I) := D.Byte (Output_Page (I - Data'First)); end loop;
   end Load;
   procedure Create (Address, Pages : W.Word; Writable : Boolean;
      Ref : out G.Reference; Status : out HID_I2C_Grant_Creation.Status) is
   begin
      Check (Pages = 1, "one explicit span, no reference arithmetic");
      if Address = Input_Address then
         Check (not Writable, "input loan is read-only"); Ref := Input_Reference;
      else
         Check (Address = Output_Address and then Writable, "output loan is writable"); Ref := Output_Reference;
      end if;
      Retired := False; Status := HID_I2C_Grant_Creation.Created;
   end Create;
   procedure Revoke (Ref : G.Reference; Accepted : out Boolean) is
   begin
      Check (Ref = Input_Reference or else Ref = Output_Reference, "revoke only exact issued grant");
      Revokes := Revokes + 1; Accepted := True;
      -- Accepted revoke is deliberately not retirement confirmation.
   end Revoke;
   function Retirement_Confirmed (Ref : G.Reference) return Boolean is
   begin
      Check (Ref = Input_Reference or else Ref = Output_Reference, "query exact issued grant");
      return Retired;
   end Retirement_Confirmed;
   procedure Run_Case (Which : Positive) is
      Device : O.State;
      Requests : Broker.State;
      Writes : constant Natural := (if Which in 3 | 12 then 0 elsif Which = 11 then 16
        elsif Which = 2 then 5 else 2);
      Reads : constant W.Count := (if Which in 2 | 11 then 0 elsif Which = 12 then 16
        elsif Which = 13 then 14
        elsif Which = 3 then 5 else 3);
      Data : D.Bytes (1 .. Writes);
      Steps : Natural := 0;
      procedure Submit (Item : W.Request; Token : W.Word; Accepted : out Boolean) is
         Encoded : constant W.Request_Words := W.Encode (Item);
         Decoded : constant W.Decoding := W.Decode
           (W.Transfer_Operation, W.Protocol_Version, Encoded, 16);
         Request : Broker.Request;
      begin
         Check (Decoded.Status = W.Decoded, "real client wire decoded by server");
         Check (Decoded.Value = Item, "all counts/deadline/generations round trip");
         Saved_Token := Token; Submissions := Submissions + 1;
         Request := (Decoded.Value.Input, Decoded.Value.Output,
           T.Count (Decoded.Value.Write_Count), T.Count (Decoded.Value.Read_Count),
           T.Tick (Decoded.Value.Limit.At_Millisecond), 80);
         Broker.Submit (Requests, Device, Caller, Badge, Token, Request);
         Accepted := True;
      end Submit;
      package Client is new HID_I2C_Buffered_Client
        (16, Allocate, Release, Clear, Store, Load, Create, Submit, Revoke, Retirement_Confirmed);
      use type Client.Setup_Result, Client.Start_Result, Client.Close_Result;
      use type Client.Transactions.Phase, Client.Transactions.Reason,
        Client.Transactions.Completion_Result;
      S : Client.State;
      Setup : Client.Setup_Result;
      Started : Client.Start_Result;
      Finished : Client.Transactions.Completion_Result;
      Closed : Client.Close_Result;
      Reply : W.Completion_Words;
      Result : D.Bytes (1 .. Natural (Reads));
      OK : Boolean;
      Token : constant W.Word := W.Word (Which);
      Success : constant Boolean := Which in 1 .. 3 | 11 .. 13;
   begin
      Reset_Model; Startup (Device);
      Allocated := 0; Released := 0; Revokes := 0; Replies := 0; Submissions := 0;
      Input_Copies := 0; Output_Copies := 0; Retired := True;
      Authority_OK := Which /= 6; Reply_OK := Which /= 7;
      Input_Status := (if Which = 5 then Q.Uncertain elsif Which = 10 then Q.Refused else Q.Copied);
      Output_Status := (if Which = 4 then Q.Refused elsif Which = 9 then Q.Uncertain else Q.Copied);
      Input_Reference.generation := G.Generation (Which + 10);
      Output_Reference.generation := G.Generation (Which + 20);
      Have_Response := False; Inject_Abort := Which = 8;
      for I in Data'Range loop Data (I) := D.Byte (16#10# + I); end loop;
      Client.Initialize (S, Server, Setup); Check (Setup = Client.Buffers_Ready, "client setup");
      Client.Start (S, Data, Reads, Token, 1, 100, True, Started);
      Check (Started = Client.Submitted and then Submissions = 1, "one asynchronous client submission");
      while Broker.Observe (Requests).Active loop
         Steps := Steps + 1; Check (Steps < 150, "bounded server progress");
         Broker.Advance (Requests, Device);
      end loop;
      Check (Have_Response and then Replies = 1, "one actual request-handler response");
      Check (Broker.Observe (Requests).Last_Reply_Delivered = Reply_OK, "saved reply delivery observation");
      Reply := W.Encode_Completion (I2C_Native_Results.To_Wire (Response));
      Client.Receive (S, Token + 100, Server, Client.Transactions.Delivered,
        W.Transfer_Reply_Operation, W.Protocol_Version, Reply, 1, True, Finished);
      Check (Finished = Client.Transactions.Unrelated and then Revokes = 0, "foreign token has no effect");
      Client.Receive (S, Token, Caller, Client.Transactions.Delivered,
        W.Transfer_Reply_Operation, W.Protocol_Version, Reply, 1, True, Finished);
      Check (Finished = Client.Transactions.Wrong_Sender and then Revokes = 0, "wrong full-width peer has no effect");
      Client.Receive (S, Token, Server,
        (if Reply_OK then Client.Transactions.Delivered else Client.Transactions.Transport_Error),
        W.Transfer_Reply_Operation, W.Protocol_Version, Reply, 1, True, Finished);
      Check (Finished = Client.Transactions.Accepted and then
        Client.Observe (S).At_Phase = Client.Transactions.Retiring, "response enters retirement");
      Check (Revokes = (if Writes = 0 or Reads = 0 then 1 else 2), "each used grant revoked once");
      Client.Read (S, Result, OK); Check (not OK, "no access before retirement");
      Client.Close (S, Closed); Check (Closed = Client.Still_Owned and then Released = 0, "no free before retirement");
      Client.Step (S, 1, True); Check (Client.Observe (S).At_Phase = Client.Transactions.Retiring,
        "successful revoke is insufficient to reuse pages");
      Retired := True; Client.Step (S, 1, True);
      Check (Client.Observe (S).At_Phase = Client.Transactions.Finished, "authoritative retirement completes");
      Check (Client.Succeeded (S) = Success, "actual server/client agree on success");
      Client.Read (S, Result, OK); Check (OK = (Success and then Reads > 0), "only successful read exposed");
      if OK then
         for I in Result'Range loop Check (Result (I) = D.Byte (16#A0# + I), "actual FIFO bytes reach client"); end loop;
      elsif not Success then
         Check ((for all B of Result => B = 0), "failure exposes no output payload");
      end if;
      if Success then
         Check (Response.Status = Q.Completed and then Response.Hardware_Succeeded,
           "successful hardware completion survives actual wire adapter");
         Check (Commands = Writes + Natural (Reads), "exact combined transfer commands");
         Check ((Command_Log (Commands) and T.Stop_Command_Bit) /= 0, "final command stops transfer");
         for I in Data'Range loop Check ((Command_Log (I) and 16#FF#) = T.Word (Data (I)), "exact client write bytes"); end loop;
      elsif Which in 5 | 6 | 10 then
         Check (Commands = 0 and then Output_Copies = 0, "refusal before hardware has no data effects");
         Check (Broker.Observe (Requests).Faulted = (Which = 5), "uncertain input quarantines server");
      end if;
      case Which is
         when 1 .. 3 | 11 .. 13 =>
            Check (Client.Observe (S).Outcome = Client.Transactions.Transfer_Complete,
              "successful typed completion reaches HID");
            Check (Registers (Index (16#04#)) = 16#15#, "target comes from admitted badge");
            if Writes > 0 and then Reads > 0 then
               Check ((Command_Log (Writes + 1) and T.Restart_Command_Bit) /= 0 and then
                 (Command_Log (Writes + 1) and T.Read_Command_Bit) /= 0,
                 "combined write/read uses repeated START");
            end if;
         when 4 | 9 =>
            Check (Response.Status = (if Which = 4 then Q.Output_Refused else Q.Output_Uncertain)
              and then Response.Hardware_Succeeded and then Response.Read_Count = 0,
              "successful hardware is distinct from failed output copy");
            Check (Client.Observe (S).Completion.Status =
              (if Which = 4 then W.Output_Refused else W.Output_Uncertain) and then
              Client.Observe (S).Outcome = Client.Transactions.Transfer_Refused,
              "typed output-copy failure survives actual adapter");
            Check (Broker.Observe (Requests).Faulted = (Which = 9), "uncertain output quarantines server");
         when 5 | 10 =>
            Check (Response.Status = (if Which = 5 then Q.Input_Uncertain else Q.Input_Refused),
              "exact input-copy failure reason");
            Check (Client.Observe (S).Completion.Status =
              (if Which = 5 then W.Input_Uncertain else W.Input_Refused) and then
              Client.Observe (S).Outcome = Client.Transactions.Transfer_Refused,
              "typed input-copy failure survives actual adapter");
         when 6 =>
            Check (Response.Status = Q.Unauthorized and then Input_Copies = 0,
              "authorization refusal precedes any client grant acquisition");
            Check (Client.Observe (S).Completion.Status = W.Unauthorized and then
              Client.Observe (S).Outcome = Client.Transactions.Transfer_Refused,
              "typed unauthorized completion reaches HID");
         when 7 =>
            Check (Response.Status = Q.Completed and then Response.Hardware_Succeeded,
              "lost reply does not erase hardware completion");
            Check (Client.Observe (S).Outcome = Client.Transactions.Transport_Failed and then
              not Client.Observe (S).Have_Completion, "lost delivery is not a trusted completion");
         when 8 =>
            Check (Response.Status = Q.Completed and then not Response.Hardware_Succeeded,
              "abort preserves request response semantics");
            Check (Client.Observe (S).Outcome = Client.Transactions.Inconsistent_Completion,
              "HID requires positive hardware success");
         when others => raise Program_Error;
      end case;
      if Which = 8 then
         Check (Broker.Observe (Requests).Faulted and then
           O.Observe (Device).At_Phase = O.Faulted and then
           O.Observe (Device).Controller_May_Be_Enabled,
           "hardware abort quarantines owner without claiming quiescence");
      else
         Check (not O.Observe (Device).Controller_May_Be_Enabled,
           "successful transfer or pre-hardware refusal leaves controller disabled");
      end if;
      declare Before : constant Natural := IO_Count; begin
         Broker.Advance (Requests, Device);
         Check (IO_Count = Before and then Replies = 1, "completed/faulted request is not replayed");
      end;
      Check (Submissions = 1 and then Replies = 1, "no replay after failure or delivery loss");
      Client.Acknowledge (S); Client.Close (S, Closed);
      Check (Closed = Client.Closed and then Released = 2, "both own allocations close after acknowledgement");
   end Run_Case;
begin
   for Which in 1 .. 13 loop Run_Case (Which); end loop;
   Ada.Text_IO.Put_Line ("HID I2C actual-handler interoperability checks" & Checks'Image);
end HID_I2C_Interop_Tests;
