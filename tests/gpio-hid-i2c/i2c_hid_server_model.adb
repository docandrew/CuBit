pragma Ada_2022;
with I2C_Channel_Badges;
with I2C_Native_Results;
with DW_I2C_Requests;
with DW_I2C_Request_Types;
with DW_I2C_Owner;
with DW_I2C_Transfer;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset;
with Intel_LPSS_I2C_Profile;
package body I2C_HID_Server_Model is
   package T renames DW_I2C_Transfer;
   package R renames DW_I2C_Registers;
   package C renames DW_I2C_Configuration;
   package L renames Intel_LPSS_I2C_Reset;
   package P renames Intel_LPSS_I2C_Profile;
   use type T.Word;
   use type T.Tick;
   use type T.Count;
   use type C.Register_ID;
   Check_Count : Natural := 0;
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Check_Count := Check_Count + 1;
   end Check;
   Registers : array (Natural range 0 .. 191) of T.Word := [others => 0];
   Raw_IRQ, Abort_Source, RX_Count, Cursor : T.Word := 0;
   IO_Count, Commands : Natural := 0;
   Inject_Abort : Boolean := False;
   Current_Time : T.Tick := 1;
   Command_Log : array (1 .. 1024) of T.Word := [others => 0];
   FIFO_Data : HID_I2C_Descriptors.Bytes (0 .. 511) := [others => 0];
   function Index (Offset : Natural) return Natural is (Offset / 4);
   procedure Reset_Model is
   begin
      Registers := [others => 0]; Registers (Index (16#204#)) := L.Release_Resets;
      Registers (Index (16#FC#)) := R.Component_Signature;
      Registers (Index (16#F4#)) := 16#10100#;
      Registers (Index (16#F8#)) := C.Minimum_Hold_Version;
      Raw_IRQ := 0; Abort_Source := 0; RX_Count := 0; Cursor := 0;
      IO_Count := 0; Commands := 0;
      Inject_Abort := False; Current_Time := 1; Command_Log := [others => 0];
   end Reset_Model;
   procedure Clock (Now : out T.Tick; Available : out Boolean) is
   begin Now := Current_Time; Available := True; end Clock;
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
         when C.TX_Level => Value := 0;
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
            RX_Count := RX_Count - 1; Cursor := Cursor + 1; Value := T.Word (FIFO_Data (Natural (Cursor - 1)));
         when others => Value := Registers (Index (Natural (R.Offset (ID))));
      end case;
   end Data_Read;
   procedure Data_Write (ID : R.Write_Register; Value : R.Word; Status : out R.Access_Status) is
      pragma Unreferenced (ID);
   begin
      IO_Count := IO_Count + 1;
      Status := R.Access_OK;
      Commands := Commands + 1; Command_Log (Commands) := Value;
      if (Value and T.Read_Command_Bit) /= 0 then
         RX_Count := RX_Count + 1; Check (RX_Count <= 2, "RX credit respected by composed owner");
      end if;
      if (Value and T.Stop_Command_Bit) /= 0 then Raw_IRQ := Raw_IRQ or R.Stop_Interrupt; end if;
      if Inject_Abort then Raw_IRQ := Raw_IRQ or R.Abort_Interrupt; Abort_Source := 8; end if;
   end Data_Write;
   package O is new DW_I2C_Owner
     (512, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write);
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
   package G renames CuBit.Grant_References;
   package PID renames CuBit.Process_IDs;
   package D renames HID_I2C_Descriptors;
   package BC renames I2C_Channel_Badges;
   use type G.Reference, PID.Process_ID, W.Word, W.Decode_Status, W.Request;
   use type Q.Copy_Result, Q.Reason;
   Client : constant PID.Process_ID := PID.From_Word (16#D123_4567_0000_9876#);
   Badge : constant W.Word := BC.Canonical ((BC.Seven_Bit, 16#15#, 400_000));
   In_Submit, Request_Queued, Have_Response : Boolean := False;
   Queued_Request : W.Request;
   Queued_Token, Saved_Token : W.Word := 0;
   Completed_Transactions : Natural := 0;
   Response : Q.Response;
   Captured_Input : D.Bytes (0 .. 511) := [others => 0];
   function Authorized (Who : PID.Process_ID; Tag : W.Word) return Boolean is
     (Who = Client and then Tag = Badge);
   procedure Copy_Input (Who : PID.Process_ID; Grant : G.Reference; Data : out O.Bytes;
      Result : out Q.Copy_Result) is
      Bytes : D.Bytes (0 .. Data'Length - 1);
      Copied : Boolean;
   begin
      Check (not In_Submit and then Who = Client, "copy input outside client submit with exact owner");
      Read_Input (Who, Grant, Bytes, Copied); Result := (if Copied then Q.Copied else Q.Refused);
      for Index in Data'Range loop Data (Index) := T.Byte (Bytes (Natural (Index - Data'First))); end loop;
      Captured_Input := [others => 0];
      if Bytes'Length > 0 then Captured_Input (0 .. Bytes'Length - 1) := Bytes; end if;
   end Copy_Input;
   procedure Copy_Output (Who : PID.Process_ID; Grant : G.Reference; Data : O.Bytes;
      Result : out Q.Copy_Result) is
      Bytes : D.Bytes (0 .. Data'Length - 1);
      Copied : Boolean;
   begin
      Check (not In_Submit and then Who = Client, "copy output outside client submit with original owner");
      for Index in Data'Range loop Bytes (Natural (Index - Data'First)) := D.Byte (Data (Index)); end loop;
      Write_Output (Who, Grant, Bytes, Copied); Result := (if Copied then Q.Copied else Q.Refused);
   end Copy_Output;
   procedure Answer (Token : W.Word; Item : Q.Response; Delivered : out Boolean) is
   begin
      Check (not In_Submit and then not Have_Response and then Token = Saved_Token, "queue actual saved I2C completion off client stack");
      Have_Response := True; Response := Item; Delivered := True;
   end Answer;
   package Broker is new DW_I2C_Requests (O, 512, PID.Process_ID, G.Reference,
      W.Word, Authorized, Copy_Input, Copy_Output, Answer);
   use type O.Work_Count;
   Device : O.State;
   Requests : Broker.State;
   procedure Initialize is
   begin Reset_Model; Startup (Device); end Initialize;
   procedure Submit (Item : W.Request; Token : W.Word; Accepted : out Boolean) is
   begin
      In_Submit := True;
      Check (not Request_Queued and then not Have_Response and then Token /= 0, "one queued I2C request");
      Queued_Request := Item; Queued_Token := Token; Request_Queued := True;
      Accepted := True; In_Submit := False;
   end Submit;
   procedure Serve (Now : W.Word; Input_Data : D.Bytes; Token : out W.Word; Completion : out W.Completion) is
      Encoded : constant W.Request_Words := W.Encode (Queued_Request);
      Decoded : constant W.Decoding := W.Decode (W.Transfer_Operation, W.Protocol_Version, Encoded, 512);
      Maximum_Steps : constant O.Work_Limit := 4096;
      Steps : O.Work_Count := 0;
      Request : Broker.Request;
   begin
      Check (Request_Queued and then not In_Submit and then T.Tick (Now) >= Current_Time, "deferred ordered I2C service turn");
      Check (Decoded.Status = W.Decoded and then Decoded.Value = Queued_Request, "actual request wire round trip");
      Current_Time := T.Tick (Now); Request_Queued := False;
      Check (Input_Data'Length <= Natural (Decoded.Value.Read_Count), "bounded fixture FIFO bytes");
      FIFO_Data := [others => 0];
      if Input_Data'Length > 0 then FIFO_Data (0 .. Input_Data'Length - 1) := Input_Data; end if;
      Commands := 0; Cursor := 0; Command_Log := [others => 0];
      Saved_Token := Queued_Token;
      Request := (Decoded.Value.Input, Decoded.Value.Output,
         T.Count (Decoded.Value.Write_Count), T.Count (Decoded.Value.Read_Count),
         T.Tick (Decoded.Value.Limit.At_Millisecond), Maximum_Steps);
      Broker.Submit (Requests, Device, Client, Badge, Saved_Token, Request);
      while Broker.Observe (Requests).Active loop
         Steps := Steps + 1; Check (Steps <= Maximum_Steps, "bounded composed I2C request");
         Broker.Advance (Requests, Device);
      end loop;
      Check (Have_Response and then Response.Status = Q.Completed, "actual request handler produced completion");
      if Inject_Abort then
         Check (not Response.Hardware_Succeeded and then Broker.Observe (Requests).Faulted
            and then O.Observe (Device).At_Phase = O.Faulted
            and then O.Observe (Device).Controller_May_Be_Enabled,
            "actual abort retains fault and possible enablement");
      else
      Check (Response.Hardware_Succeeded,
         "actual I2C owner reports successful hardware completion");
      Check (O.Observe (Device).At_Phase = O.Available and then not O.Observe (Device).Controller_May_Be_Enabled,
         "actual owner confirms controller disabled before completion");
      Check (Response.Write_Count = Request.Write_Count and then Response.Read_Count = Request.Read_Count,
         "actual transfer counts match admitted request");
      Check (Commands = Natural (Request.Write_Count) + Natural (Request.Read_Count), "exact controller command count");
      Check ((Command_Log (Commands) and T.Stop_Command_Bit) /= 0, "last command has STOP");
      for Index in 1 .. Natural (Request.Write_Count) loop
         Check ((Command_Log (Index) and 16#FF#) = T.Word (Captured_Input (Index - 1)), "exact HID write bytes reach hardware model");
      end loop;
      if Request.Write_Count > 0 and then Request.Read_Count > 0 then
         Check ((Command_Log (Natural (Request.Write_Count) + 1) and T.Restart_Command_Bit) /= 0,
            "combined transfer uses repeated START");
      end if;
      end if;
      Token := Saved_Token; Completion := I2C_Native_Results.To_Wire (Response);
      Have_Response := False; Completed_Transactions := Completed_Transactions + 1;
   end Serve;
   procedure Inject_Transfer_Abort is
   begin Inject_Abort := True; end Inject_Transfer_Abort;
   function Faulted return Boolean is (Broker.Observe (Requests).Faulted and then O.Observe (Device).At_Phase = O.Faulted);
   function Controller_May_Be_Enabled return Boolean is (O.Observe (Device).Controller_May_Be_Enabled);
   function Register_Accesses return Natural is (IO_Count);
   function Checks return Natural is (Check_Count);
   function Transactions return Natural is (Completed_Transactions);
end I2C_HID_Server_Model;
