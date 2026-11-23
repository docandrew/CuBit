pragma Ada_2022;
with DW_I2C_Owner; with DW_I2C_Registers;
with DW_I2C_Configuration; with Intel_LPSS_I2C_Reset; with Intel_LPSS_I2C_Profile;
package body I2C_HID_Channel_Model is
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
     (512, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write,
      True, Read_Input, Write_Output);
   use type O.Phase, O.Admission, O.Collection, O.Payload_Lifetime, O.Work_Count;
   use type I2C_Channel_Wire.Decode_Status, W.Word, W.Count, T.Ticket;
   Parameters : constant O.Startup :=
     (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096, 4096, 4096, 4096);
   Device : O.State;
   Queued, Refuse_Next, Refusal_Effects : Boolean := False;
   Request : I2C_Channel_Wire.Request;
   Saved_Token : W.Word := 0;
   Completed_Transactions : Natural := 0;
   procedure Initialize is
      Admission : O.Admission;
      Steps : Natural := 0;
   begin
      Reset_Model; O.Start (Device, Parameters, Admission);
      Check (Admission = O.Accepted, "startup admitted");
      while O.Observe (Device).At_Phase not in O.Available | O.Faulted loop
         Steps := Steps + 1; Check (Steps < 100, "bounded startup"); O.Advance (Device);
      end loop;
      Check (O.Observe (Device).At_Phase = O.Available and then not O.Observe (Device).Controller_May_Be_Enabled,
         "startup disabled and idle");
   end Initialize;
   procedure Submit (Header : I2C_Channel_Wire.Raw_Request; Token : W.Word; Accepted : out Boolean) is
      Raw : constant I2C_Channel_Wire.Decoding := I2C_Channel_Wire.Decode (Header, 512);
   begin
      Check (not Queued and then Token /= 0 and then Raw.Status = I2C_Channel_Wire.Decoded,
         "single copied metadata submission");
      Request := Raw.Value; Saved_Token := Token; Queued := True; Accepted := True;
   end Submit;
   procedure Serve (Now : W.Word; Input_Data : HID_I2C_Descriptors.Bytes;
      Token : out W.Word; Completion : out W.Completion; Available : out Boolean) is
      Admission : O.Admission;
      ID : T.Ticket;
      Result : O.Completion;
      Collected : O.Collection;
      Steps : O.Work_Count := 0;
   begin
      Available := False; Token := Saved_Token; Completion := (W.Invalid_Frame, 0, False, False, 0, 0);
      Check (Queued and then T.Tick (Now) >= Current_Time, "ordered deferred service");
      Check (Input_Data'Length <= Natural (Request.Read_Count), "bounded FIFO input");
      Queued := False; Current_Time := T.Tick (Now); FIFO_Data := [others => 0];
      if Input_Data'Length > 0 then FIFO_Data (0 .. Input_Data'Length - 1) := Input_Data; end if;
      Commands := 0; Cursor := 0; Command_Log := [others => 0];
      if Refuse_Next then
         Refuse_Next := False; Available := True;
         Completion := (W.Admission_Refused, 0, False, Refusal_Effects, 0, 0);
         Check (O.Observe (Device).At_Phase = O.Available and then not O.Observe (Device).Controller_May_Be_Enabled,
           "clean admission refusal performs no controller transfer");
         return;
      end if;
      O.Submit_Borrowed (Device, 16#15#, 400_000, T.Count (Request.Write_Count), T.Count (Request.Read_Count),
         T.Tick (Request.Limit), 4096, ID, Admission);
      Check (Admission = O.Accepted, "borrowed owner admitted");
      while not O.Observe (Device).Completion_Pending loop
         Steps := Steps + 1; Check (Steps <= 4096, "bounded request"); O.Advance (Device);
      end loop;
      O.Take_Borrowed_Completion (Device, Result, Collected);
      Check (Collected = O.Collected and then Result.ID = ID, "exact hardware ticket");
      if O.Payload_Status (Device) = O.Retain_For_Supervision then
         Check (not Result.Successful and then O.Observe (Device).At_Phase = O.Faulted,
            "failure retains physical owner, no recycling completion");
         return;
      end if;
      Check (O.Payload_Status (Device) = O.Returnable and then Result.Successful,
         "payload returnable before answer");
      Check (not O.Observe (Device).Controller_May_Be_Enabled and then O.Observe (Device).At_Phase = O.Available,
         "disabled idle owner before answer");
      Check (Commands = Natural (Request.Write_Count) + Natural (Request.Read_Count), "exact FIFO commands");
      Check ((Command_Log (Commands) and T.Stop_Command_Bit) /= 0, "STOP last");
      if Request.Write_Count > 0 and then Request.Read_Count > 0 then
         Check ((Command_Log (Natural (Request.Write_Count) + 1) and T.Restart_Command_Bit) /= 0,
            "combined read repeated START");
      end if;
      Completion := (W.Completed, W.Word (Result.ID), Result.Successful, Result.Effects_Possible,
         W.Count (Result.Write_Count), W.Count (Result.Read_Count));
      Available := True; Completed_Transactions := Completed_Transactions + 1;
   end Serve;
   procedure Inject_Admission_Refusal (Effects_Possible : Boolean := False) is
   begin Refuse_Next := True; Refusal_Effects := Effects_Possible; end Inject_Admission_Refusal;
   procedure Inject_Transfer_Abort is
   begin Inject_Abort := True; end Inject_Transfer_Abort;
   function Faulted return Boolean is (O.Observe (Device).At_Phase = O.Faulted);
   function Controller_May_Be_Enabled return Boolean is (O.Observe (Device).Controller_May_Be_Enabled);
   function Register_Accesses return Natural is (IO_Count);
   function Checks return Natural is (Check_Count);
   function Transactions return Natural is (Completed_Transactions);
end I2C_HID_Channel_Model;
