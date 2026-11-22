pragma Ada_2022;
with Ada.Text_IO; with DW_I2C_Owner; with DW_I2C_Transfer; with DW_I2C_Interrupt_Policy;
with DW_I2C_Registers; with DW_I2C_Configuration; with Intel_LPSS_I2C_Reset; with Intel_LPSS_I2C_Profile;
procedure Tests is
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
   Hold_TX, Delay_RX, Hold_Stop : Boolean := False;
   Mask_Status, Mask_Read_Status : C.Access_Status := C.Access_OK;
   Wrong_Mask_Read, Lose_Clock_On_Mask_Read, Reverse_Clock_On_Mask_Read : Boolean := False;
   Lose_Clock_On_Mask : Boolean := False;
   Mask_Enables, Mask_Disables : Natural := 0;
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
      Hold_TX := False; Delay_RX := False; Hold_Stop := False;
      Mask_Status := C.Access_OK; Mask_Read_Status := C.Access_OK;
      Wrong_Mask_Read := False; Lose_Clock_On_Mask_Read := False; Reverse_Clock_On_Mask_Read := False; Lose_Clock_On_Mask := False;
      Mask_Enables := 0; Mask_Disables := 0;
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
      if ID = C.Interrupt_Mask then
         Value := (if Wrong_Mask_Read then 0 else Registers (Index (Natural (C.Offset (ID)))));
         Status := Mask_Read_Status;
         if Lose_Clock_On_Mask_Read then Clock_OK := False; end if;
         if Reverse_Clock_On_Mask_Read then Current_Time := 0; end if;
         return;
      end if;
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
      IO_Count := IO_Count + 1;
      if ID = C.Interrupt_Mask then
         Status := Mask_Status;
         if Value = 0 then Mask_Disables := Mask_Disables + 1; else Mask_Enables := Mask_Enables + 1; end if;
         if Status in C.Access_OK | C.Unknown_Effect then Registers (Index (Natural (C.Offset (ID)))) := Value; end if;
         if Lose_Clock_On_Mask then Clock_OK := False; end if;
         return;
      end if;
      Registers (Index (Natural (C.Offset (ID)))) := Value;
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
         when R.TX_Level => Value := (if Hold_TX then 2 else 0);
         when R.RX_Level => Value := (if Delay_RX then 0 else RX_Count);
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
      if not Hold_Stop and then (Value and T.Stop_Command_Bit) /= 0 then Raw_IRQ := Raw_IRQ or R.Stop_Interrupt; end if;
      if Inject_Abort then Raw_IRQ := Raw_IRQ or R.Abort_Interrupt; Abort_Source := 8; end if;
      if Lose_Clock_On_Data then Clock_OK := False; end if;
   end Data_Write;

   use type C.Access_Status, T.Tick;
   package IRQ renames DW_I2C_Interrupt_Policy;
   procedure Payload_Read (Position : T.Count; Value : out T.Byte; Accepted : out Boolean) is
   begin Check (Position in 1 .. 2, "bounded borrowed write"); Value := T.Byte (Position); Accepted := True; end Payload_Read;
   procedure Payload_Write (Position : T.Count; Value : T.Byte; Accepted : out Boolean) is
   begin Check (Position in 1 .. 3 and then Value = T.Byte (16#A0# + Position), "bounded exact borrowed read"); Accepted := True; end Payload_Write;
   package O is new DW_I2C_Owner (16, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write,
      Data_Read, Data_Write, True, Payload_Read, Payload_Write);
   use type O.Phase, O.Admission, O.Interrupt_Result, O.Payload_Lifetime, O.Collection, O.Failure, T.Ticket;
   procedure Scenario (Kind : Positive) is
      S : O.State;
      Admission : O.Admission;
      Result : O.Interrupt_Result;
      Item : O.Completion;
      Collection : O.Collection;
      Ticket : T.Ticket;
      Before, Transfers_Before : Natural;
      procedure Until_Ready is
      begin
         for Step in 1 .. 100 loop exit when O.Observe (S).At_Phase in O.Available | O.Faulted; O.Advance (S); end loop;
         Check (O.Observe (S).At_Phase = O.Available and not O.Observe (S).Interrupts_May_Be_Enabled, "startup masks before available");
      end Until_Ready;
      procedure Until_Wait is
      begin
         for Step in 1 .. 100 loop exit when O.Observe (S).Waiting_For_Hardware or O.Observe (S).At_Phase = O.Faulted; O.Advance (S); end loop;
         Check (O.Observe (S).At_Phase = O.Transferring and O.Observe (S).Waiting_For_Hardware,
            "actual No_Action waits for hardware");
         Check (O.Waiting_Deadline (S) = 1000, "same absolute deadline");
      end Until_Wait;
      procedure Retained is
      begin
         Check (O.Observe (S).At_Phase = O.Faulted and O.Payload_Status (S) = O.Retain_For_Supervision,
            "mask/clock fault retains actual payload");
         Check (not O.Observe (S).Waiting_For_Hardware, "fault cannot arm another wait");
         Before := IO_Count;
         O.Advance (S); O.Arm_Interrupt_Wait (S, Result); O.Advance (S);
         Check (IO_Count = Before, "no mask/FIFO retry after fault");
         O.Take_Borrowed_Completion (S, Item, Collection);
         Check (Collection = O.Collected and not Item.Successful and O.Payload_Status (S) = O.Retain_For_Supervision,
            "collecting failure does not release mapping");
      end Retained;
   begin
      Reset_Model;
      O.Start (S, (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096, 30, 40, 100), Admission);
      Check (Admission = O.Accepted, "startup admitted"); Until_Ready;
      if Kind in 14 .. 20 then
         if Kind = 14 then Mask_Read_Status := C.Denied;
         elsif Kind = 15 then Mask_Read_Status := C.Unknown_Effect;
         elsif Kind = 16 then Registers (Index (16#30#)) := 1;
         elsif Kind = 17 then Clock_OK := False;
         elsif Kind = 18 then Current_Time := 1000;
         elsif Kind = 19 then Lose_Clock_On_Mask_Read := True;
         else Reverse_Clock_On_Mask_Read := True;
         end if;
         Before := IO_Count; O.Prepare_Interrupt_Mode (S, 1000, Result);
         Check (Result = O.Refused and O.Observe (S).At_Phase = O.Faulted and
            not O.Observe (S).Interrupt_Mode_Prepared, "startup readback/clock refusal stays unavailable");
         Check (IO_Count = Before + (if Kind in 17 | 18 then 0 else 1), "one bounded startup readback");
         Check (O.Observe (S).Interrupts_May_Be_Enabled = (Kind <= 18), "startup source certainty follows actual readback");
         Check (O.Observe (S).Reason =
            (case Kind is when 14 => O.Register_Refused, when 15 => O.Register_Indeterminate,
             when 16 => O.Interrupt_Mask_Mismatch, when 18 => O.Deadline_Expired,
             when 20 => O.Clock_Reversed, when others => O.Clock_Unavailable), "exact startup IRQ refusal reason");
         Before := IO_Count; O.Prepare_Interrupt_Mode (S, 2000, Result); O.Advance (S);
         Check (IO_Count = Before and O.Payload_Status (S) = O.Not_Borrowed, "no startup retry or invented borrowed payload");
         O.Take_Borrowed_Completion (S, Item, Collection);
         Check (Collection = O.Not_Ready, "startup failure invents no request completion");
         return;
      end if;
      if Kind /= 21 then
         O.Prepare_Interrupt_Mode (S, 1000, Result);
         Check (Result = O.Quiet and O.Observe (S).Interrupt_Mode_Prepared, "startup sources verified before mode readiness");
         Before := IO_Count; O.Prepare_Interrupt_Mode (S, 1000, Result);
         Check (Result = O.Quiet and IO_Count = Before, "prepare once before first request");
      end if;
      Before := IO_Count; O.Arm_Interrupt_Wait (S, Result);
      Check (Result = O.Not_Waiting and IO_Count = Before, "idle arm has no IO");
      O.Submit_Borrowed (S, 16#15#, 400_000, 2, 3, 1000, 500, Ticket, Admission);
      Check (Admission = O.Accepted and Ticket /= T.No_Ticket, "borrowed request admitted");
      for Step in 1 .. 100 loop exit when O.Observe (S).At_Phase in O.Transferring | O.Faulted; O.Advance (S); end loop;
      Hold_TX := True; Delay_RX := True; Hold_Stop := True;
      Until_Wait; Before := IO_Count;
      if Kind = 2 then Mask_Status := C.Denied;
      elsif Kind = 3 then Mask_Status := C.Unknown_Effect;
      elsif Kind = 4 then Lose_Clock_On_Mask := True;
      elsif Kind = 5 then Clock_OK := False;
      elsif Kind = 9 then Mask_Read_Status := C.Denied;
      elsif Kind = 10 then Mask_Read_Status := C.Unknown_Effect;
      elsif Kind = 11 then Wrong_Mask_Read := True;
      elsif Kind = 12 then Lose_Clock_On_Mask_Read := True;
      end if;
      O.Arm_Interrupt_Wait (S, Result);
      if Kind = 21 then
         Check (Result = O.Not_Waiting and IO_Count = Before, "unprepared mode cannot enable controller IRQ sources");
         O.Reject_Interrupt_Wait (S); Check (O.Observe (S).Reason = O.Interrupt_Wait_Failed, "unprepared wait is retained");
         Retained; return;
      end if;
      if Kind in 2 .. 5 | 9 .. 12 then
         Check (Result = O.Refused and IO_Count = Before + (if Kind = 5 then 0 elsif Kind in 9 .. 12 then 2 else 1), "bounded failed arm");
         Check (O.Observe (S).Interrupts_May_Be_Enabled = (Kind in 3 | 4 | 9 .. 12), "uncertain enablement tracked");
         Check (O.Observe (S).Reason = (if Kind in 2 | 9 then O.Register_Refused elsif Kind in 3 | 10 then O.Register_Indeterminate elsif Kind = 11 then O.Interrupt_Mask_Mismatch else O.Clock_Unavailable), "precise arm failure");
         Retained; return;
      end if;
      Check (Result = O.Armed and O.Observe (S).Interrupts_May_Be_Enabled and IO_Count = Before + 2, "arm write plus readback");
      Check (Registers (Index (16#30#)) = (IRQ.Error_Sources or IRQ.TX_Empty or IRQ.Stop_Detected), "only useful TX wake armed");
      Before := IO_Count; O.Arm_Interrupt_Wait (S, Result);
      Check (Result = O.Already_Armed and IO_Count = Before, "duplicate arm does not write");
      if Kind in 22 .. 24 then
         if Kind = 23 then Mask_Status := C.Denied;
         elsif Kind = 24 then Lose_Clock_On_Mask := True; end if;
         Transfers_Before := Data_Calls; O.Reject_Interrupt_Wait (S);
         Check (Data_Calls = Transfers_Before and O.Observe (S).At_Phase = O.Faulted,
            "kernel wait failure masks without FIFO replay and retains");
         Check (O.Observe (S).Reason = (if Kind = 22 then O.Interrupt_Wait_Failed
            elsif Kind = 23 then O.Register_Refused else O.Clock_Unavailable), "wait failure preserves mask/clock cause");
         Check (O.Observe (S).Interrupts_May_Be_Enabled = (Kind /= 22), "wait refusal keeps uncertain IRQ sources");
         Retained; return;
      end if;
      if Kind = 6 then Mask_Status := C.Denied;
      elsif Kind = 7 then Mask_Status := C.Unknown_Effect;
      elsif Kind = 8 then Lose_Clock_On_Mask := True;
      elsif Kind = 13 then Lose_Clock_On_Mask_Read := True;
      end if;
      Transfers_Before := Data_Calls; Before := IO_Count; O.Advance (S);
      Check (Data_Calls = Transfers_Before and IO_Count = Before + (if Kind in 6 .. 8 then 1 else 2), "advance masks first without FIFO work");
      if Kind in 6 .. 8 | 13 then
         Check (O.Observe (S).Interrupts_May_Be_Enabled = (Kind /= 13), "uncertain disablement retained until readback");
         Retained; return;
      end if;
      Check (not O.Observe (S).Interrupts_May_Be_Enabled and Registers (Index (16#30#)) = 0, "mask confirmed before work");
      Hold_TX := False; Until_Wait;
      Check (Commands = 4 and RX_Count = 2, "RX credits exhausted after two read commands");
      O.Arm_Interrupt_Wait (S, Result); Check (Result = O.Armed, "arm RX progress");
      Check (Registers (Index (16#30#)) = (IRQ.Error_Sources or IRQ.RX_Full or IRQ.Stop_Detected), "TX-empty masked while RX credits full");
      Delay_RX := False; O.Advance (S); Until_Wait;
      Check (Commands = 5 and RX_Count = 0, "all exact commands and bytes consumed");
      O.Arm_Interrupt_Wait (S, Result); Check (Result = O.Armed, "arm final STOP");
      Check (Registers (Index (16#30#)) = (IRQ.Error_Sources or IRQ.Stop_Detected), "no pointless FIFO interrupts after data");
      Raw_IRQ := Raw_IRQ or R.Stop_Interrupt; O.Advance (S);
      for Step in 1 .. 100 loop exit when O.Observe (S).Completion_Pending; O.Advance (S); end loop;
      Check (O.Observe (S).Completion_Pending and not O.Observe (S).Interrupts_May_Be_Enabled,
         "quiet IRQ owner before completion");
      Check (O.Payload_Status (S) = O.Returnable, "confirmed quiescence permits mapping return");
      O.Take_Borrowed_Completion (S, Item, Collection);
      Check (Collection = O.Collected and Item.Successful and Item.ID = Ticket, "exact successful completion");
      Check ((Command_Log (1) and 16#FF#) = 1 and (Command_Log (2) and 16#FF#) = 2 and
         (Command_Log (5) and T.Stop_Command_Bit) /= 0, "exact payload and final STOP survive IRQ scheduling");
      Check (Mask_Enables = 3 and Commands = 5, "no duplicated masks or transfer commands");
      Before := IO_Count; O.Disarm_Interrupt_Wait (S, Result);
      Check (Result = O.Quiet and IO_Count = Before, "quiet disarm has no IO");
   end Scenario;
begin
   for Kind in 1 .. 24 loop Scenario (Kind); end loop;
   Ada.Text_IO.Put_Line ("I2C owner interrupt wait checks" & Checks'Image);
end Tests;
