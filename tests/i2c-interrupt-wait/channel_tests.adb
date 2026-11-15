pragma Ada_2022;
with Ada.Text_IO;
with DW_I2C_Owner;
with I2C_Channel_Turn; with CuBit.Control_Events; with CuBit.Process_IDs;
with I2C_Channel_Controller; with I2C_Channel; with I2C_Channel_Wire;
with I2C_Channel_Badges; with Interfaces;
with DW_I2C_Transfer;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset;
with Intel_LPSS_I2C_Profile;
procedure Channel_Tests is
   package T renames DW_I2C_Transfer;
   package R renames DW_I2C_Registers;
   package C renames DW_I2C_Configuration;
   package L renames Intel_LPSS_I2C_Reset;
   package P renames Intel_LPSS_I2C_Profile;
   use type T.Word;
   use type T.Byte;
   use type T.Count;
   use type T.Ticket;
   use type C.Register_ID;
   use type C.Access_Status;
   use type T.Phase;
   use type R.Outcome;
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
   Data_TX_Held : Boolean := False;
   Unknown_Cleanup, Dirty_TX, Late_Abort : Boolean := False;
   Current_Time : T.Tick := 1;
   Clock_Calls : Natural := 0;
   Config_Attempts, Fail_Config_Call, Target_Writes : Natural := 0;
   Config_Fault : C.Access_Status := C.Access_OK;
   Post_Config_Clock_Failure : Boolean := False;
   procedure Before_Config (Status : out C.Access_Status) is
   begin
      Config_Attempts := Config_Attempts + 1; Status := C.Access_OK;
      if Config_Attempts = Fail_Config_Call then
         Status := Config_Fault;
         if Post_Config_Clock_Failure then Clock_OK := False; end if;
      end if;
   end Before_Config;
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
      Data_TX_Held := False; Inject_Abort := False; Current_Time := 1; Command_Log := [others => 0];
      Unknown_Cleanup := False; Dirty_TX := False; Late_Abort := False;
      Config_Attempts := 0; Fail_Config_Call := 0; Target_Writes := 0;
      Config_Fault := C.Access_OK; Post_Config_Clock_Failure := False; Clock_Calls := 0;
   end Reset_Model;
   procedure Clock (Now : out T.Tick; Available : out Boolean) is
   begin Clock_Calls := Clock_Calls + 1; Now := Current_Time; Available := Clock_OK; end Clock;
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
      IO_Count := IO_Count + 1; Value := 0; Before_Config (Status);
      if Status in C.Denied | C.Unavailable then return; end if;
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
      IO_Count := IO_Count + 1; Before_Config (Status);
      if Status in C.Denied | C.Unavailable then return; end if;
      if ID in C.Target | C.Standard_High | C.Standard_Low | C.Fast_High |
        C.Fast_Low | C.SDA_Hold | C.Control
      then
         Check (Registers (Index (16#6C#)) = 0 and then RX_Count = 0,
                "retarget/timing writes only while disabled with empty input");
      end if;
      if ID = C.Target then Target_Writes := Target_Writes + 1; end if;
      Registers (Index (Natural (C.Offset (ID)))) := Value;
      if ID = C.Enable and then Value = C.Enabled_Bit then Cursor := 0; end if;
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
         when R.TX_Level => Value := (if Data_TX_Held then 2 else 0);
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
      Commands := Commands + 1;
      if Commands <= Command_Log'Last then Command_Log (Commands) := Value; end if;
      if (Value and T.Read_Command_Bit) /= 0 then
         RX_Count := RX_Count + 1; Check (RX_Count <= 2, "RX credit respected by composed owner");
      end if;
      if (Value and T.Stop_Command_Bit) /= 0 then Raw_IRQ := Raw_IRQ or R.Stop_Interrupt; end if;
      if Inject_Abort then Raw_IRQ := Raw_IRQ or R.Abort_Interrupt; Abort_Source := 8; end if;
      if Lose_Clock_On_Data then Clock_OK := False; end if;
   end Data_Write;
   package O is new DW_I2C_Owner
     (16, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write);
   Payload_Input : O.Bytes (1 .. 16) := [others => 0];
   Payload_Output : O.Bytes (1 .. 16) := [others => 0];
   Input_Calls, Output_Calls : Natural := 0;
   Refuse_Input, Refuse_Output : Boolean := False;
   procedure Read_Payload (Index : T.Count; Value : out T.Byte; Accepted : out Boolean) is
   begin
      Input_Calls := Input_Calls + 1;
      Accepted := Index in Payload_Input'Range and then not Refuse_Input;
      Value := (if Accepted then Payload_Input (Index) else 0);
   end Read_Payload;
   procedure Write_Payload (Index : T.Count; Value : T.Byte; Accepted : out Boolean) is
   begin
      Output_Calls := Output_Calls + 1;
      Accepted := Index in Payload_Output'Range and then not Refuse_Output;
      if Accepted then Payload_Output (Index) := Value; end if;
   end Write_Payload;
   package E is new DW_I2C_Owner
     (16, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write,
      True, Read_Payload, Write_Payload);
   package Large_E is new DW_I2C_Owner
     (65_535, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write,
      True, Read_Payload, Write_Payload);
   package Denied_E is new DW_I2C_Owner
     (16, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write,
      Borrowed_Payload => True);
   use type Denied_E.Phase, Denied_E.Admission, Denied_E.Payload_Lifetime, Denied_E.Failure;
   use type E.Phase, E.Admission, E.Collection, E.Payload_Lifetime, E.Failure;
   use type O.Phase;
   use type O.Admission;
   use type O.Failure;
   use type O.Collection;
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
             "startup has no target-specific timing writes");
      Check (Registers (Index (16#7C#)) = 0 and then Registers (Index (16#04#)) = 0,
             "startup does not invent target or hold settings");
   end Startup;
   procedure Finish_Request (S : in out O.State) is
      Calls : Natural := 0;
   begin
      while not O.Observe (S).Completion_Pending loop
         Calls := Calls + 1; Check (Calls < 100, "request progress"); O.Advance (S);
      end loop;
   end Finish_Request;
   procedure Success_Cases is
      S : O.State;
      Request : O.Bytes (100 .. 101) := [16#12#, 16#34#];
      Empty : O.Bytes (1 .. 0);
      Small : O.Bytes (500 .. 501);
      Output : O.Bytes (600 .. 607);
      Result : O.Admission;
      Ticket, Second, Rejected : T.Ticket;
      Item : O.Completion;
      Required : T.Count;
      Collected : O.Collection;
      Before : Natural;
   begin
      Reset_Model; Startup (S);
      O.Submit (S, 16#15#, 400_000, Request, 4, 1000, 80, Ticket, Result);
      Check (Result = O.Accepted and then Ticket /= T.No_Ticket, "copied request admitted");
      Request := [others => 16#EE#];
      O.Submit (S, 16#15#, 400_000, Empty, 1, 1000, 80, Rejected, Result);
      Check (Result = O.Busy and then Rejected = T.No_Ticket, "active owner refuses overlap");
      Finish_Request (S);
      Check (O.Observe (S).At_Phase = O.Completion_Ready and then Raw_IRQ = 0
             and then Registers (Index (16#6C#)) = 0, "STOP acknowledged after disabled quiescence");
      Check (Command_Log (1) = 16#12# and then Command_Log (2) = 16#34#,
             "caller mutation cannot change copied write span");
      Check (Command_Log (3) = 16#500# and then Command_Log (6) = 16#300#,
             "composed transfer emits RESTART and STOP");
      Before := IO_Count;
      O.Take_Completion (S, Small, Item, Required, Collected);
      Check (Collected = O.Insufficient_Capacity and then Required = 4 and then Item.ID = Ticket,
             "short collection retains exact completion");
      Check ((for all B of Small => B = 0), "short collection zero output");
      O.Advance (S);
      Check (IO_Count = Before, "waiting for reply capacity cannot replay transfer");
      O.Take_Completion (S, Output, Item, Required, Collected);
      Check (Collected = O.Collected and then Item.Successful and then Item.ID = Ticket,
             "successful collection bound to owner ticket");
      Check (Output (600) = 16#A1# and then Output (603) = 16#A4#
             and then Output (604) = 0 and then Output (607) = 0, "read prefix and unused zeros");
      O.Take_Completion (S, Output, Item, Required, Collected);
      Check (Collected = O.Not_Ready and then (for all B of Output => B = 0), "completion consumed once");
      O.Submit (S, 16#15#, 400_000, Empty, 2, 1000, 80, Second, Result);
      Check (Result = O.Accepted and then Second > Ticket, "planner/port reused with fresh request identity");
      Finish_Request (S);
      O.Take_Completion (S, Output, Item, Required, Collected);
      Check (Collected = O.Collected and then Item.Successful and then Item.ID = Second,
             "second request not poisoned by stale STOP or access tickets");
      Check (Output (600) = 16#A1# and then Output (601) = 16#A2#, "second request has owned input");
      Before := IO_Count;
      O.Submit (S, 16#15#, 400_000, Empty, 0, 1000, 80, Rejected, Result);
      Check (Result = O.Empty_Transfer and then IO_Count = Before, "empty request no IO");
      O.Submit (S, 16#15#, 400_000, Request, 16, 1000, 80, Rejected, Result);
      Check (Result = O.Capacity_Refused and then IO_Count = Before, "combined byte quota before copying");
   end Success_Cases;
   procedure Fault_Case (Which : Positive) is
      S : O.State;
      Request : constant O.Bytes := [1 => 16#55#];
      Output : O.Bytes (1 .. 4);
      Result : O.Admission;
      Ticket, Rejected : T.Ticket;
      Item : O.Completion;
      Required : T.Count;
      Collected : O.Collection;
      Before : Natural;
   begin
      Reset_Model; Startup (S);
      case Which is
         when 1 => Fail_Command := 1; Fail_Status := R.Unknown_Effect;
         when 2 => Fail_Command := 1; Fail_Status := R.Denied;
         when 3 => Inject_Abort := True;
         when 4 => Lose_Clock_On_Data := True;
         when 6 => Unknown_Cleanup := True;
         when 7 => Dirty_TX := True;
         when 8 => Late_Abort := True;
         when others => null;
      end case;
      O.Submit (S, 16#15#, 400_000, Request, 2, 1000, (if Which = 5 then 1 else 80), Ticket, Result);
      Check (Result = O.Accepted, "fault request admitted"); Finish_Request (S);
      Check (O.Observe (S).At_Phase = O.Faulted, "fault cannot return controller to service");
      Before := IO_Count; Clock_OK := True;
      O.Advance (S);
      O.Take_Completion (S, Output, Item, Required, Collected);
      Check (Collected = O.Collected and then Item.ID = Ticket and then not Item.Successful
             and then (Item.Effects_Possible or else Which = 5) and then Required = 0,
             "failed completion preserves identity and possible controller effects");
      Check ((for all B of Output => B = 0), "failed completion hides partial input");
      O.Submit (S, 16#15#, 400_000, Request, 2, 1000, 80, Rejected, Result);
      Check (Result = O.Busy and then IO_Count = Before, "fault requires supervisor, no automatic retry");
      if Which = 4 then
         Check (O.Observe (S).Port_Status.Status = R.Clock_Unavailable,
                "post-access clock failure diagnostic retained");
      elsif Which = 5 then
         Check (Item.Reason = O.Budget_Exhausted, "request quota independent of clock progress");
      elsif Which = 6 then
         Check (Item.Reason = O.Register_Indeterminate
                and then O.Observe (S).Last_Owner_Status = C.Unknown_Effect
                and then O.Observe (S).Owner_Access_Attempted
                and then O.Observe (S).Failed_At = O.Disable_Controller,
                "unknown disable is retained for supervisor cleanup");
      elsif Which = 7 then
         Check (Item.Reason = O.Controller_Not_Quiescent
                and then O.Observe (S).Last_Owner_Register = C.TX_Level
                and then O.Observe (S).Last_Owner_Value = 1
                and then O.Observe (S).Failed_At = O.Verify_TX,
                "nonempty TX prevents planner retirement");
      elsif Which = 8 then
         Check (Item.Reason = O.Abort_During_Quiescence
                and then O.Observe (S).Cleanup_Abort_Value = 8
                and then O.Observe (S).Failed_At = O.Copy_Abort,
                "late abort cannot become a successful completion");
      end if;
      if Which >= 6 then
         Check (O.Observe (S).Engine_Phase = T.Complete,
                "cleanup failure preserves completed hardware progress without reuse");
      end if;
   end Fault_Case;
   procedure Channel_Cases is
      S : O.State;
      Request : constant O.Bytes := [1 => 16#67#];
      Output : O.Bytes (1 .. 1);
      Result : O.Admission;
      Ticket, Rejected : T.Ticket;
      Item : O.Completion;
      Required : T.Count;
      Collected : O.Collection;
      Before, Targets : Natural;
   begin
      Reset_Model; Startup (S); Check (Target_Writes = 0, "no startup target access");
      Before := IO_Count;
      O.Submit (S, 16#15#, 0, Request, 0, 1000, 100, Ticket, Result);
      Check (Result = O.Invalid_Timing and then Ticket = T.No_Ticket and then IO_Count = Before,
             "zero channel speed refused without side effects");
      O.Submit (S, 16#15#, 1_000_001, Request, 0, 1000, 100, Ticket, Result);
      Check (Result = O.Invalid_Timing and then Ticket = T.No_Ticket and then IO_Count = Before,
             "unsupported channel speed refused without side effects");
      for Channel in 1 .. 4 loop
         O.Submit (S, (if Channel mod 2 = 1 then 16#15# else 16#2A#),
                   (if Channel mod 2 = 1 then 400_000 else 100_000),
                   Request, 1, 1000, 100, Ticket, Result);
         Check (Result = O.Accepted, "alternating channel admitted");
         Before := IO_Count; Targets := Target_Writes;
         O.Submit (S, 16#33#, 10_000, Request, 1, 1000, 100, Rejected, Result);
         Check (Result = O.Busy and then Rejected = T.No_Ticket and then IO_Count = Before
                and then Target_Writes = Targets, "other channel cannot retarget pending setup");
         Finish_Request (S);
         Check (O.Observe (S).At_Phase = O.Completion_Ready, "alternating channel completes");
         Check (Registers (Index (16#04#)) = (if Channel mod 2 = 1 then 16#15# else 16#2A#),
                "exact admitted target installed on every transfer");
         Check (Registers (Index (16#00#)) =
                  (C.Master_Bit or C.Restart_Enable_Bit or C.Slave_Disable_Bit or
                   (if Channel mod 2 = 1 then C.Fast_Speed_Bits else C.Standard_Speed_Bits)),
                "mode follows channel speed, not startup or previous channel");
         Before := IO_Count;
         O.Submit (S, 16#33#, 10_000, Request, 1, 1000, 100, Rejected, Result);
         Check (Result = O.Busy and then IO_Count = Before,
                "retained completion prevents another channel configuration");
         O.Take_Completion (S, Output, Item, Required, Collected);
         Check (Collected = O.Collected and then Item.Successful and then Item.ID = Ticket,
                "channel completion retains identity");
      end loop;
      Check (Target_Writes = 4, "one target configuration per admitted request");
   end Channel_Cases;
   procedure Configuration_Faults is
      Request : constant O.Bytes := [1 => 16#55#];
      Calls : Natural;
      -- Obtain the exercised configuration+quiescence access count using the
      -- actual composition. Then fail each one, including destructive reads.
      S : O.State;
      Result : O.Admission;
      Ticket : T.Ticket;
   begin
      Reset_Model; Startup (S); Config_Attempts := 0;
      O.Submit (S, 16#15#, 400_000, Request, 0, 1000, 100, Ticket, Result);
      Check (Result = O.Accepted, "matrix baseline admitted"); Finish_Request (S);
      Check (O.Observe (S).At_Phase = O.Completion_Ready, "matrix baseline completes");
      Calls := Config_Attempts; Check (Calls > 20, "matrix includes setup and cleanup");
      for N in 1 .. Calls loop
         for Kind in 1 .. 3 loop
            declare
               F : O.State;
               Item : O.Completion;
               Output : O.Bytes (1 .. 1);
               Required : T.Count;
               Collected : O.Collection;
               Before : Natural;
            begin
               Reset_Model; Startup (F); Config_Attempts := 0; Fail_Config_Call := N;
               Config_Fault := (if Kind = 1 then C.Denied elsif Kind = 2 then C.Unknown_Effect else C.Access_OK);
               Post_Config_Clock_Failure := Kind = 3;
               O.Submit (F, 16#2A#, 100_000, Request, 0, 1000, 100, Ticket, Result);
               Check (Result = O.Accepted, "matrix request admitted"); Finish_Request (F);
               Check (O.Observe (F).At_Phase = O.Faulted, "every config access fault stops owner");
               Before := IO_Count; Clock_OK := True; Fail_Config_Call := 0;
               O.Advance (F); O.Take_Completion (F, Output, Item, Required, Collected);
               Check (Collected = O.Collected and then Item.ID = Ticket and then not Item.Successful
                      and then Required = 0, "matrix failure owns exact completion");
               O.Submit (F, 16#15#, 400_000, Request, 0, 1000, 100, Ticket, Result);
               Check (Result = O.Busy and then IO_Count = Before,
                      "faulted controller cannot retarget, retry or replay");
            end;
         end loop;
      end loop;
   end Configuration_Faults;
   procedure Maximum_Transaction is
      package Big is new DW_I2C_Owner
        (T.Limit'Last, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write);
      use type Big.Phase;
      use type Big.Admission;
      use type Big.Collection;
      S : Big.State;
      Input : constant Big.Startup :=
        (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096, 30, 40, 100);
      Empty : Big.Bytes (1 .. 0);
      Data : Big.Bytes (1 .. T.Count'Last);
      Result : Big.Admission;
      Ticket : T.Ticket;
      Item : Big.Completion;
      Required : T.Count;
      Collected : Big.Collection;
      Steps : Natural := 0;
      -- Exact data actions are twice the read count; allow named setup/cleanup
      -- headroom rather than silently capping a transaction at the byte limit.
      Maximum_Data_Actions : constant Natural := 2 * Natural (T.Count'Last);
      Setup_And_Cleanup_Headroom : constant Natural := 256;
   begin
      Reset_Model; Big.Start (S, Input, Result);
      Check (Result = Big.Accepted, "maximum-read startup");
      while Big.Observe (S).At_Phase not in Big.Available | Big.Faulted loop
         Steps := Steps + 1; Check (Steps < 100, "maximum-read startup bounded"); Big.Advance (S);
      end loop;
      Check (Big.Observe (S).At_Phase = Big.Available, "maximum-read controller ready");
      Big.Submit (S, 16#15#, 400_000, Empty, T.Count'Last, T.Tick'Last,
                  Big.Work_Limit (Maximum_Data_Actions + Setup_And_Cleanup_Headroom), Ticket, Result);
      Check (Result = Big.Accepted, "full-width read count admitted with independent work budget");
      Steps := 0;
      while not Big.Observe (S).Completion_Pending loop
         Steps := Steps + 1;
         Check (Steps <= Maximum_Data_Actions + Setup_And_Cleanup_Headroom, "maximum read progress bounded");
         Big.Advance (S);
      end loop;
      Big.Take_Completion (S, Data, Item, Required, Collected);
      Check (Collected = Big.Collected and then Item.Successful and then Item.ID = Ticket
             and then Required = T.Count'Last and then Commands = Natural (T.Count'Last),
             "65535-byte read completes instead of exhausting byte-sized work quota");
      for I in Data'Range loop
         Check (Data (I) = T.Byte ((16#A0# + T.Word (I)) and 16#FF#), "maximum read byte prefix retained");
      end loop;
   end Maximum_Transaction;
   procedure Borrowed_Cases is
      procedure Boot (S : in out E.State) is
         Result : E.Admission;
      begin
         Reset_Model; Input_Calls := 0; Output_Calls := 0;
         Refuse_Input := False; Refuse_Output := False;
         Payload_Input := [others => 0]; Payload_Output := [others => 0];
         E.Start (S, (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096,
                  30, 40, 100), Result);
         Check (Result = E.Accepted, "borrowed startup accepted");
         for Turn in 1 .. 100 loop
            exit when E.Observe (S).At_Phase in E.Available | E.Faulted;
            E.Advance (S);
         end loop;
         Check (E.Observe (S).At_Phase = E.Available, "borrowed startup quiet");
         Check (E.Payload_Status (S) = E.Not_Borrowed, "startup borrows no payload");
      end Boot;
      procedure Finish (S : in out E.State) is
      begin
         for Turn in 1 .. 100 loop
            exit when E.Observe (S).Completion_Pending;
            E.Advance (S);
         end loop;
         Check (E.Observe (S).Completion_Pending, "borrowed bounded finish");
      end Finish;
      S : E.State; ID : T.Ticket; Admission : E.Admission;
      Item : E.Completion; Collection : E.Collection;
      Empty : E.Bytes (1 .. 0);
      Required : T.Count;
      Before : Natural;
   begin
      Check (E.State'Size = Large_E.State'Size, "borrowed state size independent of 65535-byte capacity");
      Check (E.State'Size < O.State'Size, "borrowed state excludes both payload arrays");
      Boot (S);
      Before := IO_Count;
      E.Submit_Borrowed (S, 16#2C#, 400_000, 0, 0, 1000, 80, ID, Admission);
      Check (Admission = E.Empty_Transfer, "empty borrowed transfer rejected");
      E.Submit_Borrowed (S, 16#2C#, 400_000, 16, 1, 1000, 80, ID, Admission);
      Check (Admission = E.Capacity_Refused, "combined borrowed counts bounded");
      E.Submit_Borrowed (S, 16#2C#, 400_000, 1, 0, 1, 80, ID, Admission);
      Check (Admission = E.Expired, "borrowed expired deadline refused");
      E.Submit (S, 16#2C#, 400_000, Empty, 1, 1000, 80, ID, Admission);
      Check (Admission = E.Invalid_Resource, "buffered entry refused by borrowed owner");
      Check (IO_Count = Before and Input_Calls = 0 and Output_Calls = 0,
             "rejected metadata and wrong storage API do not touch payload or hardware");
      E.Submit_Borrowed (S, 16#2C#, 400_000, 2, 4, 1000, 80, ID, Admission);
      Check (Admission = E.Accepted, "borrowed transfer admitted");
      Check (Input_Calls = 0 and Output_Calls = 0, "admission neither copies nor clears payload");
      Check (E.Payload_Status (S) = E.In_Use, "active payload retained");
      Payload_Input (1) := 16#12#; Payload_Input (2) := 16#34#;
      Finish (S);
      Check (E.Payload_Status (S) = E.Returnable, "payload returnable only after quiet completion");
      Check (not E.Observe (S).Controller_May_Be_Enabled and RX_Count = 0, "borrowed completion controller quiet");
      Check (Input_Calls = 2 and Output_Calls = 4, "exactly one provider access per transferred byte");
      Check (Command_Log (1) = 16#12# and Command_Log (2) = 16#34#, "live payload read at FIFO service time");
      Check (Payload_Output (1) = 16#A1# and Payload_Output (4) = 16#A4#, "direct receive payload");
      E.Take_Completion (S, Empty, Item, Required, Collection);
      Check (Collection = E.Not_Ready and E.Observe (S).Completion_Pending,
             "buffered collector cannot consume borrowed completion");
      E.Take_Borrowed_Completion (S, Item, Collection);
      Check (Collection = E.Collected and Item.Successful and Item.ID = ID, "borrowed metadata completion");
      Check (E.Payload_Status (S) = E.Returnable and E.Observe (S).At_Phase = E.Available, "collected success remains quiet");
      Check (Input_Calls = 2 and Output_Calls = 4, "collection does not copy payload");
      E.Take_Borrowed_Completion (S, Item, Collection);
      Check (Collection = E.Not_Ready, "borrowed completion consumed once");
      for Scenario in 1 .. 5 loop
         declare F : E.State; begin
            Boot (F);
            E.Submit_Borrowed (F, 16#2C#, 400_000, 1, 1, 1000, 80, ID, Admission);
            Check (Admission = E.Accepted, "fault scenario admitted");
            case Scenario is
               when 1 => Refuse_Input := True;
               when 2 => Refuse_Output := True;
               when 3 => Dirty_TX := True;
               when 4 => Current_Time := 1000;
               when 5 => Unknown_Cleanup := True;
            end case;
            Finish (F);
            Check (E.Payload_Status (F) = E.Retain_For_Supervision, "failure never recycles channel mapping");
            if Scenario in 1 .. 2 then
               Check (E.Observe (F).Reason = E.Payload_Access_Failed, "typed payload provider failure");
            end if;
            if Scenario = 1 then Check (Commands = 0, "input refusal before command side effect"); end if;
            E.Take_Borrowed_Completion (F, Item, Collection);
            Check (Collection = E.Collected and not Item.Successful, "fault report available");
            Check (E.Payload_Status (F) = E.Retain_For_Supervision, "collecting error preserves retained lifetime");
            E.Submit_Borrowed (F, 16#2C#, 400_000, 1, 0, 2000, 80, ID, Admission);
            Check (Admission = E.Busy and ID = T.No_Ticket, "faulted borrowed owner cannot replay");
         end;
      end loop;
      declare
         F : Denied_E.State;
         Result : Denied_E.Admission;
      begin
         Reset_Model;
         Denied_E.Start (F, (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096,
                           30, 40, 100), Result);
         for Turn in 1 .. 100 loop
            exit when Denied_E.Observe (F).At_Phase in Denied_E.Available | Denied_E.Faulted;
            Denied_E.Advance (F);
         end loop;
         Check (Denied_E.Observe (F).At_Phase = Denied_E.Available, "default-provider startup");
         Denied_E.Submit_Borrowed (F, 16#2C#, 400_000, 1, 0, 1000, 80, ID, Result);
         Check (Result = Denied_E.Accepted, "default-provider metadata admission");
         for Turn in 1 .. 100 loop
            exit when Denied_E.Observe (F).Completion_Pending;
            Denied_E.Advance (F);
         end loop;
         Check (Denied_E.Observe (F).Reason = Denied_E.Payload_Access_Failed and Commands = 0,
                "missing provider denies before data command");
         Check (Denied_E.Payload_Status (F) = Denied_E.Retain_For_Supervision,
                "missing provider cannot release borrowed mapping");
      end;
   end Borrowed_Cases;

   procedure Composed_Channel_Case (Which : Positive) is
      package Bridge is new I2C_Channel_Controller
        (Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write);
      package Core renames Bridge.Controller;
      package RC renames I2C_Channel.RC;
      package W renames I2C_Channel_Wire;
      package B renames I2C_Channel_Badges;
      use type Bridge.Submission, Bridge.Releasing, Core.Phase, Core.Admission, Core.Collection;
      type Byte_Array is array (Natural range <>) of Interfaces.Unsigned_8 with Component_Size => 8;
      Guard : constant := RC.Page_Bytes;
      Client : aliased Byte_Array (0 .. (RC.Client_Pages + 2) * RC.Page_Bytes - 1) := [others => 0]
        with Alignment => 4096;
      Server : aliased Byte_Array (0 .. (RC.Server_Pages + 2) * RC.Page_Bytes - 1) := [others => 0]
        with Alignment => 4096;
      Client_Ring : RC.Client; Server_Ring : RC.Server;
      Input, Output : RC.Payload_View;
      Admission : Core.Admission; Result : Bridge.Submission; Releasing : Bridge.Releasing;
      Collection : Core.Collection; Item : Core.Completion; ID, Rejected : T.Ticket;
      Header : W.Raw_Request := W.Encode ((2, 4, 1000), W.Maximum_Payload);
      Badge : constant B.Word := B.Canonical ((B.Seven_Bit, 16#2C#, 400_000));
      Before : Natural;
   begin
      Reset_Model;
      Bridge.Start ((P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096,
                    30, 40, 100), Admission);
      Check (Admission = Core.Accepted, "channel composition startup");
      for Step in 1 .. 100 loop
         exit when Bridge.Observe.At_Phase in Core.Available | Core.Faulted;
         Bridge.Advance;
      end loop;
      Check (Bridge.Observe.At_Phase = Core.Available, "channel composition ready");
      RC.Attach_Client (Client_Ring, Client (Guard)'Address, Server (Guard)'Address);
      RC.Attach_Server (Server_Ring, Server (Guard)'Address, Client (Guard)'Address);
      Input := RC.Read_Payload (Server_Ring); Output := RC.Write_Payload (Server_Ring);
      Before := IO_Count;
      Bridge.Submit (0, Header, Input, Output, ID, Result, Admission);
      Check (Result = Bridge.Invalid_Channel and not Bridge.Has_Payload, "unauthorized-format badge not admitted");
      Bridge.Submit (B.Canonical ((B.Ten_Bit, 1023, 400_000)), Header, Input, Output, ID, Result, Admission);
      Check (Result = Bridge.Unsupported_Channel, "unsupported addressing mode refused");
      Bridge.Submit (B.Canonical ((B.Seven_Bit, 44, B.Hertz'Last)), Header, Input, Output, ID, Result, Admission);
      Check (Result = Bridge.Unsupported_Channel, "speed conversion bounded before narrowing");
      Header.Reserved_A := 1;
      Bridge.Submit (Badge, Header, Input, Output, ID, Result, Admission);
      Check (Result = Bridge.Invalid_Metadata, "channel metadata copied and validated");
      Header := W.Encode ((1, 0, 1), W.Maximum_Payload);
      Bridge.Submit (Badge, Header, Input, Output, ID, Result, Admission);
      Check (Result = Bridge.Owner_Refused and Admission = Core.Expired and not Bridge.Has_Payload,
             "expired submission rolls back views using actual quiet owner");
      Check (IO_Count = Before, "invalid channel/metadata/deadline never touches hardware");
      Header := W.Encode ((2, 4, 1000), W.Maximum_Payload);
      Bridge.Submit (Badge, Header, Input, Output, ID, Result, Admission);
      Check (Result = Bridge.Accepted and Bridge.Has_Payload and ID /= T.No_Ticket,
             "shared mapped transfer accepted");
      Bridge.Release (Releasing);
      Check (Releasing = Bridge.Busy and Bridge.Has_Payload, "actual active owner refuses release");
      Bridge.Submit (Badge, Header, Input, Output, Rejected, Result, Admission);
      Check (Result = Bridge.Busy and Rejected = T.No_Ticket, "no overlapping channel binding");
      Client (Guard + RC.Client_Payload_At) := 16#12#;
      Client (Guard + RC.Client_Payload_At + 1) := 16#34#;
      case Which is
         when 1 => null;
         when 2 => Current_Time := 1000;
         when 3 => Dirty_TX := True;
         when 4 => Unknown_Cleanup := True;
         when others => raise Program_Error;
      end case;
      for Step in 1 .. 150 loop
         exit when Bridge.Observe.Completion_Pending;
         Bridge.Advance;
      end loop;
      Check (Bridge.Observe.Completion_Pending, "bounded composed completion");
      Bridge.Take_Completion (Item, Collection);
      Check (Collection = Core.Collected and Item.ID = ID, "composed completion exact ticket");
      if Which = 1 then
         Check (Item.Successful and not Bridge.Observe.Controller_May_Be_Enabled,
                "composed success verified disabled");
         Check (Command_Log (1) = 16#12# and Command_Log (2) = 16#34#, "mapped input reaches actual FIFO owner");
         Check (Registers (Index (16#04#)) = 44, "target comes from admitted badge");
         Check (Server (Guard + RC.Server_Payload_At) = 16#A1# and
                Server (Guard + RC.Server_Payload_At + 3) = 16#A4#,
                "actual receive bytes reach persistent channel output");
         Bridge.Release (Releasing);
         Check (Releasing = Bridge.Released and not Bridge.Has_Payload, "quiet actual owner permits view release");
         Check (Server (Guard + RC.Server_Payload_At + 3) = 16#A4#, "release does not clear completed report");
      else
         Check (not Item.Successful, "composed failure reported");
         Bridge.Release (Releasing);
         Check (Releasing = Bridge.Retained and Bridge.Has_Payload,
                "collecting failure never releases actual owner/adapter lease");
         Bridge.Submit (Badge, Header, Input, Output, Rejected, Result, Admission);
         Check (Result = Bridge.Retained and Rejected = T.No_Ticket, "retained composition cannot recycle mapping");
      end if;
      for Offset in 0 .. Guard - 1 loop
         Check (Client (Offset) = 0 and Server (Offset) = 0, "composed leading guards");
         Check (Client (Client'Last - Offset) = 0 and Server (Server'Last - Offset) = 0, "composed trailing guards");
      end loop;
   end Composed_Channel_Case;

   procedure Channel_Turn_Case (Which : Positive) is
      package Bridge is new I2C_Channel_Controller
        (Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write);
      package Core renames Bridge.Controller;
      package RC renames I2C_Channel.RC;
      package W renames I2C_Channel_Wire;
      package Wire renames W.W;
      package B renames I2C_Channel_Badges;
      package Evt renames CuBit.Control_Events;
      package PID renames CuBit.Process_IDs;
      use type Core.Admission, Core.Phase, RC.Token, Wire.Completion_Decode_Status;
      use type Wire.Completion_Status;
      use type Wire.Word, Wire.Count;
      type Slot is range 1 .. 2;
      type Byte_Array is array (Natural range <>) of Interfaces.Unsigned_8 with Component_Size => 8;
      Guard : constant := RC.Page_Bytes;
      Client : aliased Byte_Array (0 .. (RC.Client_Pages + 2) * RC.Page_Bytes - 1) := [others => 0]
        with Alignment => 4096;
      Server : aliased Byte_Array (0 .. (RC.Server_Pages + 2) * RC.Page_Bytes - 1) := [others => 0]
        with Alignment => 4096;
      CR : RC.Client; SR : RC.Server;
      Active, Other_Active : Boolean := True;
      Completed, Closed, Served_Count : Natural := 0;
      Actor : constant PID.Process_ID := PID.From_Word (16#12_3456_7890#);
      procedure Next_Channel (Found : out Boolean; Index : out Slot) is
      begin Index := 1; Found := Active and then RC.Can_Take (SR); end Next_Channel;
      procedure Take (Index : Slot; Item : out RC.Queues.Submission; Got : out Boolean) is
         Rung : Boolean;
      begin Check (Index = 1, "turn takes selected slot"); RC.Take (SR, Item, Got, Rung); end Take;
      procedure Served (Index : Slot) is
      begin Check (Index = 1, "turn serves selected slot"); Served_Count := Served_Count + 1; end Served;
      function Badge_Of (Index : Slot) return Wire.Word is
         pragma Unreferenced (Index);
      begin return B.Canonical ((B.Seven_Bit, 44, 400_000)); end Badge_Of;
      function Input_Of (Index : Slot) return RC.Payload_View is
         pragma Unreferenced (Index);
      begin return RC.Read_Payload (SR); end Input_Of;
      function Output_Of (Index : Slot) return RC.Payload_View is
         pragma Unreferenced (Index);
      begin return RC.Write_Payload (SR); end Output_Of;
      procedure Complete
        (Index : Slot; Tag : RC.Token; Answer : Wire.Completion_Words; Published : out Boolean) is
         Rung : Boolean;
      begin
         Check (Index = 1 and Active, "turn completion bound to live slot");
         Check (not Bridge.Has_Payload and not Bridge.Observe.Controller_May_Be_Enabled,
                "completion only after actual quiet payload release");
         Published := Which /= 7;
         if Published then RC.Complete (SR, Tag, Answer, Rung); Completed := Completed + 1; end if;
      end Complete;
      procedure Find_Ending (Event : Evt.Event; Found : out Boolean; Index : out Slot) is
      begin
         Found := False; Index := 1;
         if Event.Kind in Evt.Grant_Revoked | Evt.Grant_Returned and then Event.Generation = 7 then
            if Event.Slot = 1 then Found := Active;
            elsif Event.Slot = 2 then Found := Other_Active; Index := 2;
            end if;
         end if;
      end Find_Ending;
      procedure Close_Ending (Event : Evt.Event; Expected : Slot; Was_Closed : out Boolean) is
         Found : Boolean; Index : Slot;
      begin
         Find_Ending (Event, Found, Index); Was_Closed := Found and then Index = Expected;
         if not Was_Closed then return; end if;
         if Expected = 1 then
            Check (not Bridge.Has_Payload and not Bridge.Observe.Controller_May_Be_Enabled,
                   "mapping return waits for actual quiet controller");
            Active := False; RC.Detach_Server (SR); RC.Detach_Client (CR);
         else Other_Active := False;
         end if;
         Closed := Closed + 1;
      end Close_Ending;
      package Loop_Turn is new I2C_Channel_Turn
        (Bridge, Slot, Next_Channel, Take, Served, Badge_Of, Input_Of, Output_Of,
         Complete, Find_Ending, Close_Ending);
      use type Loop_Turn.Phase, Evt.Event_Kind, Core.Interrupt_Result, T.Tick;
      IRQ_Result : Core.Interrupt_Result;
      Admission : Core.Admission; Submitted, Rung, Got : Boolean;
      Header : W.Raw_Request := W.Encode ((2, 4, 1000), W.Maximum_Payload);
      Answer : RC.Queues.Completion;
      Before : Natural;
      Ending : Evt.Event := (Evt.Grant_Revoked, 1, 7, Actor);
      Stop : constant Evt.Event := (Evt.Control, Evt.Stop, Actor);
   begin
      Reset_Model;
      Bridge.Start ((P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096,
                    30, 40, 100), Admission);
      Check (Admission = Core.Accepted, "turn controller start");
      for Step in 1 .. 100 loop exit when Bridge.Observe.At_Phase = Core.Available; Bridge.Advance; end loop;
      Check (Bridge.Observe.At_Phase = Core.Available, "turn controller ready");
      if Which in 8 .. 9 then
         Bridge.Prepare_Interrupt_Mode (1000, IRQ_Result);
         Check (IRQ_Result = Core.Quiet and Bridge.Observe.Interrupt_Mode_Prepared,
            "startup mask verified before first interrupt-mode request");
      end if;
      RC.Attach_Client (CR, Client (Guard)'Address, Server (Guard)'Address);
      RC.Attach_Server (SR, Server (Guard)'Address, Client (Guard)'Address);
      Client (Guard + RC.Client_Payload_At) := 16#12#;
      Client (Guard + RC.Client_Payload_At + 1) := 16#34#;
      if Which = 6 then Header.Reserved_A := 1; end if;
      RC.Submit (CR, 99, Header, Submitted, Rung); Check (Submitted, "turn request queued in actual ring");
      Before := IO_Count; Loop_Turn.Turn;
      Check (Served_Count = 1, "one request scheduling batch");
      if Which = 6 then
         Check (Loop_Turn.Observe.Rejected = 1 and Completed = 1 and IO_Count = Before,
                "invalid metadata rejected without IO");
         RC.Reap (CR, Answer, Got);
         Check (Got and Answer.Tag = 99, "rejection bound to actual ring tag");
         declare Item : constant Wire.Completion_Decoding := Wire.Decode_Completion
              (Wire.Transfer_Reply_Operation, Wire.Protocol_Version, Answer.Answer); begin
            Check (Item.Status = Wire.Completion_Decoded and then Item.Value.Status = Wire.Invalid_Frame,
                   "typed channel rejection wire reply");
         end;
         return;
      end if;
      Check (Loop_Turn.Observe.At_Phase = Loop_Turn.Running and Bridge.Has_Payload, "turn owns mapped transfer");
      Loop_Turn.On_Event (Actor, Ending); Loop_Turn.On_Event (Actor, Stop);
      Check (not Loop_Turn.Observe.End_Pending and not Loop_Turn.Observe.Stop_Requested and Closed = 0,
             "full-width peer identity cannot forge kernel lifecycle events");
      Ending.Generation := 6; Loop_Turn.On_Event (PID.No_Process, Ending);
      Check (not Loop_Turn.Observe.End_Pending and Closed = 0, "stale generation cannot close mapping");
      Ending.Generation := 7;
      if Which in 2 .. 4 then
         Loop_Turn.On_Event (PID.No_Process, Ending);
         Loop_Turn.On_Event (PID.No_Process, Ending);
         Check (Loop_Turn.Observe.End_Pending and Closed = 0, "matching end copied once and deferred while active");
      elsif Which = 1 then
         Ending.Slot := 2; Loop_Turn.On_Event (PID.No_Process, Ending);
         Check (Closed = 1 and Active and not Other_Active, "unborrowed channel may close independently");
      elsif Which = 5 then Loop_Turn.On_Event (PID.No_Process, Stop);
      end if;
      if Which = 3 then Current_Time := 1000;
      elsif Which = 4 then Unknown_Cleanup := True;
      end if;
      if Which in 8 .. 9 then
         Data_TX_Held := True; Loop_Turn.Turn;
         Check (Bridge.Observe.Waiting_For_Hardware and Bridge.Waiting_Deadline = 1000 and Completed = 0,
            "actual turn stops at hardware wait with same deadline");
         Bridge.Arm_Interrupt_Wait (IRQ_Result);
         Check (IRQ_Result = Core.Armed and Bridge.Observe.Interrupts_May_Be_Enabled,
            "facade arms only its retained owner");
         Before := IO_Count; Loop_Turn.Turn;
         Check (IO_Count - Before <= 8 and Commands = 0 and Bridge.Observe.Waiting_For_Hardware,
            "woken turn masks then samples once rather than spinning Work_Burst");
         Check (not Bridge.Observe.Interrupts_May_Be_Enabled and RC.Owed (SR) = 1 and Bridge.Has_Payload,
            "IRQ wake neither replies nor releases mapping early");
         if Which = 9 then Loop_Turn.On_Event (PID.No_Process, Stop); end if;
         Data_TX_Held := False;
      end if;
      Loop_Turn.Turn;
      if Which in 3 .. 4 then
         Check (Loop_Turn.Observe.At_Phase = Loop_Turn.Retained and Bridge.Has_Payload,
                "failed actual owner leaves turn retained");
         Check (Closed = 0 and Completed = 0 and RC.Owed (SR) = 1,
                "uncertain work returns no mapping or recycling completion");
         Before := IO_Count; Loop_Turn.Turn;
         Check (IO_Count = Before and Closed = 0, "retained turn never retries hardware or closes mapping");
      elsif Which = 2 then
         Check (Closed = 1 and Completed = 0 and not Active and not Bridge.Has_Payload,
                "deferred close after real quiescence drops owed answer");
         Check (Loop_Turn.Observe.At_Phase = Loop_Turn.Idle, "close retires transfer bookkeeping");
      elsif Which = 7 then
         Check (Loop_Turn.Observe.At_Phase = Loop_Turn.Retained and RC.Owed (SR) = 1,
                "publication refusal retained without replay");
      else
         Check (Completed = 1 and not Bridge.Has_Payload, "normal turn completes after release");
         RC.Reap (CR, Answer, Got); Check (Got and Answer.Tag = 99, "turn exact completion ring token");
         declare Item : constant Wire.Completion_Decoding := Wire.Decode_Completion
              (Wire.Transfer_Reply_Operation, Wire.Protocol_Version, Answer.Answer); begin
            Check (Item.Status = Wire.Completion_Decoded and then Item.Value.Hardware_Succeeded
                   and then Item.Value.Delivered_Reads = 4, "turn actual mapped read counts");
         end;
         Check (Loop_Turn.Observe.At_Phase = (if Which in 5 | 9 then Loop_Turn.Stopped else Loop_Turn.Idle),
                "authenticated stop drains active work before stopping");
      end if;
   end Channel_Turn_Case;

begin
   for Which in 1 .. 9 loop Channel_Turn_Case (Which); end loop;
   for Which in 1 .. 4 loop Composed_Channel_Case (Which); end loop;
   Borrowed_Cases;
   Maximum_Transaction;
   Channel_Cases; Configuration_Faults;
   Success_Cases;
   for Which in 1 .. 8 loop Fault_Case (Which); end loop;
   declare
      S : O.State;
      Input : O.Startup := Parameters;
      Result : O.Admission;
   begin
      Reset_Model;
      Input.Vendor := 0; O.Start (S, Input, Result);
      Check (Result = O.Invalid_Resource and then IO_Count = 0, "unsupported controller before hardware");
      Input := Parameters; Input.Window_Bytes := 767; O.Start (S, Input, Result);
      Check (Result = O.Invalid_Resource and then IO_Count = 0, "invalid BAR refused before hardware");
      Input := Parameters; Clock_OK := False; O.Start (S, Input, Result);
      Check (Result = O.Accepted and then IO_Count = 0 and then Clock_Calls = 0,
             "work-bounded startup admission does not need a clock");
      declare Steps : Natural := 0;
      begin
         while O.Observe (S).At_Phase not in O.Available | O.Faulted loop
            Steps := Steps + 1; Check (Steps < 100, "clockless startup work bound"); O.Advance (S);
         end loop;
         Check (O.Observe (S).At_Phase = O.Available and then Clock_Calls = 0 and then
                not O.Observe (S).Controller_May_Be_Enabled,
                "actual reset/probe/mask/idle succeeds with unavailable wall clock");
      end;
   end;
   Ada.Text_IO.Put_Line ("I2C composed owner checks" & Checks'Image);
end Channel_Tests;
