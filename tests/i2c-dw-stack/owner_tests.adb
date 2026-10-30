pragma Ada_2022;
with Ada.Text_IO;
with DW_I2C_Owner;
with DW_I2C_Transfer;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset;
with Intel_LPSS_I2C_Profile;
procedure Owner_Tests is
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
      Inject_Abort := False; Current_Time := 1; Command_Log := [others => 0];
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
begin
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
end Owner_Tests;
