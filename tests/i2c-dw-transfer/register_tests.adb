pragma Ada_2022;
with Ada.Text_IO;
with DW_I2C_Registers; use DW_I2C_Registers;
procedure Register_Tests is
   use type Word;
   use type T.Tick;
   use type T.Count;
   use type T.Action_Kind;
   use type T.Phase;
   use type T.Access_Result;
   use type T.Admission;
   use type T.FIFO_Count;
   use type Byte;
   Error_Flags : constant array (1 .. 3) of Word :=
     [RX_Underflow_Interrupt, RX_Overflow_Interrupt, TX_Overflow_Interrupt];
   Checks : Natural := 0;
   Values : array (Read_Register) of Word := [others => 0];
   Reads, Writes, Clock_Calls : Natural := 0;
   Fail_Read_At, Fail_Clock_At, Expire_Clock_At, Reverse_Clock_At : Natural := 0;
   Write_Status, Read_Status : Access_Status := Access_OK;
   Write_Value : Word := 0;
   Read_Order : array (1 .. 8) of Read_Register := [others => Component_Type];
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   procedure Reset is
   begin
      Values := [others => 0];
      Values (Component_Type) := Component_Signature;
      Values (Component_Version) := 16#3131312A#;
      Reads := 0; Writes := 0; Clock_Calls := 0;
      Fail_Read_At := 0; Fail_Clock_At := 0;
      Expire_Clock_At := 0; Reverse_Clock_At := 0;
      Write_Status := Access_OK; Read_Status := Access_OK;
      Write_Value := 0;
   end Reset;
   procedure Clock (Now : out T.Tick; Available : out Boolean) is
   begin
      Clock_Calls := Clock_Calls + 1;
      Available := Clock_Calls /= Fail_Clock_At;
      Now := (if Clock_Calls = Expire_Clock_At then 10
              elsif Clock_Calls = Reverse_Clock_At then 0 else 1);
   end Clock;
   procedure Read (ID : Read_Register; Value : out Word; Status : out Access_Status) is
   begin
      Reads := Reads + 1;
      if Reads <= Read_Order'Last then Read_Order (Reads) := ID; end if;
      Value := Values (ID);
      Status := (if Reads = Fail_Read_At then Read_Status else Access_OK);
   end Read;
   procedure Write (ID : Write_Register; Value : Word; Status : out Access_Status) is
      pragma Unreferenced (ID);
   begin
      Writes := Writes + 1; Write_Value := Value; Status := Write_Status;
   end Write;
   package P is new Port (Clock, Read, Write);
   Report : Diagnostic;
   Caps : Capabilities;
   Snapshot : Sample;
   procedure Execute_Case (Which : Positive) is
      Owner : P.Access_Owner;
      Item : T.Action := (T.Send_Command, 1, 1, False, False, True);
      Value : Byte;
      Result : T.Access_Result;
   begin
      Reset;
      case Which is
         when 2 => Item.Read_Request := True; Item.Restart := True;
         when 3 => Item.Kind := T.Receive_Byte; Values (Read_Data_Command) := 16#ABC#;
         when 4 => Write_Status := Unknown_Effect;
         when 5 => Write_Status := Denied;
         when 6 => Fail_Clock_At := 1;
         when 7 => Expire_Clock_At := 2;
         when 8 => Reverse_Clock_At := 2;
         when 9 => Fail_Clock_At := 2;
         when others => null;
      end case;
      P.Execute (Owner, Item, 16#5A#, 10, Value, Result, Report);
      case Which is
         when 1 =>
            Check (Result = T.Succeeded and then Write_Value = 16#25A#, "write STOP command");
         when 2 =>
            Check (Result = T.Succeeded and then Write_Value = 16#700#, "read RESTART STOP command");
         when 3 =>
            Check (Result = T.Succeeded and then Value = 16#BC# and then Reads = 1,
                   "receive masks first-data metadata");
         when 4 =>
            Check (Result = T.Indeterminate and then Report.Status = Register_Indeterminate,
                   "uncertain write preserved");
         when 5 =>
            Check (Result = T.Refused and then Report.Status = Register_Refused, "denied write");
         when 6 =>
            Check (Result = T.Refused and then Writes = 0 and then Report.Status = Clock_Unavailable,
                   "no access without clock");
         when 7 =>
            Check (Result = T.Succeeded and then Writes = 1 and then Report.Status = Deadline_Expired,
                   "late successful write must be accounted once");
         when 8 =>
            Check (Result = T.Succeeded and then Report.Status = Clock_Reversed,
                   "reversed post-access clock preserves effect");
         when 9 =>
            Check (Result = T.Succeeded and then Report.Status = Clock_Unavailable,
                   "lost post-access clock preserves effect");
         when others => raise Program_Error;
      end case;
      declare
         Before : constant Natural := Reads + Writes;
      begin
         P.Execute (Owner, Item, 16#5A#, 10, Value, Result, Report);
         Check (Result = T.Refused and then Report.Status = Stale_Action
                and then Reads + Writes = Before and then not Report.Access_Attempted,
                "duplicate ticket never touches FIFO");
      end;
   end Execute_Case;
   procedure Integrated is
      S : T.State;
      Owner : P.Access_Owner;
      A : T.Action;
      Admitted : T.Admission;
      Result : T.Access_Result;
      Value : Byte;
      Matched : Boolean;
      Steps : Natural := 0;
   begin
      Reset;
      T.Submit (S, 2, 4, 6, 2, 2, 1, 10, Admitted);
      Check (Admitted = T.Accepted, "integrated submit");
      while T.Current_Phase (S) = T.Active loop
         Steps := Steps + 1;
         Check (Steps < 20, "integrated progress");
         Values (Raw_Interrupts) := (if T.Sent (S) = 6 then Stop_Interrupt else 0);
         Values (RX_Level) := Word (T.Outstanding_Reads (S));
         Values (Read_Data_Command) := 16#5A#;
         P.Capture (2, 2, 10, Snapshot, Report);
         Check (Snapshot.Ready and then Report.Status = Success, "integrated sample");
         T.Poll (S, 1, Snapshot.Hardware, A);
         if A.Kind /= T.No_Action then
            P.Execute (Owner, A, 16#AA#, 10, Value, Result, Report);
            T.Finish_Access (S, A.ID, Result, Matched);
            Check (Matched and then Result = T.Succeeded, "integrated one action");
            if A.Kind = T.Receive_Byte then Check (Value = 16#5A#, "integrated receive data"); end if;
         end if;
      end loop;
      Check (T.Current_Phase (S) = T.Complete and then T.Sent (S) = 6
             and then T.Received (S) = 4 and then Writes = 6, "integrated complete");
   end Integrated;
begin
   for TX in Word range 0 .. 255 loop
      for RX in Word range 0 .. 255 loop
         Reset;
         Values (Component_Parameters) := TX * 16#10000# + RX * 16#100#;
         P.Probe (10, Caps, Report);
         Check (Caps.Ready and then Report.Status = Success, "probe ready");
         Check (Caps.TX_Depth = T.FIFO_Depth (TX + 1)
                and then Caps.RX_Depth = T.FIFO_Depth (RX + 1), "all FIFO field encodings");
         Check (Caps.Version = Values (Component_Version) and then Reads = 3 and then Writes = 0,
                "probe reads only three nondestructive registers");
      end loop;
   end loop;
   Reset;
   Values (Component_Type) := 0;
   P.Probe (10, Caps, Report);
   Check (not Caps.Ready and then Report.Status = Wrong_Component and then Reads = 1,
          "unknown component refused");
   for Call in 1 .. 6 loop
      Reset; Fail_Clock_At := Call;
      P.Probe (10, Caps, Report);
      Check (not Caps.Ready and then Report.Status = Clock_Unavailable, "probe clock loss");
      Reset; Expire_Clock_At := Call;
      P.Probe (10, Caps, Report);
      Check (not Caps.Ready and then Report.Status = Deadline_Expired, "probe absolute deadline");
   end loop;
   for Call in 1 .. 3 loop
      for Status in Access_Status range Unavailable .. Unknown_Effect loop
         Reset; Fail_Read_At := Call; Read_Status := Status;
         P.Probe (10, Caps, Report);
         Check (not Caps.Ready and then Reads = Call and then Report.Bus_Status = Status,
                "probe stops on refused register");
      end loop;
   end loop;
   for TX in Word range 0 .. 4 loop
      for RX in Word range 0 .. 4 loop
         Reset; Values (TX_Level) := TX; Values (RX_Level) := RX;
         Values (Raw_Interrupts) := Stop_Interrupt;
         P.Capture (4, 4, 10, Snapshot, Report);
         Check (Snapshot.Ready and then Snapshot.Hardware.TX_Free = T.FIFO_Count (4 - TX)
                and then Snapshot.Hardware.RX_Available = T.FIFO_Count (RX)
                and then Snapshot.Hardware.Stop_Detected, "FIFO occupancy conversion");
         Check (Reads = 3 and then Read_Order (1) = Raw_Interrupts
                and then Read_Order (2) = TX_Level and then Read_Order (3) = RX_Level,
                "snapshot avoids destructive registers");
      end loop;
   end loop;
   Reset; Values (Raw_Interrupts) := Abort_Interrupt or Stop_Interrupt;
   Values (Abort_Source) := 16#1234#;
   P.Capture (4, 4, 10, Snapshot, Report);
   Check (Snapshot.Ready and then Snapshot.Hardware.Aborted
          and then Snapshot.Abort_Value = 16#1234# and then Reads = 2
          and then Read_Order (2) = Abort_Source, "abort source copied before any clear");
   Reset; Values (TX_Level) := Word'Last;
   P.Capture (4, 4, 10, Snapshot, Report);
   Check (not Snapshot.Ready and then Report.Status = Invalid_FIFO_Level and then Reads = 2,
          "bad TX count rejected before narrowing");
   Reset; Values (RX_Level) := Word'Last;
   P.Capture (4, 4, 10, Snapshot, Report);
   Check (not Snapshot.Ready and then Report.Status = Invalid_FIFO_Level,
          "bad RX count rejected before narrowing");
   for Flag of Error_Flags loop
      Reset; Values (Raw_Interrupts) := Flag;
      P.Capture (4, 4, 10, Snapshot, Report);
      Check (not Snapshot.Ready and then Report.Status = FIFO_Error and then Reads = 1,
             "overflow cannot appear successful");
   end loop;
   for Which in 1 .. 9 loop Execute_Case (Which); end loop;
   Integrated;
   Ada.Text_IO.Put_Line ("DesignWare register checks" & Checks'Image);
end Register_Tests;
