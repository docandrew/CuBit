pragma Ada_2022;
with Ada.Text_IO;
with HID_I2C_Bring_Up;
with HID_I2C_Descriptors;
procedure Bring_Up_Tests is
   package B renames HID_I2C_Bring_Up;
   package D renames HID_I2C_Descriptors;
   use type B.Tick;
   use type B.Reset_Ticket;
   use type B.Transport_Status;
   use type B.Failure;
   use type B.Stage;
   use type B.Stage_Count;
   use type D.Bytes;
   use type D.Byte;
   Checks : Natural := 0;
   Transfers, Arms, Waits, Releases, Clocks, Traces : Natural := 0;
   Failure_Transfer, Short_Transfer, Excess_Transfer : Natural := 0;
   Clock_Failure, Clock_Backward, Clock_Expiry : Natural := 0;
   Fail_Arm, Fail_Wait, Fail_Release, Zero_Ticket, Wrong_Ack, Bad_Header : Boolean := False;
   Armed, Pending, Released : Boolean := False;
   Expected_Status : B.Transport_Status := B.Bus_Error;
   Result : B.Report;
   Report_Data : D.Bytes (10 .. 19);
   Input_Data : D.Bytes (50 .. 65);
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
   procedure Clock (Now : out B.Tick; Available : out Boolean) is
   begin
      Clocks := Clocks + 1;
      Now := (if Clocks = Clock_Backward then 0 elsif Clocks = Clock_Expiry then 1000 else 100 + B.Tick (Clocks));
      Available := Clocks /= Clock_Failure;
   end Clock;
   procedure Transfer
     (Write_Data : D.Bytes; Read_Data : out D.Bytes; Deadline : B.Tick;
      Written, Read : out Natural; Status : out B.Transport_Status) is
      Header : constant D.Bytes :=
        [30,0,0,1,4,0,16#34#,16#12#,16#78#,16#56#,8,0,0,0,0,0,
         16#CD#,16#AB#,16#10#,16#10#,16#F3#,4,16#34#,16#12#,1,0,0,0,0,0];
      Payload : constant D.Bytes := [5,1,9,2];
   begin
      Transfers := Transfers + 1;
      Check (Deadline = 1000, "same absolute transfer deadline");
      Read_Data := [others => 16#EE#];
      Written := Write_Data'Length; Read := Read_Data'Length;
      Status := B.Transferred;
      case Transfers is
         when 1 =>
            Check (Write_Data = [16#20#,0] and Read_Data'Length = 30, "HID selector and exact descriptor read");
            for I in Header'Range loop Read_Data (Read_Data'First + I) := Header (I); end loop;
            if Bad_Header then Read_Data (Read_Data'First + 3) := 2; end if;
         when 2 =>
            Check (Write_Data = [16#CD#,16#AB#,0,8] and Read_Data'Length = 0, "power on before reset");
         when 3 =>
            Check (Write_Data = [16#CD#,16#AB#,0,1] and Read_Data'Length = 0, "reset command bytes");
            Check (Armed and Arms = 1, "IRQ generation armed before reset write");
            Pending := True; -- Interrupt arrives DURING the reset write.
         when 4 =>
            Check (Write_Data'Length = 0 and Read_Data'Length = 8, "bare input read at advertised size");
            Check (Waits = 1 and Pending, "latched early interrupt delivered before read");
            Read_Data := [others => 0];
            if Wrong_Ack then Read_Data (Read_Data'First) := 3; end if;
         when 5 =>
            Check (Write_Data = [16#34#,16#12#] and Read_Data'Length = 4, "report descriptor selector/length");
            Check (Released and Releases = 1, "reset completion cleaned before report descriptor");
            for I in Payload'Range loop Read_Data (Read_Data'First + I) := Payload (I); end loop;
         when others => raise Program_Error with "unexpected retry/extra transfer";
      end case;
      if Transfers = Failure_Transfer then Status := Expected_Status;
      elsif Transfers = Short_Transfer then
         if Read > 0 then Read := Read - 1; else Written := Written - 1; end if;
      elsif Transfers = Excess_Transfer then
         if Read > 0 then Read := Read + 1; else Written := Written + 1; end if;
      end if;
   end Transfer;
   procedure Arm (Deadline : B.Tick; Ticket : out B.Reset_Ticket; Status : out B.Transport_Status) is
   begin
      Arms := Arms + 1;
      Check (Deadline = 1000 and Transfers = 2, "arm after power and before reset");
      Ticket := (if Zero_Ticket then B.No_Reset_Ticket else 42);
      Armed := Ticket /= B.No_Reset_Ticket;
      Status := (if Fail_Arm then Expected_Status else B.Transferred);
   end Arm;
   procedure Await_IRQ (Ticket : B.Reset_Ticket; Deadline : B.Tick; Status : out B.Transport_Status) is
   begin
      Waits := Waits + 1;
      Check (Ticket = 42 and Deadline = 1000 and Armed and Pending, "exact generation and latched early IRQ");
      Status := (if Fail_Wait then Expected_Status else B.Transferred);
   end Await_IRQ;
   procedure Release_IRQ (Ticket : B.Reset_Ticket; Status : out B.Transport_Status) is
   begin
      Releases := Releases + 1;
      Check (Ticket = 42 and Armed and Releases = 1, "release once even after failure");
      Status := (if Fail_Release then Expected_Status else B.Transferred);
      if not Fail_Release then Armed := False; Released := True; end if;
   end Release_IRQ;
   procedure Trace (Item : B.Report; OK : Boolean) is
   begin
      Traces := Traces + 1;
      Check (Natural (B.Stage'Pos (Item.At_Stage)) + 1 = Traces, "one ordered event per attempted stage");
      Check ((Item.Reason = B.None) = OK, "typed trace success/refusal");
      if OK then
         Check (Natural (Item.Completed) = Traces, "trace detached completion count");
         if Item.At_Stage = B.Reset_Command then
            Check (Item.Reset_Still_Armed and Item.Ticket = 42, "trace reset arm fact");
         elsif Item.At_Stage = B.Reset_Completion then
            Check (not Item.Reset_Still_Armed, "trace reset release fact");
         elsif Item.At_Stage = B.Report_Descriptor then
            Check (Item.Descriptor_Retrieved, "trace descriptor acquisition fact");
         end if;
      end if;
   end Trace;
   procedure Run is new B.Run (Clock, Transfer, Arm, Await_IRQ, Release_IRQ, Trace);
   procedure Reset_Test is
   begin
      Transfers := 0; Arms := 0; Waits := 0; Releases := 0; Clocks := 0; Traces := 0;
      Failure_Transfer := 0; Short_Transfer := 0; Excess_Transfer := 0;
      Clock_Failure := 0; Clock_Backward := 0; Clock_Expiry := 0;
      Fail_Arm := False; Fail_Wait := False; Fail_Release := False;
      Zero_Ticket := False; Wrong_Ack := False; Bad_Header := False;
      Armed := False; Pending := False; Released := False;
      Expected_Status := B.Bus_Error;
   end Reset_Test;
   procedure Start is
   begin
      Run (16#20#, 1000, Report_Data, Input_Data, 0, Result);
      Check ((for all Byte of Input_Data => Byte = 0), "input scratch never forwarded");
      if not Result.Descriptor_Retrieved then
         Check ((for all Byte of Report_Data => Byte = 0), "partial descriptor discarded");
         Check (Result.Reason /= B.None and Traces = Natural (Result.Completed) + 1, "one failure event and prefix completion");
      end if;
   end Start;
begin
   Reset_Test; Start;
   Check (Result.Descriptor_Retrieved and Result.Reason = B.None and Result.Completed = 5, "transport acquisition succeeds");
   Check (Transfers = 5 and Arms = 1 and Waits = 1 and Releases = 1 and Traces = 5, "single sequential acquisition no retries");
   Check (Report_Data (10 .. 13) = [5,1,9,2] and (for all I in 14 .. 19 => Report_Data (I) = 0), "report prefix only");
   Check (not Result.Reset_Still_Armed, "reset generation released");
   for Status in B.Transport_Status loop
      if Status /= B.Transferred then
         for At_Transfer in 1 .. 5 loop
            Reset_Test; Failure_Transfer := At_Transfer; Expected_Status := Status; Start;
            Check (not Result.Descriptor_Retrieved and Result.Reason = B.Transport_Failed
              and Result.Transport = Status and Transfers = At_Transfer, "transport failure stops exact stage");
            Check (not Armed and not Result.Reset_Still_Armed, "failure releases generation");
         end loop;
         Reset_Test; Fail_Arm := True; Expected_Status := Status; Start;
         Check (Result.Reason = B.Transport_Failed and Transfers = 2 and Releases = 1, "failed arm with ticket still cleaned");
         Reset_Test; Fail_Wait := True; Expected_Status := Status; Start;
         Check (Result.Reason = B.Transport_Failed and Transfers = 3 and Releases = 1, "wait failure cleaned without input read");
         Reset_Test; Fail_Release := True; Expected_Status := Status; Start;
         Check (Result.Reason = B.Reset_Release_Failed and Transfers = 4
           and Releases = 1 and Result.Reset_Still_Armed and Result.Ticket = 42, "failed release retained for owner no retry");
      end if;
   end loop;
   for At_Transfer in 1 .. 5 loop
      Reset_Test; Short_Transfer := At_Transfer; Start;
      Check (Result.Reason = B.Incomplete_Transfer and Transfers = At_Transfer, "short completion not success");
      Reset_Test; Excess_Transfer := At_Transfer; Start;
      Check (Result.Reason = B.Bad_Completion_Count and Transfers = At_Transfer, "impossible completion count refused");
   end loop;
   for At_Clock in 1 .. 12 loop
      Reset_Test; Clock_Failure := At_Clock; Start;
      Check (Result.Reason = B.Clock_Unavailable and Clocks = At_Clock and not Armed, "clock unavailable stops and cleans");
      Reset_Test; Clock_Expiry := At_Clock; Start;
      Check (Result.Reason = B.Deadline_Expired and Clocks = At_Clock and not Armed, "one shared deadline includes post IO");
      if At_Clock > 1 then
         Reset_Test; Clock_Backward := At_Clock; Start;
         Check (Result.Reason = B.Clock_Reversed and Clocks = At_Clock and not Armed, "clock reversal refused");
      end if;
   end loop;
   Reset_Test; Wrong_Ack := True; Start;
   Check (Result.Reason = B.Reset_Not_Acknowledged and Transfers = 4 and Releases = 1, "input report not reset completion");
   Reset_Test; Zero_Ticket := True; Start;
   Check (Result.Reason = B.Invalid_Reset_Ticket and Transfers = 2 and Releases = 0, "zero ticket cannot admit reset");
   Reset_Test; Bad_Header := True; Start;
   Check (Result.Reason = B.Descriptor_Refused and Transfers = 1, "unsupported descriptor has no power side effect");
   Reset_Test;
   declare
      Empty : D.Bytes (1 .. 0);
      Scratch : D.Bytes (0 .. 7);
   begin
      Run (16#20#, 1000, Empty, Scratch, 0, Result);
      Check (Result.Reason = B.Capacity_Refused and Transfers = 1, "zero report capacity before power");
   end;
   Reset_Test;
   declare
      High_Report : D.Bytes (Natural'Last - 3 .. Natural'Last);
      High_Input : D.Bytes (Natural'Last - 7 .. Natural'Last);
   begin
      Run (16#20#, 1000, High_Report, High_Input, 0, Result);
      Check (Result.Descriptor_Retrieved and High_Report = [5,1,9,2], "high bounded arrays no intermediate overflow");
   end;
   Ada.Text_IO.Put_Line ("HID I2C bring up checks" & Checks'Image);
end Bring_Up_Tests;
