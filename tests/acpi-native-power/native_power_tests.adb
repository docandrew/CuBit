pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Native_Power_Detection;
with Power_Test_Log;
with Firmware_Tables;
with Interfaces; use Interfaces;
with ACPI_FADT.Sleep_Registers;
procedure Native_Power_Tests is
   package C renames ACPI_Service;
   package Q renames ACPI_Requests;
   package P renames ACPI_Native_Power_Detection;
   package IO renames Ada.Streams.Stream_IO;
   package F renames ACPI_FADT;
   package Sleep renames F.Sleep_Registers;
   use type Sleep.Source_Status, Sleep.Selection_Status, Sleep.Register_Model;
   use type Ada.Streams.Stream_Element_Offset, Q.Outcome, Q.Phase;
   use type Q.Response_Words;
   use type P.Outcome, P.Report, P.Lid_State, P.Source_State;
   Checks : Natural := 0;
   Mode : constant String := Ada.Command_Line.Argument (2);
   function Is_FADT_Case return Boolean is
     (Mode'Length >= 4 and then Mode (Mode'First .. Mode'First + 3) = "fadt");
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      Check (IO.Size (File) in 36 .. 4096, "bounded fixture");
      declare
         Bytes : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Bytes'Length);
      begin
         IO.Read (File, Bytes, Last); IO.Close (File); Check (Last = Bytes'Last, "whole fixture");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   FADT_Length : constant := 276;
   Server : Q.State (3, Table'Length + 2 * FADT_Length, Natural'Max (Table'Length, FADT_Length), 3, 0);
   Foreign_Server : Q.State (1, Table'Length, Table'Length, 1, 0);
   function Fixed_Table return Firmware_Tables.Bytes is
      Header_Length_Offset : constant := 4;
      Revision_Offset : constant := 8;
      Checksum_Offset : constant := 9;
      A_Control_Offset : constant := 64;
      B_Control_Offset : constant := 68;
      Control_Length_Offset : constant := 89;
      Flags_Offset : constant := 112;
      Sleep_Control_Offset : constant := 244;
      Data : Firmware_Tables.Bytes (1 .. (if Mode = "fadt-short" then 100 else FADT_Length)) := [others => 0];
      Sum : Firmware_Tables.Byte := 0;
      procedure Word (Offset : Natural; Value : Unsigned_64; Count : Positive) is
      begin
         for I in 0 .. Count - 1 loop
            Data (Offset + I + 1) := Firmware_Tables.Byte (Shift_Right (Value, 8 * I) and 255);
         end loop;
      end Word;
   begin
      Data (1 .. 4) := [Character'Pos ('F'), Character'Pos ('A'), Character'Pos ('C'), Character'Pos ('P')];
      Word (Header_Length_Offset, Data'Length, 4); Data (Revision_Offset + 1) := 6;
      if Data'Length = FADT_Length then
         Word (Flags_Offset, 16#20_0000#, 4);
         if Mode not in "fadt-none" | "fadt-reduced" then Word (A_Control_Offset, 16#1000#, 4); end if;
         if Mode = "fadt-paired" then Word (B_Control_Offset, 16#1004#, 4); end if;
         Data (Control_Length_Offset + 1) := (if Mode = "fadt-invalid" then 1 else 2);
         if Mode = "fadt-reduced" then
            Word (Flags_Offset, 16#30_0000#, 4);
            Data (Sleep_Control_Offset + 2) := 8;
            Data (Sleep_Control_Offset + 4) := 1;
            Word (Sleep_Control_Offset + 4, 16#1234#, 8);
         end if;
      end if;
      for Byte of Data loop Sum := Sum + Byte; end loop;
      Data (Checksum_Offset + 1) := 0 - Sum;
      return Data;
   end Fixed_Table;
   Reply : Q.Response;
   Result, Again : P.Report;
   Limits : P.Policy;
   Expected : P.Outcome := P.Finished;
   Raised : Boolean := False;
   procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), Value, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "snapshot request");
   end Send;
begin
   if Mode /= "unstarted" then
      Send (Q.Start_Snapshot, (if Mode = "fadt-duplicate" then 3 elsif Is_FADT_Case then 2 else 1));
      Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, C.DSDT, Table, Reply);
      Check (Reply.Status = Q.OK, "real AML admitted");
      if Is_FADT_Case then
         declare Data : constant Firmware_Tables.Bytes := Fixed_Table; begin
            Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 2, C.Description, Data, Reply);
            Check (Reply.Status = Q.OK, "copied FADT admitted as immutable description");
            if Mode = "fadt-duplicate" then
               Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 3, C.Description, Data, Reply);
               Check (Reply.Status = Q.OK, "second copied FADT available for ambiguity rejection");
            end if;
         end;
      end if;
      Send (Q.Finish_Snapshot);
      Check (Q.Current (Server) = Q.Complete, "complete snapshot");
   end if;
   if Mode = "log-error" then Expected := P.Owner_Error;
   elsif Mode = "empty" then Expected := P.No_Devices;
   elsif Mode in "hardware" | "fadt-hardware" | "unstarted" then Expected := P.Not_Ready;
   elsif Mode in "blocked" | "both" then Expected := P.Lid_Failed;
   elsif Mode = "source" then Expected := P.Source_Failed;
   elsif Mode in "invalid" | "changed" then Expected := P.Battery_Failed;
   elsif Mode in "fatal" | "source-fatal" then Expected := P.Recovery_Required;
   elsif Mode = "zero" then Limits.Fuel := 0; Expected := P.Budget_Exhausted;
   elsif Mode = "fuel" then Limits.Fuel := 1; Expected := P.Budget_Exhausted;
   elsif Mode in "limit" | "blocked-limit" then Limits.Batteries := 1; Expected := P.Battery_Limit;
   end if;
   declare Before : constant Q.Revision_Number := Q.Revision (Server); begin
      begin
         P.Run (Server, Result, Limits);
      exception
         when Program_Error =>
            if Mode /= "log-error" then raise; end if;
            Raised := True;
      end;
      Check (Raised = (Mode = "log-error"), "only modeled logging exception escapes");
      if Raised then P.Run (Server, Result, Limits); end if;
      Check (Result.Status = Expected, "native typed outcome " & Result.Status'Image);
      Check (Q.Valid (Server) and then Q.Revision (Server) = Before, "point observations preserve owner/revision");
   end;
   Check (Result.Charged <= Limits.Fuel, "one shared fuel budget across families");
   Check (Result.Recovery_Needed = (Mode in "fatal" | "source-fatal"), "execution uncertainty retained");
   Check (Result.Lid_Query_Failed = (Mode in "blocked" | "both" | "blocked-limit"),
     "ordinary lid failure recorded separately from absence or uncertainty");
   Check (Result.Source_Query_Failed = (Mode in "source" | "both" | "source-fatal"),
     "ordinary source failure recorded separately from absence or uncertainty");
   if Mode /= "log-error" then
   Check (Power_Test_Log.Lids = Boolean'Pos (Result.Lid_Observed)
     and then Power_Test_Log.Sources = Boolean'Pos (Result.Source_Observed), "only actual samples logged");
   Check (Power_Test_Log.Batteries = Result.Sampled_Batteries and then
     Power_Test_Log.Absent = Result.Absent_Batteries, "actual battery/presence outcomes logged");
   Check (Power_Test_Log.Summaries = 1, "one final summary");
   else
      Check (Power_Test_Log.Total = 1 and then Power_Test_Log.Sleep_Registers = 1 and then Result.Lid_Observed and then
        not Result.Source_Observed and then Result.Batteries = 0, "exception returns terminal partial point report without replay");
   end if;
   if Mode = "log-error" then
      Check (Result.Status = P.Owner_Error, "unexpected exception never returns Not_Attempted");
   elsif Mode = "fadt-hardware" then
      Check (not Result.Lid_Observed and then not Result.Source_Observed and then
        Result.Batteries = 0 and then Result.Charged = 0,
        "passive FADT classification does not bypass AML readiness or execute device methods");
   elsif Mode in "normal" | "absent" | "limit" or else Is_FADT_Case then
      Check (Result.Lid_Observed and then Result.Lid = P.Open and then
        Result.Source_Observed and then Result.Source = P.Online, "actual lid and source states");
      Check (Result.Batteries = (if Mode = "limit" then 1 else 2), "bounded battery enumeration");
      Check (Result.Sampled_Batteries = (if Mode = "absent" then 0 elsif Mode = "limit" then 1 else 2)
        and then Result.Absent_Batteries = (if Mode = "absent" then 2 else 0),
        "actual battery collection or absence without later fatal methods");
   elsif Mode in "blocked" | "source" | "both" | "blocked-limit" | "source-fatal" then
      Check (Result.Lid_Observed = (Mode in "source" | "source-fatal") and then
        Result.Lid = (if Result.Lid_Observed then P.Open else P.Unknown),
        "independent pass never invents failed lid data");
      Check (Result.Source_Observed = (Mode in "blocked" | "blocked-limit") and then
        Result.Source = (if Result.Source_Observed then P.Online else P.Unknown),
        "ordinary lid failure permits AC observation; bad AC never means offline");
      Check (Result.Batteries = (if Mode in "blocked-limit" | "source-fatal" then 1 else 2) and then
        Result.Sampled_Batteries = (if Mode = "source-fatal" then 0 else Result.Batteries) and then
        Result.Absent_Batteries = 0,
        "independent battery samples proceed after ordinary failure, but stop on limit or Fatal");
   elsif Mode in "fatal" | "hardware" | "unstarted" | "zero" | "fuel" | "empty" then
      Check (not Result.Lid_Observed and then Result.Lid = P.Unknown
        and then not Result.Source_Observed and then Result.Source = P.Unknown
        and then Result.Batteries = 0, "no later or invented observations after refusal");
   else
      Check (Result.Batteries = 1 and then Result.Sampled_Batteries = 0
        and then Result.Absent_Batteries = 0, "bad or changed battery bundle never escapes");
   end if;
   Check (Power_Test_Log.Sleep_Registers = 1, "one bounded sleep metadata stage is logged");
   if Mode = "unstarted" then
      Check (Result.Sleep_Registers.Status = Sleep.Not_Ready, "unready catalog cannot imply a sleep model");
   elsif not Is_FADT_Case then
      Check (Result.Sleep_Registers.Status = Sleep.Missing_FADT, "missing FADT stays unknown");
   elsif Mode = "fadt-duplicate" then
      Check (Result.Sleep_Registers.Status = Sleep.Multiple_FADT and then Result.Sleep_Registers.Table = 0,
        "duplicate FADT never selects an arbitrary register model");
   elsif Mode = "fadt-short" then
      Check (Result.Sleep_Registers.Status = Sleep.Invalid_FADT, "invalid decoded FADT stays unknown");
   elsif Mode = "fadt-invalid" then
      Check (Result.Sleep_Registers.Status = Sleep.Classified
        and then Result.Sleep_Registers.Data.Code = Sleep.Invalid_Fixed_Blocks,
        "invalid register description never yields a selected model");
   else
      Check (Result.Sleep_Registers.Status = Sleep.Classified and then Result.Sleep_Registers.Data.Code = Sleep.Selected,
        "model comes from the actual retained FADT");
      Check (Result.Sleep_Registers.Data.Model = (if Mode = "fadt-paired" then Sleep.PM1A_And_B
        elsif Mode = "fadt-reduced" then Sleep.Hardware_Reduced elsif Mode = "fadt-none" then Sleep.No_Sleep_Registers
        else Sleep.PM1A_Only), "exact descriptive used-field model");
      Check (Result.Sleep_Registers.Data.Low_Power_Idle, "low-power-idle bit observed without readiness claim");
   end if;
   if Mode = "normal" then
      declare
         Item : F.Description;
         Selection : Sleep.Selection;
      begin
         Selection := Sleep.Describe (Item);
         Check (Selection.Code = Sleep.Selected and then Selection.Model = Sleep.No_Sleep_Registers, "empty fixed model");
         Item.Revision := 6; Item.Lengths (F.PM1A_Control) := 2; Item.Lengths (F.PM1B_Control) := 2;
         Item.Legacy (F.PM1B_Control) := 16#1000#;
         Selection := Sleep.Describe (Item);
         Check (Selection.Code = Sleep.Inconsistent_Fixed_Blocks, "B-only fixed model refused");
         Item.Legacy (F.PM1B_Control) := 0; Item.Legacy (F.PM1A_Control) := 16#1000#;
         Item.Extended (F.PM1A_Control) := (True, (3, 16, 0, 2, 16#1234#));
         Selection := Sleep.Describe (Item);
         Check (Selection.Code = Sleep.Selected and then Selection.Model = Sleep.PM1A_Only
           and then Selection.Legacy_Fallback_A, "legacy fallback explicitly reported");
         Item.Flags := 16#30_0000#;
         Selection := Sleep.Describe (Item);
         Check (Selection.Code = Sleep.Selected and then Selection.Model = Sleep.No_Sleep_Registers,
           "HW reduced absent control ignores legacy PM1 addresses");
         Item.Sleep_Control := (True, (0, 8, 0, 1, 16#1234#));
         for Space in Unsigned_8 range 0 .. 3 loop
            Item.Sleep_Control.Value.Space := Space; Selection := Sleep.Describe (Item);
            Check (Selection.Code = (if Space <= 2 then Sleep.Selected else Sleep.Invalid_Sleep_Control),
              "reduced memory IO and PCI description spaces distinguished");
         end loop;
         Item.Sleep_Control.Value.Space := 0;
         Item.Sleep_Control.Value.Width := 16; Selection := Sleep.Describe (Item);
         Check (Selection.Code = Sleep.Invalid_Sleep_Control, "reduced control byte width required");
         Item.Sleep_Control.Value.Width := 8; Item.Sleep_Control.Value.Bit_Offset := 1;
         Selection := Sleep.Describe (Item);
         Check (Selection.Code = Sleep.Invalid_Sleep_Control, "reduced control zero bit offset required");
         Item.Sleep_Control.Value.Bit_Offset := 0; Item.Sleep_Control.Value.Access_Size := 2;
         Selection := Sleep.Describe (Item);
         Check (Selection.Code = Sleep.Invalid_Sleep_Control, "reduced control byte access geometry required");
         Item.Revision := 4; Item.Extended (F.PM1A_Control) := (others => <>);
         Selection := Sleep.Describe (Item);
         Check (Selection.Code = Sleep.Selected and then Selection.Model = Sleep.PM1A_Only
           and then not Selection.Low_Power_Idle, "old revision ignores HW-reduced and low-idle bits");
      end;
   end if;
   Q.Handle (Server, Q.Observer,
     (Label => Q.Read_Notification_State, Data => [Q.Notification_State_Summary, 0, 0, 0], others => <>), Reply);
   Check (Reply.Status = Q.OK and then Reply.Data (1 .. 3) = [0, 0, 0],
     "point pass does not enable capture,claim events or imply cache freshness");
   declare Logs : constant Natural := Power_Test_Log.Total; begin
      P.Run (Foreign_Server, Again, (Fuel => 0, Batteries => 1));
      Check (Again = Result and then Power_Test_Log.Total = Logs, "terminal pass never replays AML/logs on another Core");
      Check (Q.Current (Foreign_Server) = Q.Idle and then Q.Revision (Foreign_Server) = 0, "foreign owner untouched");
   end;
   Ada.Text_IO.Put_Line ("Native power checks" & Checks'Image);
end Native_Power_Tests;
