pragma Ada_2022;
with AML_Objects; with Ada.Text_IO; with Ada.Command_Line; with Ada.Streams; with Ada.Streams.Stream_IO;
with ACPI_Service; with ACPI_Requests; with ACPI_Native_Power_Preparation; with ACPI_Native_Power_Entry;
with CuBit.Messages; with CuBit.Power_Event_Exchange; with Firmware_Tables; with Interfaces;
with S5_Boundary; with ACPI_Sleep_Entry;
procedure Cached_S5_Adapter_Tests is
   package Q renames ACPI_Requests; package M renames CuBit.Messages;
   package E renames CuBit.Power_Event_Exchange; package P renames ACPI_Native_Power_Preparation;
   package Entry_Owner renames ACPI_Native_Power_Entry; package B renames S5_Boundary;
   package S renames ACPI_Sleep_Entry;
   use type Q.Outcome, Interfaces.Unsigned_64, Interfaces.Unsigned_16, S.Action;
   use type Ada.Streams.Stream_Element_Offset;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Message : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Message; end if; end Check;
   function Read_Table return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         Check (Last = Raw'Last, "complete table read");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;


   Table : constant Firmware_Tables.Bytes := Read_Table;
   Mode : constant String := Ada.Command_Line.Argument (2);
   Scenario : constant String := Ada.Command_Line.Argument (3);
   Server : Q.State (1, Table'Length, Table'Length, 1, 0, Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes, Value_Element_Capacity => AML_Objects.Max_Elements);
   Reply : Q.Response;
   Request, Response : M.Message := M.NULL_MESSAGE;
   Handled, Reply_Now : Boolean;
   Step : Entry_Owner.Step_Outcome;
   procedure Send (Label : Interfaces.Unsigned_32) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), 1, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "owner protocol succeeds");
   end Send;
begin
   Send (Q.Start_Snapshot);
   Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, ACPI_Service.DSDT, Table, Reply);
   Check (Reply.Status = Q.OK, "import");
   Q.Handle (Server, Q.Snapshot_Provider, (Label => Q.Finish_Snapshot, Data => [Q.Revision (Server), 0, 0, 0], others => <>), Reply);
   Check (Reply.Status = Q.OK, "startup");
   Request.tag.label := E.Prepare_To_Sleep; Request.tag.length := E.Sleep_Request_Words;
   Request.authorityTag := E.Authority_Tag;
   Request.words := [E.Request_Word => 1, E.State_Word => E.Sleep_State'Enum_Rep (E.S5), E.Confirm_Word => E.Sleep_Confirmation, others => 0];
   if Scenario in "runtime-stopped" | "stopped-normal-refused" | "stopped-ec-retained"
     | "stopped-hardware-refused" | "stopped-fields-busy" | "stopped-entry-ec-retained" then B.Runtime_Stopped := True;
   elsif Scenario = "runtime-active" then B.Runtime_Idle := False;
   elsif Scenario = "runtime-fresh" then B.Runtime_Fresh := True; end if;
   if Scenario in "hardware-refused" | "stopped-hardware-refused" then B.Hardware_Ready := False;
   elsif Scenario in "ec-retained" | "stopped-ec-retained" then B.EC_Quiet := False;
   elsif Scenario = "stopped-fields-busy" then B.Fields_Quiet := False;
   elsif Scenario = "clock-missing" then B.Clock_Known := False;
   elsif Scenario = "unauthorized" then Request.authorityTag := 0;
   elsif Scenario = "malformed" then Request.words (E.Confirm_Word) := 0; end if;
   P.Dispatch (Server, Request, Scenario = "stopped-normal-refused", Response, Handled,
      Cached_Shutdown_Selected => Scenario not in "not-selected" | "stopped-normal-refused");
   Check (Handled and then B.Windows = 0 and then B.Commands = 0 and then B.Saves = 0, "prepare no AML window, hardware or saved reply");
   if Scenario in "hardware-refused" | "ec-retained" | "clock-missing" | "not-selected" | "runtime-active" | "runtime-fresh" | "stopped-normal-refused" | "stopped-hardware-refused"
     | "stopped-ec-retained" | "stopped-fields-busy" then
      Check (Response.words (E.Prepare_Status_Word) = E.Prepare_Status'Enum_Rep (E.Not_Ready), "unsafe prepare refused");
   elsif Scenario = "unauthorized" then
      Check (Response.words (E.Prepare_Status_Word) = E.Prepare_Status'Enum_Rep (E.Not_Approved), "role refusal");
   elsif Scenario = "malformed" then
      Check (Response.words (E.Prepare_Status_Word) = E.Prepare_Status'Enum_Rep (E.Malformed), "confirmation refusal");
   elsif Mode /= "normal" then
      Check (Response.words (E.Prepare_Status_Word) = E.Prepare_Status'Enum_Rep (E.No_Sleep_Package), "no dynamic or malformed package fallback");
   else
      Check (Response.words (E.Prepare_Status_Word) = E.Prepare_Status'Enum_Rep (E.Prepared), "cached-only prepare succeeds");
      Check (Response.words (E.Sleep_Types_Word) = E.Sleep_Types_Of (5,0), "only PM1a used");
      Request.tag.label := E.Enter_Sleep_State;
      if Scenario = "entry-hardware-lost" then B.Hardware_Ready := False;
      elsif Scenario in "entry-ec-retained" | "stopped-entry-ec-retained" then B.EC_Quiet := False;
      elsif Scenario = "entry-clock-lost" then B.Clock_Known := False;
      elsif Scenario = "entry-unauthorized" then Request.authorityTag := 0; end if;
      if Scenario = "save-refused" then B.Saved_Allowed := False; end if;
      if Scenario = "unknown-write" then B.Hardware_Result := S.Indeterminate; end if;
      Entry_Owner.Dispatch (Server, Request, False, Response, Handled, Reply_Now, Cached_Shutdown_Selected => True);
      Check (Handled and then B.Commands = 0 and then B.Windows = 0, "enter queues without executing or AML");
      if Scenario in "entry-hardware-lost" | "entry-ec-retained" | "entry-clock-lost" | "entry-unauthorized" | "stopped-entry-ec-retained" then
         Check (Reply_Now and then B.Saves = 0 and then B.Commands = 0, "entry rechecks owner, clock and authority before saved reply or hardware");
      elsif Scenario = "save-refused" then
         Check (Reply_Now and then B.Saves = 1, "save failure has no work");
      else
         Check (not Reply_Now and then B.Saves = 1 and then Entry_Owner.Blocks_Runtime, "one saved reply before commands");
         if Scenario = "stop" then Entry_Owner.Begin_Stop; end if;
         for I in 1 .. 20 loop
            Entry_Owner.Advance (Server, Step);
            if B.Commands = 6 then B.Milliseconds := 10 + E.Settle_Milliseconds; end if;
         end loop;
         Check (B.Answers = 1 and then B.Windows = 0, "exactly one answer; no normal window");
         if Scenario = "stop" then Check (B.Commands = 0, "stop precedes hardware");
         elsif Scenario = "unknown-write" then Check (B.Commands = 1 and then not Entry_Owner.Quiet, "uncertain first action retained, no next command");
         else
            Check (B.Commands = 6, "single PM1 command sequence");
            Check (B.Issued (1).Kind = S.Mask_GPEs and then B.Issued (2).Kind = S.Clear_GPEs
              and then B.Issued (3).Kind = S.Clear_Wake and then B.Issued (4).Kind = S.Read_A
              and then B.Issued (5).Kind = S.Stage_A and then B.Issued (6).Kind = S.Enable_A, "exact existing sequencer order");
            Check (B.Issued (5).Value = E.PM1_Sleep_Value (B.Saved_Read, 5, False)
              and then B.Issued (6).Value = E.PM1_Sleep_Value (B.Saved_Read, 5, True), "preserve admitted PM1 bits, exact types and enable");
         end if;
         Entry_Owner.Dispatch (Server, Request, True, Response, Handled, Reply_Now);
         Check (Reply_Now and then B.Saves = 1 and then B.Answers = 1, "no replay of terminal entry");
      end if;
   end if;
   Check (not Q.Retained_Results_Pending (Server), "no result roots retained");
   Ada.Text_IO.Put_Line ("Cached S5 checks" & Checks'Image);
end Cached_S5_Adapter_Tests;
