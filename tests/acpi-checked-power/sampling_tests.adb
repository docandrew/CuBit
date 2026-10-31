pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Lid;
with ACPI_Power_Source;
with ACPI_Requests.Lids;
with ACPI_Requests.Power_Sources;
with Firmware_Tables;
with Interfaces;
procedure Sampling_Tests is
   package C renames ACPI_Service;
   package Q renames ACPI_Requests;
   package IO renames Ada.Streams.Stream_IO;
   Lid_Calls, Power_Calls : Natural := 0;
   procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      if C.Values.Node_Name (Item) = "_LID" then Lid_Calls := Lid_Calls + 1; end if;
      if C.Values.Node_Name (Item) = "_PSR" then Power_Calls := Power_Calls + 1; end if;
      C.Evaluate_Named (Service, Item, Budget, Result, Status);
   end Evaluate;
   package Lid is new ACPI_Lid (C, Evaluate);
   package Power is new ACPI_Power_Source (C, Evaluate);
   package Lids is new Q.Lids (Lid);
   package Sources is new Q.Power_Sources (Power);
   use type Ada.Streams.Stream_Element_Offset, Q.Outcome, Q.Phase, Q.Revision_Number;
   use type Lids.Controller.Sampling_Outcome, Sources.Controller.Sampling_Outcome;
   use type Lid.Device_Status.Status_Outcome, Power.Device_Status.Status_Outcome;
   use type Lid.Lid_State, Power.Source_State;
   Checks : Natural := 0;
   Mode : constant String := Ada.Command_Line.Argument (2);
   Budget : Natural := 1000;
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
   Server : Q.State (1, Table'Length, Table'Length, 1, 0);
   Reply : Q.Response;
   LR : Lids.Controller.Sampling_Report;
   PR : Sources.Controller.Sampling_Report;
   procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), Value, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "snapshot request");
   end Send;
begin
   if Mode /= "unstarted" then
      Send (Q.Start_Snapshot, 1);
      Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, C.DSDT, Table, Reply);
      Check (Reply.Status = Q.OK, "real AML admitted"); Send (Q.Finish_Snapshot);
      Check (Q.Current (Server) = Q.Complete, "complete catalog");
   end if;
   Lid_Calls := 0; Power_Calls := 0;
   if Mode = "zero" then Budget := 0; elsif Mode = "fuel" then Budget := 1; end if;
   declare Before : constant Q.Revision_Number := Q.Revision (Server); begin
      Lids.Sample_Ready (Server, Budget, LR);
      Sources.Sample_Ready (Server, Budget, PR);
      Check (Q.Valid (Server) and then Q.Revision (Server) = Before, "same owner preserves invariant/revision");
   end;
   Check (LR.Charged <= Budget and then PR.Charged <= Budget, "each full search/status/query shares budget");
   if Mode in "ready" | "default" then
      Check (LR.Outcome = Lids.Controller.Sampled and then PR.Outcome = Sources.Controller.Sampled,
        "both status-checked members sampled");
      Check (Lid_Calls = 1 and then Power_Calls = 1, "one fixed member evaluation each");
      Check (LR.Query.State = Lid.Open and then PR.Query.State = Power.Online, "actual method values copied");
      if Mode = "default" then
         Check (LR.Device_Status.Defaulted and then PR.Device_Status.Defaulted
           and then LR.Device_Status.Flags = 16#FFFF_FFFF# and then PR.Device_Status.Flags = 16#FFFF_FFFF#,
           "missing direct STA preserves existing full default");
      end if;
   else
      Check (Lid_Calls = 0 and then Power_Calls = 0, "ineligible status never invokes LID or PSR");
      Check (LR.Query.State = Lid.Unknown and then PR.Query.State = Power.Unknown,
        "refusal never means closed lid or offline source");
      if Mode in "absent" | "disabled" | "broken" | "noini" then
         Check (LR.Outcome = Lids.Controller.Device_Unavailable and then PR.Outcome = Sources.Controller.Device_Unavailable,
           "valid unusable STA blocks sample");
         Check (LR.Device_Status.Outcome = Lid.Device_Status.Status_Available
           and then PR.Device_Status.Outcome = Power.Device_Status.Status_Available, "valid flags differ from evaluation error");
      elsif Mode = "wrong" then
         Check (LR.Outcome = Lids.Controller.Status_Failed and then PR.Outcome = Sources.Controller.Status_Failed,
           "object status failure blocks members");
         Check (LR.Device_Status.Outcome = Lid.Device_Status.Wrong_Result_Type and then
           PR.Device_Status.Outcome = Power.Device_Status.Wrong_Result_Type,
           "wrong-type STA distinguished and cleaned");
      elsif Mode in "unstarted" | "hardware" then
         Check (LR.Outcome = Lids.Controller.Not_Ready and then PR.Outcome = Sources.Controller.Not_Ready,
           "startup region/device barrier enforced");
      elsif Mode in "zero" | "fuel" then
         Check (LR.Outcome = Lids.Controller.Budget_Exhausted and then PR.Outcome = Sources.Controller.Budget_Exhausted,
           "bounded incomplete sample");
      elsif Mode = "empty" then
         Check (LR.Outcome = Lids.Controller.No_Match and then PR.Outcome = Sources.Controller.No_Match,
           "no device is distinct from device failure");
      end if;
   end if;
   if Mode = "ready" then
      declare
         Lid_Budget : constant Natural := LR.Charged - 1;
         Power_Budget : constant Natural := PR.Charged - 1;
         Short_Lid : Lids.Controller.Sampling_Report;
         Short_Power : Sources.Controller.Sampling_Report;
      begin
         Check (LR.Charged > 1 and then PR.Charged > 1, "actual STA and member queries charge fuel");
         Lids.Sample_Ready (Server, Lid_Budget, Short_Lid);
         Sources.Sample_Ready (Server, Power_Budget, Short_Power);
         Check (Short_Lid.Outcome = Lids.Controller.Budget_Exhausted
           and then Short_Lid.Query.State = Lid.Unknown and then Short_Lid.Charged <= Lid_Budget,
           "one-short lid budget cannot restart fuel after STA");
         Check (Short_Power.Outcome = Sources.Controller.Budget_Exhausted
           and then Short_Power.Query.State = Power.Unknown and then Short_Power.Charged <= Power_Budget,
           "one-short power budget cannot restart fuel after STA");
      end;
   end if;
   Ada.Text_IO.Put_Line ("Power sampling checks" & Checks'Image);
end Sampling_Tests;
