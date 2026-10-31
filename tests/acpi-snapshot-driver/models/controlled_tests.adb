pragma Ada_2022;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Snapshot_Startup;
with ACPI_Snapshot_Startup.With_Driver;
with ACPI_Device_Initialization;
with ACPI_Device_Policy;
with AML_Execute;
with AML_Decode;
procedure Controlled_Tests is
   package C renames ACPI_Service;
   package Q renames ACPI_Requests;
   package L renames ACPI_Snapshot_Startup;
   package R renames L.R;
   package IO renames Ada.Streams.Stream_IO;
   use type L.Outcome, L.State_Access, R.S.Table_ID;
   use type C.Values.Access_Status, ACPI_Device_Policy.Region_Outcome;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   Mode : constant String := Ada.Command_Line.Argument (2);
   Checks, Copies, Infos, Evaluations, Barriers : Natural := 0;
   Completion_Seen : Boolean := False;
   Fault_Seen : Boolean := False;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table return R.F.Bytes is
      File : IO.File_Type;
      Data : Ada.Streams.Stream_Element_Array (1 .. 4_096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      IO.Read (File, Data, Last); IO.Close (File);
      return Bytes : R.F.Bytes (1 .. Natural (Last)) do
         for I in Bytes'Range loop Bytes (I) := R.F.Byte (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
      end return;
   end Read_Table;
   Table : constant R.F.Bytes := Read_Table;
   procedure Info (Item : out L.Inventory; Status : out R.Copy_Status) is
   begin
      Infos := Infos + 1;
      if Mode = "source-exception" then raise Program_Error with "modeled source refusal"; end if;
      Item := (True, 1, L.Count (Table'Length), L.Count (Table'Length));Status := R.Copy_OK;
   end Info;
   procedure Describe (ID : Positive; Item : out L.Description; Status : out R.Copy_Status) is
   begin
      Check (ID = 1, "single fixture");Item := ("DSDT", Table (9), L.Count (Table'Length));Status := R.Copy_OK;
   end Describe;
   procedure Copy (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
      Copied : out R.Byte_Count; Status : out R.Copy_Status) is
   begin
      Check (ID = 1, "exact table identity"); Copies := Copies + 1;
      for I in Into'Range loop Into (I) := Table (Table'First + Offset + I - Into'First); end loop;
      Copied := R.Byte_Count (Into'Length);Status := R.Copy_OK;
   end Copy;
   procedure Clock (Now : out R.Tick; Available : out Boolean) is
   begin
      Available := True;
      Now := (if Mode = "before-deadline" or else
        (Mode = "after-deadline" and then Completion_Seen) then 11 else 0);
   end Clock;
   procedure Regions (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      Node : C.Namespace.Node_ID;
      Description : C.Node_Descriptor;
      Status : C.Values.Access_Status;
      Marker : C.Values.Result;
   begin
      Barriers := Barriers + 1; Charged := 0;
      C.Find_Child (Service, C.Namespace.Root, "MARK", Node, Status);
      Check (Status = C.Values.Available, "initializer marker found");
      C.Describe_Node (Service, Node, Description, Status);
      Check (Status = C.Values.Available, "initializer marker identity");
      C.Evaluate_Named (Service, Description, Budget, Marker, Status);
      Check (Status = C.Values.Available and then Marker.Status = AML_Execute.Returned
        and then Marker.Number = 16#5A#, "actual initializer effect matches ACPICA");
      Charged := Marker.Charged;
      if Mode = "barrier-exception" then raise Program_Error with "modeled provider exception"; end if;
      Outcome := (if Mode = "unavailable" then ACPI_Device_Policy.Regions_Unavailable
        else ACPI_Device_Policy.No_Regions_Required);
   end Regions;
   procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Evaluations := Evaluations + 1; Completion_Seen := True;
      if Mode = "evaluation-exception" then raise Program_Error with "modeled unknown provider effect"; end if;
      C.Evaluate_Named (Service, Item, Budget, Result, Status);
   end Evaluate;
   package Driver is new ACPI_Device_Initialization (C, Regions, Evaluate);
   package Controlled is new L.With_Driver (Driver, Info, Describe, 4_096, Copy, Clock);
   procedure Default_Load is new L.Load (Info, Describe, 4_096, Copy, Clock);
   Context, Other_Context : Controlled.Session;
   Run : Driver.Session;
   Owner, Again : L.State_Access;
   Result, Repeated : L.Report;
begin
   Check (not Controlled.Consumed (Context), "fresh controller");
   if Mode = "default" then
      Default_Load (10, Owner, Result);
      Check (Result.Status = L.Loaded and then Owner /= null and then not Result.Owner_Retained, "default behavior retained");
      Check (Evaluations = 0 and then Barriers = 0, "default never selects controlled actuals");
   else
      begin
         Controlled.Load (Context, Run, 10, Owner, Result);
      exception
         when Error : Program_Error =>
            --  Hosted observation only: the native runtime terminates here.
            Check (Mode in "source-exception" | "evaluation-exception" |
              "barrier-exception", "only modeled contract faults propagate");
            Check (Ada.Exceptions.Exception_Message (Error) =
              (if Mode = "source-exception" then "modeled source refusal"
               elsif Mode = "evaluation-exception" then "modeled unknown provider effect"
               else "modeled provider exception"), "exact callback fault");
            Fault_Seen := True;
      end;
      Check (Controlled.Consumed (Context), "one shot consumed even on refusal");
      if Mode in "source-exception" | "evaluation-exception" | "barrier-exception" then
         Check (Fault_Seen, "callback contract violation cannot become a typed result");
         Check ((if Mode = "source-exception" then Evaluations = 0 and then Barriers = 0
           elsif Mode = "evaluation-exception" then Evaluations = 1 and then Barriers = 0
           else Evaluations = 1 and then Barriers = 1), "effects before the exact fault");
         declare
            Prior_Copies : constant Natural := Copies;
            Prior_Infos : constant Natural := Infos;
            Prior_Evaluations : constant Natural := Evaluations;
            Prior_Barriers : constant Natural := Barriers;
         begin
            Controlled.Load (Context, Run, 10, Again, Repeated);
            Check (Repeated.Status = L.Startup_Already_Consumed,
              "host observation cannot replay the consumed controller");
            Check (Copies = Prior_Copies and then Infos = Prior_Infos and then
              Evaluations = Prior_Evaluations and then Barriers = Prior_Barriers,
              "no repeated source, AML or provider effects after host fault");
         end;
         Ada.Text_IO.Put_Line ("controlled snapshot checks " & Checks'Image);
         return;
      end if;
      if Mode = "before-deadline" then
         Check (Result.Status = L.Deadline_Expired and then Owner = null and then not Result.Owner_Retained,
           "deadline before provider releases owner");
      elsif Mode = "module-fatal" then
         Check (Result.Status = L.Startup_Refused and then Owner /= null and then Result.Owner_Retained,
           "controlled completion failure conservatively retains owner");
         Check (Result.Fatal.Present and then Evaluations = 0 and then Barriers = 0,
           "module Fatal prevents device provider callbacks");
      elsif Mode = "after-deadline" then
         Check (Result.Status = L.Deadline_Expired and then Owner /= null and then Result.Owner_Retained,
           "late completion retains owning arena");
      else
         Check (Result.Status = L.Loaded and then Owner /= null and then not Result.Owner_Retained,
           "controlled snapshot completed");
         Check (Evaluations = 1 and then Barriers = 1, "selected root evaluator and barrier called once");
         Check ((Q.Device_Completion (Owner.all).Region_Status = ACPI_Device_Policy.Regions_Unavailable) =
           (Mode = "unavailable"), "provider readiness separate from snapshot");
      end if;
      declare
         Prior_Copies : constant Natural := Copies;
         Prior_Infos : constant Natural := Infos;
         Prior_Evaluations : constant Natural := Evaluations;
      begin
         Controlled.Load (Context, Run, 10, Again, Repeated);
         Check (Repeated.Status = L.Startup_Already_Consumed and then Again = Owner,
           "repeat preserves original owner without ready attestation");
         Check (Copies = Prior_Copies and then Infos = Prior_Infos and then Evaluations = Prior_Evaluations,
           "no source, AML or provider replay");
         if Mode = "normal" then
            Controlled.Load (Other_Context, Run, 10, Again, Repeated);
            Check (Repeated.Status = L.Startup_Already_Consumed and then Again = null,
              "used driver rejected before a fresh snapshot");
            Check (Copies = Prior_Copies and then Infos = Prior_Infos, "used driver cannot replay module phase");
         end if;
      end;
   end if;
   Ada.Text_IO.Put_Line ("controlled snapshot checks " & Checks'Image);
end Controlled_Tests;
