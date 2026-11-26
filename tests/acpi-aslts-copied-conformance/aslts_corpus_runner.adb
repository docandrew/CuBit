pragma Ada_2022;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with ACPI_Service_Core;
with ACPI_Service_Core.Copy_Probe;
with AML_Decode;
with AML_Delays;
with AML_Events;
with AML_Execute;
with AML_Profile_Events;
with Firmware_Tables;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity.Issuer;
with ACPI_FADT.Transactions;
with Hosted_Byte_Memory;
procedure ASLTS_Corpus_Runner is
   package Streams renames Ada.Streams.Stream_IO;
   use type Streams.Count, Ada.Streams.Stream_Element_Offset;
   use type Ada.Real_Time.Time, Ada.Real_Time.Time_Span;
   use type AML_Decode.Integer_Value, AML_Execute.Execution_Status;
   Started : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   Host_Window_Seconds : constant := 60;
   Fuel : constant Natural := 5_000_000;
   Maximum_Table_Bytes : constant := 16 * 1024 * 1024;
   Last_Event : AML_Profile_Events.Event;
   Event_Count : Natural := 0;
   procedure Profile (Item : AML_Profile_Events.Event) is
   begin
      if Item.Kind in AML_Profile_Events.Statement_Visit | AML_Profile_Events.Operand_Visit then
         Last_Event := Item;
         if Event_Count < Natural'Last then Event_Count := Event_Count + 1; end if;
      end if;
   end Profile;
   procedure Clock (Value : out AML_Decode.Integer_Value; Available : out Boolean) is
      Elapsed : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Clock - Started;
   begin
      Available := Elapsed >= Ada.Real_Time.Time_Span_Zero and then Elapsed < Ada.Real_Time.Seconds (Host_Window_Seconds);
      Value := (if Available then AML_Decode.Integer_Value (Elapsed / Ada.Real_Time.Microseconds (1)) else 0);
   end Clock;
   procedure Delay_AML (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Span : constant Ada.Real_Time.Time_Span :=
        (case Item.Kind is when AML_Delays.Sleep_Delay => Ada.Real_Time.Milliseconds (Integer (Item.Milliseconds)),
                          when AML_Delays.Stall_Delay => Ada.Real_Time.Microseconds (Integer (Item.Microseconds)));
   begin
      Result := AML_Delays.Failed;
      if Now + Span >= Started + Ada.Real_Time.Seconds (Host_Window_Seconds) then return; end if;
      delay until Now + Span; Result := AML_Delays.Completed;
   end Delay_AML;
   procedure Wait_Event (Milliseconds : AML_Events.Timeout; Outcome : out AML_Events.Timeout_Outcome) is
      Until_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (Integer (Milliseconds));
   begin
      Outcome := AML_Events.Unavailable;
      if Until_Time >= Started + Ada.Real_Time.Seconds (Host_Window_Seconds) then return; end if;
      delay until Until_Time; Outcome := AML_Events.Elapsed;
   end Wait_Event;
   function Read_Input return Firmware_Tables.Bytes is
      File : Streams.File_Type;
   begin
      if Ada.Command_Line.Argument_Count /= 1 then raise Program_Error with "one AML table argument required"; end if;
      Streams.Open (File, Streams.In_File, Ada.Command_Line.Argument (1));
      declare Size : constant Streams.Count := Streams.Size (File); begin
         if Size < 36 or else Size > Maximum_Table_Bytes then raise Program_Error with "table size outside harness bound"; end if;
         declare Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Size));
            Last : Ada.Streams.Stream_Element_Offset; Data : Firmware_Tables.Bytes (1 .. Natural (Size));
         begin
            Streams.Read (File, Raw, Last); Streams.Close (File);
            if Last /= Raw'Last then raise Program_Error with "short input"; end if;
            for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
            return Data;
         end;
      end;
   end Read_Input;
   Table : constant Firmware_Tables.Bytes := Read_Input;
   -- An upstream output may concatenate AML tables. Validate the complete
   -- bundle before staging any of it; Core still admits exact table extents.
   Maximum_Catalog_Tables : constant := 32;
   use type Firmware_Tables.Admission;
   function Signature_At (Offset : Natural) return Firmware_Tables.Signature is
      Signature : Firmware_Tables.Signature;
   begin
      for I in Signature'Range loop
         Signature (I) := Character'Val (Table (Table'First + Offset + I - 1));
      end loop;
      return Signature;
   end Signature_At;
   function Inspect_Bundle return Natural is
      Offset, Count : Natural := 0;
   begin
      while Offset < Table'Length loop
         if Table'Length - Offset < Firmware_Tables.Table_Header_Size then return 0; end if;
         declare
            Signature : constant Firmware_Tables.Signature := Signature_At (Offset);
            Header : constant Firmware_Tables.Table_Result := Firmware_Tables.Read_Table
              (Table (Table'First + Offset .. Table'Last), Signature);
         begin
            if Signature not in "DSDT" | "SSDT"
              or else (Signature = "DSDT" and then Offset /= 0)
              or else Header.Status /= Firmware_Tables.Accepted
              or else Count = Maximum_Catalog_Tables
            then return 0; end if;
            Count := Count + 1;
            Offset := Offset + Header.Extent;
         end;
      end loop;
      -- SSDT-only inputs need the same 64-bit empty-DSDT policy as acpiexec.
      -- It occupies a slot and counts against the explicit catalog ceiling.
      if Signature_At (0) = "SSDT" and then Count = Maximum_Catalog_Tables then return 0; end if;
      return Count;
   end Inspect_Bundle;
   Bundle_Count : constant Natural := Inspect_Bundle;
   Needs_Empty_DSDT : constant Boolean := Signature_At (0) = "SSDT";
   Empty_DSDT : constant Firmware_Tables.Bytes :=
     [68, 83, 68, 84, 36, 0, 0, 0, 2, 61, 67, 85, 66, 73, 84, 0,
      80, 82, 79, 70, 69, 77, 80, 84, 1, 0, 0, 0, 73, 78, 84, 76, 8, 4, 38, 32];
   Catalog_Count : constant Positive :=
     (if Bundle_Count = 0 then 1 else Bundle_Count + (if Needs_Empty_DSDT then 1 else 0));
   -- Storage follows admitted input size, with named, explicit hosted ceilings.
   Nodes : constant Positive := Positive'Min (1_000_000, Table'Length + 512);
   Objects : constant Positive := Positive'Min (262_144, Table'Length * 4 + 8192);
   Bytes : constant Positive := Positive'Min (64 * 1024 * 1024, Table'Length * 32 + 65_536);
   Elements : constant Positive := Positive'Min (4 * 1024 * 1024, Table'Length * 8 + 8192);
   -- Explicit named copied-owner conformance candidate; core defaults stay zero.
   Owner_Copied_Tables : constant Positive := 2;
   Owner_Copied_Bytes : constant Positive := 256;
   package C is new ACPI_Service_Core
     (Perform_Delay => Delay_AML, Read_Microseconds => Clock,
      Namespace_Node_Capacity => Nodes, Aggregate_Method_Capacity => Table'Length + 65_536,
      Retained_Result_Capacity => 64, Wait_Event_Timeout => Wait_Event,
      Copied_Table_Capacity => Owner_Copied_Tables, Copied_Byte_Capacity => Owner_Copied_Bytes,
      Block_Capacity => 256, Profile_Enabled => True, Observe_Profile => Profile);
   package Retention is new C.Copy_Probe;
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package M renames Hosted_Byte_Memory;
   use type A.Endpoint_ID, AML_Identity.Identity;
   use type AML_Decode.Byte, L.Register_Geometry;


   type Geometry_List is array (Positive range 1 .. 128) of L.Register_Geometry;
   type Model is record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Memory : M.State := M.Empty;
      Ports : M.State := M.Empty;
      Saved : Geometry_List;
      Used, Reads, Writes : Natural := 0;
   end record;
   function Pending (IO : Model) return Boolean is
      pragma Unreferenced (IO);
   begin
      return False;
   end Pending;
   function Admitted (G : L.Register_Geometry) return Boolean is
     (L.Domain_Valid (G.Region) and then G.Region.Space in L.Memory_Space | L.IO_Space
      and then G.Region.Length > 0 and then G.Region.Base <= M.Last_Address
      and then G.Region.Length - 1 <= M.Last_Address - G.Region.Base
      and then G.Byte_Offset < G.Region.Length
      and then AML_Decode.Integer_Value (ACPI_FADT.Transactions.Octets (G.Width)) <= G.Region.Length - G.Byte_Offset
      and then M.Can_Access (G.Region.Base + G.Byte_Offset, ACPI_FADT.Transactions.Octets (G.Width)));
   procedure Describe (IO : Model; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>);
      Accepted := IO.Owner /= AML_Identity.No_Identity and then Target.Provider = IO.Owner
        and then Target.Endpoint = 1 and then Target.Epoch = 1
        and then Target.Register in 1 .. A.Register_ID (IO.Used);
      if Accepted then Geometry := IO.Saved (Positive (Target.Register)); Accepted := Admitted (Geometry); end if;
   end Describe;
   procedure Dispatch (IO : in out Model; Item : A.Request; Reply : out A.Response) is
      G : L.Register_Geometry; OK : Boolean; Value : M.Number;
   begin
      Reply := (others => <>); Describe (IO, Item.Destination, G, OK);
      if not OK then return; end if;
      if Item.For_Write then
         if G.Region.Space = L.Memory_Space then
            M.Write (IO.Memory, G.Region.Base + G.Byte_Offset,
              ACPI_FADT.Transactions.Octets (G.Width), Item.Value, OK);
         else
            M.Write (IO.Ports, G.Region.Base + G.Byte_Offset,
              ACPI_FADT.Transactions.Octets (G.Width), Item.Value, OK);
         end if;
         if not OK then return; end if;
         IO.Writes := IO.Writes + 1; Value := 0;
      else
         if G.Region.Space = L.Memory_Space then
            M.Read (IO.Memory, G.Region.Base + G.Byte_Offset,
              ACPI_FADT.Transactions.Octets (G.Width), Value, OK);
         else
            M.Read (IO.Ports, G.Region.Base + G.Byte_Offset,
              ACPI_FADT.Transactions.Octets (G.Width), Value, OK);
         end if;
         if not OK then return; end if;
         IO.Reads := IO.Reads + 1;
      end if;
      Reply := (A.Completed, IO.Owner, Value, A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : Model; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Confirmed;
   procedure Authorize (IO : Model; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      pragma Unreferenced (For_Write);
      G : L.Register_Geometry;
   begin Describe (IO, Target, G, Accepted); end Authorize;
   procedure Resolve (IO : in out Model; G : L.Register_Geometry; Target : in out A.Target; Accepted : in out Boolean) is
   begin
      Accepted := IO.Owner /= AML_Identity.No_Identity and then Admitted (G);
      if not Accepted then return; end if;
      for I in 1 .. IO.Used loop
         if IO.Saved (I) = G then Target := (IO.Owner, 1, 1, A.Register_ID (I)); return; end if;
      end loop;
      if IO.Used = IO.Saved'Length then Accepted := False; return; end if;
      IO.Used := IO.Used + 1; IO.Saved (IO.Used) := G;
      Target := (IO.Owner, 1, 1, A.Register_ID (IO.Used));
   end Resolve;
   procedure Begin_Field (IO : in out Model; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
      pragma Unreferenced (IO, For_Write);
   begin Accepted := not Requires_Lock; end Begin_Field;
   procedure End_Field (IO : in out Model; Accepted : in out Boolean) is
      pragma Unreferenced (IO, Accepted);
   begin null; end End_Field;
   package F is new C.Field_IO
     (Model, Pending, Describe, Dispatch, Confirmed, Maximum_Bindings => 128,
      Authorize_Target => Authorize, Resolve_Module_Target => Resolve,
      Begin_Field => Begin_Field, End_Field => End_Field, Enable_Startup_Platform_Resolution => True);

   IO : Model;
   Bindings : F.Binding_Set (128);
   Identity_Issued : Boolean;
   Field_Members : C.Namespace.Field_Initialization_Report;
   package V renames C.Values; package N renames C.Namespace;
   use type C.Stage_Status, C.Freeze_Status, V.Access_Status, V.Module_Outcome;
   use type V.Value_Handle, V.Value_Kind;
   type Service_Access is access C.State;
   Owner : constant Service_Access := new C.State
     (Catalog_Count, Table'Length + (if Needs_Empty_DSDT then Empty_DSDT'Length else 0),
      Table'Length, Catalog_Count + Owner_Copied_Tables, Value_Object_Capacity => Objects,
      Value_Byte_Capacity => Bytes, Value_Element_Capacity => Elements);
   Stage : C.Stage_Status; Frozen : C.Freeze_Status; Access_Result : V.Access_Status;
   Execution : C.Execution_Report;
   Node : N.Node_ID; Result : AML_Execute.Execution_Result;
   Errors, Records, Passed_Records, Skipped_Records, Blocked_Records : AML_Decode.Integer_Value := 0;
   Finalized : Boolean := False;
   function Escape (Text : String) return String is
      Copy : String := Text;
   begin
      for I in Copy'Range loop
         if Copy (I) = '"' or else Copy (I) = '\' or else Character'Pos (Copy (I)) < 32 then Copy (I) := ' '; end if;
      end loop;
      return Copy;
   end Escape;
   procedure Emit (Status, Reason : String; Execution_Status : AML_Execute.Execution_Status := AML_Execute.No_Return) is
      M : constant C.Metrics := C.Observe (Owner.all);
   begin
      Finalized := True;
      Ada.Text_IO.Put_Line ("ASLTS_COPY_PROBE tables=" & Retention.Tables (Owner.all)'Image &
        " bytes=" & Retention.Bytes (Owner.all)'Image & " table-cap=" & Owner_Copied_Tables'Image &
        " byte-cap=" & Owner_Copied_Bytes'Image);
      Ada.Text_IO.Put_Line ("ASLTS_MODEL name=finite-memory-io-v1 bytes-per-space=65536 handles=128 reads=" &
        IO.Reads'Image & " writes=" & IO.Writes'Image);
      Ada.Text_IO.Put_Line ("ASLTS_RESULT {""status"":""" & Status & """,""reason"":""" & Escape (Reason) &
        """,""execution"":""" & Execution_Status'Image & """,""assertions_failed"":" & Errors'Image &
        ",""records"":" & Records'Image & ",""passed_records"":" & Passed_Records'Image &
        ",""skipped_records"":" & Skipped_Records'Image & ",""blocked_records"":" & Blocked_Records'Image &
        ",""last_observed_opcode"":" & Natural (Last_Event.Opcode)'Image &
        ",""last_observed_extension"":" & Natural (Last_Event.Extension)'Image &
        ",""has_extension"":" & (if Last_Event.Has_Extension then "true" else "false") &
        ",""last_observed_position"":" & Natural (Last_Event.Position)'Image &
        ",""profile_visits"":" & Event_Count'Image & ",""value_objects"":" & M.Value_Objects'Image &
        ",""value_bytes"":" & M.Value_Bytes'Image & ",""package_elements"":" & M.Package_Elements'Image & "}");
   end Emit;
   procedure Execution_Failure (At_Phase : String; Status : AML_Execute.Execution_Status) is
   begin
      if Status = AML_Execute.Budget_Exceeded then Emit ("TIMEOUT", At_Phase & " fuel limit", Status);
      else Emit ("UNSUPPORTED", At_Phase & " typed execution refusal; opcode is last passive visit, not a causal proof", Status); end if;
   end Execution_Failure;
   function Find (Name : String) return N.Node_ID is
      Found : N.Node_ID;
   begin C.Find_Child (Owner.all, N.Root, Name, Found, Access_Result); return Found; end Find;
   procedure Integer_Observation (Name : String; Value : out AML_Decode.Integer_Value; OK : out Boolean) is
      Item : V.Result;
   begin
      Value := 0; OK := False; Node := Find (Name);
      if Access_Result /= V.Available or else Node = N.Root then return; end if;
      C.Observe_Named_Value (Owner.all, Node, Item, Access_Result);
      OK := Access_Result = V.Available and then Item.Status = AML_Execute.Returned;
      if OK then Value := Item.Number; end if;
   end Integer_Observation;
   procedure Check_Records (OK : out Boolean) is
      Item : V.Result; Parent, Child : V.Value_Handle := V.No_Value;
      Description : V.Value_Description; Released : V.Access_Status;
      Count_Limit : constant := 65_536;
      procedure Drop (Handle : in out V.Value_Handle) is
      begin if Handle /= V.No_Value then C.Release_Result (Owner.all, Handle, Released); Handle := V.No_Value; end if; end Drop;
   begin
      OK := False; Node := Find ("RP0P");
      if Access_Result /= V.Available or else Node = N.Root or else Records = 0 or else Records > Count_Limit then return; end if;
      C.Observe_Named_Value (Owner.all, Node, Item, Access_Result);
      if Access_Result /= V.Available or else Item.Status /= AML_Execute.Object_Returned then return; end if;
      Parent := Item.Handle; C.Describe_Result (Owner.all, Parent, Description, Access_Result);
      if Access_Result /= V.Available or else Description.Kind /= V.Package_Description
        or else AML_Decode.Integer_Value (Description.Length) < Records then Drop (Parent); return; end if;
      for Index in 0 .. Natural (Records) - 1 loop
         C.Read_Result_Element (Owner.all, Parent, Index, Child, Access_Result);
         if Access_Result /= V.Available then Drop (Child); Drop (Parent); return; end if;
         C.Describe_Result (Owner.all, Child, Description, Access_Result);
         if Access_Result /= V.Available or else Description.Kind /= V.String_Description or else Description.Length not in 1 .. 4096 then
            Drop (Child); Drop (Parent); return;
         end if;
         declare Data : AML_Decode.Bytes (1 .. Description.Length); Copied : Natural; Text : String (Data'Range); begin
            C.Read_Result_Bytes (Owner.all, Child, 0, Data, Copied, Access_Result);
            if Access_Result /= V.Available or else Copied /= Data'Length then Drop (Child); Drop (Parent); return; end if;
            for I in Data'Range loop Text (I) := Character'Val (Data (I)); end loop;
            if Ada.Strings.Fixed.Index (Text, ":STST:") /= 1 then Drop (Child); Drop (Parent); return; end if;
            if Ada.Strings.Fixed.Index (Text, ":PASS:") > 0 then Passed_Records := Passed_Records + 1;
            elsif Ada.Strings.Fixed.Index (Text, ":SKIPPED:") > 0 then Skipped_Records := Skipped_Records + 1;
            elsif Ada.Strings.Fixed.Index (Text, ":BLOCKED:") > 0 then Blocked_Records := Blocked_Records + 1;
            elsif Ada.Strings.Fixed.Index (Text, ":FAIL:") = 0 then Drop (Child); Drop (Parent); return; end if;
         end;
         Drop (Child);
      end loop;
      Drop (Parent); OK := C.Retained_Results (Owner.all) = 0;
   exception when others => Drop (Child); Drop (Parent); raise;
   end Check_Records;
   Known, Complete : Boolean;
begin
   if Bundle_Count = 0 then Emit ("UNSUPPORTED", "stage invalid AML table bundle"); return; end if;
   AML_Identity.Issuer.Issue (IO.Owner, Identity_Issued);
   if not Identity_Issued then Emit ("BLOCKED", "simulated provider identity exhausted"); return; end if;
   F.Clear (Bindings);
   declare
      Offset : Natural := 0;
      ID : Positive := 1;
   begin
      if Needs_Empty_DSDT then
         C.Stage_Table (Owner.all, ID, C.DSDT, Empty_DSDT, Stage);
         if Stage /= C.Staged then Emit ("UNSUPPORTED", "empty DSDT stage " & Stage'Image); return; end if;
         ID := ID + 1;
      end if;
      for Index in 1 .. Bundle_Count loop
         declare
            Signature : constant Firmware_Tables.Signature := Signature_At (Offset);
            Header : constant Firmware_Tables.Table_Result := Firmware_Tables.Read_Table
              (Table (Table'First + Offset .. Table'Last), Signature);
         begin
            if Header.Status /= Firmware_Tables.Accepted then
               Emit ("UNSUPPORTED", "stage bundle header changed"); return;
            end if;
            C.Stage_Table (Owner.all, ID, (if Signature = "DSDT" then C.DSDT else C.SSDT),
              Table (Table'First + Offset .. Table'First + Offset + Header.Extent - 1), Stage);
            if Stage /= C.Staged then Emit ("UNSUPPORTED", "stage " & Stage'Image); return; end if;
            Offset := Offset + Header.Extent;
            ID := ID + 1;
         end;
      end loop;
   end;
   C.Freeze_Catalog (Owner.all, Frozen);
   if Frozen /= C.Catalog_Frozen_OK then Emit ("BLOCKED", "harness freeze " & Frozen'Image); return; end if;
   F.Execute_Frozen (Owner.all, IO, F.Default_Module_Target_Set'(others => A.No_Target), 0, Fuel, Execution, Access_Result);
   if Access_Result /= V.Available or else Execution.Modules.Outcome /= V.Modules_Completed then
      Execution_Failure ("module", Execution.Modules.Failure); return;
   end if;
   F.Initialize_Members (Owner.all, IO, F.Default_Module_Target_Set'(others => A.No_Target), 0, Fuel, Field_Members, Access_Result);
   if Access_Result /= V.Available then Emit ("UNSUPPORTED", "member initialization " & Access_Result'Image); return; end if;
   Node := Find ("MN00");
   if Access_Result /= V.Available or else Node = N.Root then Emit ("BLOCKED", "harness has no admitted nonstandard entry/report contract"); return; end if;
   F.Invoke_Scalar (Owner.all, IO, Bindings, Node, [others => 0], 0, Fuel, Result);
   if Access_Result /= V.Available or else Result.Status /= AML_Execute.Returned then
      Execution_Failure ("MN00", Result.Status); return;
   end if;
   Integer_Observation ("ERRS", Errors, Known);
   if not Known then Emit ("BLOCKED", "harness report contract missing ERRS"); return; end if;
   if Errors /= 0 or else Result.Value /= 0 then Emit ("FAIL", "original assertion errors or MN00 return", Result.Status); return; end if;
   Integer_Observation ("RMRC", Records, Known);
   if not Known then Emit ("BLOCKED", "harness report contract missing RMRC"); return; end if;
   Check_Records (Complete);
   if not Complete then Emit ("BLOCKED", "harness cannot establish complete original status records");
   elsif Skipped_Records /= 0 or else Blocked_Records /= 0 then Emit ("BLOCKED", "upstream skipped/blocked records retained");
   elsif Passed_Records /= Records then Emit ("FAIL", "original non-PASS records");
   else Emit ("PASS", "original MN00/ERRS zero and all status records PASS", Result.Status); end if;
exception
   when E : others =>
      if not Finalized then
         Ada.Text_IO.Put_Line ("ASLTS_RESULT {""status"":""BLOCKED"",""reason"":""harness exception " &
           Escape (Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E)) & """,""assertions_failed"":null}");
      end if;
end ASLTS_Corpus_Runner;
