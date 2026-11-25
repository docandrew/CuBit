pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity.Issuer;
with AML_Storage_Capacities;
with Firmware_Tables;
procedure Module_Name_Recovery_Runner is
   procedure Deny_Delay (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
   begin Result := AML_Delays.Unavailable; end Deny_Delay;
   -- Keep the qualification's namespace/method/fuel limits. Every register
   -- below is an in-process synthetic cell; no address is dereferenced.
   package Core is new ACPI_Service_Core
     (Perform_Delay => Deny_Delay,
      Namespace_Node_Capacity => 16384, Aggregate_Method_Capacity => 1048576,
      Enable_PCI_Context => True);
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   use type A.Endpoint_ID, AML_Identity.Identity, L.Register_Geometry;
   use type AML_Decode.Integer_Value, AML_Decode.Byte;
   use type Core.Values.Access_Status, Core.Values.Value_Handle;
   use type Core.Stage_Status, Core.Freeze_Status;
   use type AML_Execute.Execution_Status;
   Maximum_Targets : constant Positive := 8192;
   type Geometries is array (Positive range 1 .. Maximum_Targets) of L.Register_Geometry;
   type Cells is array (Positive range 1 .. Maximum_Targets) of AML_Decode.Integer_Value;
   type Model is record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Geometry : Geometries;
      Value : Cells := [others => 0];
      Used, Reads, Writes : Natural := 0;
   end record;
   IO : Model;
   Issued : Boolean;
   function Pending (Context : Model) return Boolean is
      pragma Unreferenced (Context);
   begin return False; end Pending;
   procedure Describe (Context : Model; Target : A.Target;
     Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>);
      Accepted := Target.Provider = Context.Owner and then Target.Endpoint = 1
        and then Target.Epoch = 1 and then Target.Register in 1 .. A.Register_ID (Context.Used);
      if Accepted then Geometry := Context.Geometry (Positive (Target.Register)); end if;
   end Describe;
   procedure Dispatch (Context : in out Model; Item : A.Request; Reply : out A.Response) is
      Geometry : L.Register_Geometry;
      Accepted : Boolean;
   begin
      Reply := (others => <>);
      Describe (Context, Item.Destination, Geometry, Accepted);
      if not Accepted then return; end if;
      if Item.For_Write then
         Context.Writes := Context.Writes + 1;
         Context.Value (Positive (Item.Destination.Register)) := Item.Value;
         Reply := (A.Completed, Context.Owner, 0, A.No_Operation);
      else
         Context.Reads := Context.Reads + 1;
         Reply := (A.Completed, Context.Owner,
           Context.Value (Positive (Item.Destination.Register)), A.No_Operation);
      end if;
   end Dispatch;
   procedure Confirmed (Context : Model; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (Context, Expected);
   begin Accepted := False; end Confirmed;
   procedure Authorize (Context : Model; Target : A.Target;
     For_Write : Boolean; Accepted : out Boolean) is
      pragma Unreferenced (For_Write);
      Geometry : L.Register_Geometry;
   begin Describe (Context, Target, Geometry, Accepted); end Authorize;
   procedure Resolve (Context : in out Model; Geometry : L.Register_Geometry;
     Target : in out A.Target; Accepted : in out Boolean) is
   begin
      Accepted := L.Domain_Valid (Geometry.Region)
        and then Geometry.Region.Space = L.Memory_Space
        and then not Geometry.Region.PCI.Known
        and then Geometry.Region.Base in 16#100# | 16#1000# | 16#2000# | 16#2100#
        and then Geometry.Region.Length in 1 .. 4
        and then Geometry.Byte_Offset < Geometry.Region.Length;
      if not Accepted then return; end if;
      for I in 1 .. Context.Used loop
         if Context.Geometry (I) = Geometry then
            Target := (Context.Owner, 1, 1, A.Register_ID (I)); return;
         end if;
      end loop;
      if Context.Used = Maximum_Targets then Accepted := False; return; end if;
      Context.Used := Context.Used + 1;
      Context.Geometry (Context.Used) := Geometry;
      Target := (Context.Owner, 1, 1, A.Register_ID (Context.Used));
   end Resolve;
   procedure Begin_Field (Context : in out Model; Requires_Lock, For_Write : Boolean;
     Accepted : in out Boolean) is
      pragma Unreferenced (Context, Requires_Lock, For_Write);
   begin Accepted := True; end Begin_Field;
   procedure End_Field (Context : in out Model; Accepted : in out Boolean) is
      pragma Unreferenced (Context);
   begin Accepted := True; end End_Field;
   package Fields is new Core.Field_IO (Model, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize, Resolve_Module_Target => Resolve,
     Begin_Field => Begin_Field, End_Field => End_Field,
     Allow_Locked_NVS_Reads => True, Allow_Locked_Fields => True,
     Enable_Startup_Platform_Resolution => True);
   Bindings : Fields.Binding_Set (1);
   function Read_Table (Name : String) return Firmware_Tables.Bytes is
      package S renames Ada.Streams.Stream_IO;
      File : S.File_Type;
      use type S.Count, Ada.Streams.Stream_Element_Offset;
   begin
      S.Open (File, S.In_File, Name);
      declare Size : constant S.Count := S.Size (File); begin
         if Size < 36 or else Size > 16 * 1024 * 1024 then
            raise Program_Error with "table extent outside harness bound";
         end if;
         declare
            Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Size));
            Last : Ada.Streams.Stream_Element_Offset;
            Data : Firmware_Tables.Bytes (1 .. Natural (Size));
         begin
            S.Read (File, Raw, Last); S.Close (File);
            if Last /= Raw'Last then raise Program_Error with "short input"; end if;
            for I in Data'Range loop
               Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I)));
            end loop;
            return Data;
         end;
      end;
   end Read_Table;
   Count : constant Positive := Ada.Command_Line.Argument_Count;
   Total : Natural := 0;
   Largest : Positive := 1;
begin
   if Count /= 3 then raise Program_Error with "three synthetic table paths required"; end if;
   AML_Identity.Issuer.Issue (IO.Owner, Issued);
   if not Issued then raise Program_Error with "synthetic identity unavailable"; end if;
   for I in 1 .. Count loop
      declare Data : constant Firmware_Tables.Bytes := Read_Table (Ada.Command_Line.Argument (I)); begin
         if Data'Length > Natural'Last - Total then raise Program_Error with "input extent"; end if;
         Total := Total + Data'Length; Largest := Positive'Max (Largest, Data'Length);
      end;
   end loop;
   if not Core.Storage_Input_Supported (Total) then raise Program_Error with "input capacity"; end if;
   declare
      Limits : constant AML_Storage_Capacities.Limits := Core.Initial_Storage_Limits (Total);
      subtype Sized_State is Core.State (Count, Total, Largest, Count,
        Limits.Objects, Limits.Bytes, Limits.Elements);
      type State_Access is access Sized_State;
      Service : constant State_Access := new Sized_State;
      Staged : Core.Stage_Status;
      Frozen : Core.Freeze_Status;
      Report : Core.Execution_Report;
      Status, Released : Core.Values.Access_Status;
      Members : Core.Namespace.Initialization_Report;
      Item : Core.Node_Descriptor;
      Result : Core.Values.Result;
      Found : Boolean := False;
   begin
      for I in 1 .. Count loop
         declare Data : constant Firmware_Tables.Bytes := Read_Table (Ada.Command_Line.Argument (I)); begin
            Core.Stage_Table (Service.all, 99 + I, (if I = 1 then Core.DSDT else Core.SSDT), Data, Staged);
            if Staged /= Core.Staged then raise Program_Error with "table staging refused"; end if;
         end;
      end loop;
      Core.Freeze_Catalog (Service.all, Frozen);
      if Frozen /= Core.Catalog_Frozen_OK then raise Program_Error with "catalog freeze refused"; end if;
      Fields.Execute_Frozen (Service.all, IO,
        Fields.Default_Module_Target_Set'(others => A.No_Target), 0, 100_000, Report, Status);
      Ada.Text_IO.Put_Line ("MODULE outcome=" & Report.Modules.Outcome'Image
        & " failure=" & Report.Modules.Failure'Image & " completed=" & Report.Modules.Completed'Image);
      declare Errors : constant Core.Module_Error_Observation := Core.Module_Errors (Service.all); begin
         Ada.Text_IO.Put_Line ("RECOVERY count=" & Errors.Count'Image & " kind=" & Errors.Kind'Image);
      end;
      Core.Initialize_Members (Service.all, Members, Status);
      if Status = Core.Values.Available then
         for Node in Core.Namespace.Node_ID range Core.Namespace.Root .. Core.Namespace_Node_Count (Service.all) loop
            Core.Describe_Node (Service.all, Node, Item, Status);
            if Status = Core.Values.Available and then Core.Values.Node_Name (Item) = "_LID" then
               if Found then raise Program_Error with "duplicate synthetic probe"; end if;
               Found := True;
               Fields.Evaluate_Named (Service.all, IO, Bindings, Item, 100_000, Result, Status);
               Ada.Text_IO.Put_Line ("PROBE outcome=" & Result.Status'Image & " access=" & Status'Image);
               if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
                 and then Result.Handle /= Core.Values.No_Value
               then
                  Core.Release_Result (Service.all, Result.Handle, Released);
                  if Released /= Core.Values.Available then raise Program_Error with "result release"; end if;
               end if;
            end if;
         end loop;
      end if;
      Ada.Text_IO.Put_Line ("MODEL reads=" & IO.Reads'Image & " writes=" & IO.Writes'Image);
   end;
end Module_Name_Recovery_Runner;
