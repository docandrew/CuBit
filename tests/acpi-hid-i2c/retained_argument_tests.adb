pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Objects;
with Firmware_Tables;
procedure Retained_Argument_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Aggregate_Method_Capacity => 4096,
      Retained_Result_Capacity => 8);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Values.Value_Kind;
   use type C.Values.Value_Handle;
   use type C.Values.Audit_Value;
   use type AML_Field_Access.Target;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Byte;
   Routine_Budget : constant Positive := 1_000;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
   Service, Other : aliased C.State (1, 4096, 4096);
   type IO_Context is record
      Is_Pending : Boolean := False;
      Calls : Natural := 0;
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
   end record;
   IO : IO_Context;
   function Pending (Context : IO_Context) return Boolean is (Context.Is_Pending);
   procedure Describe (Context : IO_Context; Target : AML_Field_Access.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 1), others => <>);
      Accepted := not Context.Is_Pending and then
        Target = AML_Field_Access.Target'(Context.Owner, 7, 1, 1);
   end Describe;
   procedure Dispatch (Context : in out IO_Context; Item : AML_Field_Access.Request;
     Reply : out AML_Field_Access.Response) is
      pragma Unreferenced (Item);
   begin Context.Calls := Context.Calls + 1; Reply := (others => <>); end Dispatch;
   procedure Confirm (Context : IO_Context; Item : AML_Field_Access.Observation;
     Accepted : out Boolean) is
      pragma Unreferenced (Context, Item);
   begin Accepted := False; end Confirm;
   procedure Authorize (Context : IO_Context; Target : AML_Field_Access.Target;
     For_Write : Boolean; Accepted : out Boolean) is
      pragma Unreferenced (For_Write);
   begin
      Accepted := not Context.Is_Pending and then
        Target = AML_Field_Access.Target'(Context.Owner, 7, 1, 1);
   end Authorize;
   package F is new C.Field_IO
     (IO_Context, Pending, Describe, Dispatch, Confirm, Authorize_Target => Authorize);
   Bindings : F.Binding_Set;
   One_Binding : F.Binding;
   Args : C.Values.Arguments := [others => (C.Values.Immediate_Argument, 0)];
   R : C.Values.Result;
   Status : C.Values.Access_Status;
   UUID, Package_Value, Buffer_Value, Foreign, Saved, Returned : C.Values.Value_Handle;
   DSM_Node, Echo_Node, Mutate_Node, Scalar_Node : C.Node_Descriptor;
   function Find (S : C.State; Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Access_Result : C.Values.Access_Status;
   begin
      C.Find_Child (S, C.Namespace.Root, Name, Node, Access_Result);
      Check (Access_Result = C.Values.Available, "find " & Name);
      C.Describe_Node (S, Node, Item, Access_Result);
      Check (Access_Result = C.Values.Available, "describe " & Name);
      return Item;
   end Find;
   function Retain (S : aliased in out C.State; Name : String) return C.Values.Value_Handle is
      Value : C.Values.Result;
      Access_Result : C.Values.Access_Status;
   begin
      C.Evaluate_Named (S, Find (S, Name), Routine_Budget, Value, Access_Result);
      Check (Access_Result = C.Values.Available and then Value.Status = AML_Execute.Object_Returned,
        "retain " & Name);
      return Value.Handle;
   end Retain;
   procedure Release (S : in out C.State; Handle : in out C.Values.Value_Handle) is
      Access_Result : C.Values.Access_Status;
   begin
      C.Release_Result (S, Handle, Access_Result);
      Check (Access_Result = C.Values.Available and then Handle = C.Values.No_Value, "release");
   end Release;
   procedure Expect_Number (Number : AML_Decode.Integer_Value; Roots : Natural := 3) is
   begin
      Check (Status = C.Values.Available and then R.Status = AML_Execute.Returned
        and then R.Number = Number and then R.Charged <= Routine_Budget, "integer result");
      Check (C.Retained_Results (Service) = Roots, "argument roots retained");
   end Expect_Number;
   procedure Prepare (S : aliased in out C.State; Data : Firmware_Tables.Bytes) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Access_Result : C.Values.Access_Status;
      Members : C.Namespace.Initialization_Report;
   begin
      C.Stage_Table (S, 1, C.DSDT, Data, Staged);
      Check (Staged = C.Staged, "stage");
      C.Freeze_Catalog (S, Frozen);
      Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (S, Routine_Budget, Execution, Access_Result);
      Check (Access_Result = C.Values.Available
        and then Execution.Modules.Outcome = C.Values.Modules_Completed, "execute");
      C.Initialize_Members (S, Members, Access_Result);
      Check (Access_Result = C.Values.Available, "members");
   end Prepare;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "AML fixture path required");
   C.Create_Buffer_Result (Service, [1], R, Status);
   Check (Status = C.Values.Wrong_Phase and then R.Charged = 0
     and then C.Retained_Results (Service) = 0, "constructor phase gate");
   C.Create_Empty_Package_Result (Service, R, Status);
   Check (Status = C.Values.Wrong_Phase and then C.Retained_Results (Service) = 0, "package phase gate");
   declare
      Input : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (Input, Stream, Last);
      Ada.Streams.Stream_IO.Close (Input);
      declare
         Data : Firmware_Tables.Bytes (1 .. Natural (Last));
      begin
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Data); Prepare (Other, Data);
      end;
   end;
   UUID := Retain (Service, "UUID");
   Package_Value := Retain (Service, "PKG0");
   Buffer_Value := Retain (Service, "BUF0");
   Foreign := Retain (Other, "UUID");
   DSM_Node := Find (Service, "_DSM"); Echo_Node := Find (Service, "ECHO");
   Mutate_Node := Find (Service, "MUT0"); Scalar_Node := Find (Service, "SCAL");
   Args (0) := (C.Values.Retained_Argument, UUID);
   Args (1) := (C.Values.Immediate_Argument, 1);
   Args (2) := (C.Values.Immediate_Argument, 1);
   Args (3) := (C.Values.Retained_Argument, Package_Value);
   Args (6) := (C.Values.Retained_Argument, Foreign); -- Outside Argument_Count.
   C.Invoke_With_Arguments (Service, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
   Expect_Number (16#1234#);
   F.Clear (Bindings);
   F.Invoke_With_Arguments (Service, IO, Bindings, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
   Expect_Number (16#1234#);
   declare
      Issued : Boolean;
      Description : C.Values.Field_Description;
      Key : C.Values.Field_Key;
      Target : AML_Field_Access.Target;
   begin
      AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "test provider identity");
      C.Read_Field_Descriptor (Service, C.Values.Position (Find (Service, "FLD0")), Description, Key, Status);
      Check (Status = C.Values.Available, "named field descriptor");
      Target := (IO.Owner, 7, 1, 1);
      F.Bind (Service, IO, Key, Target, One_Binding, Status);
      Check (Status = C.Values.Available, "single named binding");
      F.Invoke_With_Arguments (Service, IO, One_Binding, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
      Expect_Number (16#1234#);
   end;
   F.Invoke_With_Arguments (Service, IO, F.No_Binding, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
   Check (Status = C.Values.Invalid_Value and then R.Charged = 0, "singleton requires valid binding");
   IO.Is_Pending := True;
   F.Invoke_With_Arguments (Service, IO, Bindings, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
   Check (R.Status = AML_Execute.Field_Indeterminate and then R.Charged = 0, "pending field gate retained");
   IO.Is_Pending := False;
   Args (2) := (C.Values.Immediate_Argument, 0);
   C.Invoke_With_Arguments (Service, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
   Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned
     and then C.Retained_Results (Service) = 4, "function query result retained");
   Returned := R.Handle;
   declare
      Data : AML_Decode.Bytes (1 .. 1);
      Copied : Natural;
   begin
      C.Read_Result_Bytes (Service, Returned, 0, Data, Copied, Status);
      Check (Status = C.Values.Available and then Copied = 1 and then Data (1) = 3, "ACPICA function mask");
   end;
   Release (Service, Returned);
   Args (0) := (C.Values.Retained_Argument, Package_Value);
   C.Invoke_With_Arguments (Service, C.Values.Position (Echo_Node), Args, 1, Routine_Budget, R, Status);
   Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned, "package argument returned");
   Returned := R.Handle;
   declare
      Description : C.Values.Value_Description;
   begin
      C.Describe_Result (Service, Returned, Description, Status);
      Check (Status = C.Values.Available and then Description.Kind = C.Values.Package_Description
        and then Description.Length = 0, "empty package preserved");
   end;
   Release (Service, Returned);
   Args (0) := (C.Values.Retained_Argument, Buffer_Value);
   C.Invoke_With_Arguments (Service, C.Values.Position (Mutate_Node), Args, 1, Routine_Budget, R, Status);
   Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned, "buffer argument mutation");
   Returned := R.Handle;
   declare
      Data : AML_Decode.Bytes (1 .. 2);
      Copied : Natural;
   begin
      C.Read_Result_Bytes (Service, Buffer_Value, 0, Data, Copied, Status);
      Check (Status = C.Values.Available and then Copied = 2
        and then Data (1) = 16#5A# and then Data (2) = 16#22#, "caller buffer remains live");
   end;
   Release (Service, Returned);
   Args (0) := (C.Values.Retained_Argument, Foreign);
   C.Invoke_With_Arguments (Service, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
   Check (Status = C.Values.Invalid_Value and then R.Charged = 0
     and then C.Retained_Results (Service) = 3, "other service handle rejected");
   Saved := UUID; Release (Service, UUID);
   Args (0) := (C.Values.Retained_Argument, Saved);
   C.Invoke_With_Arguments (Service, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
   Check (Status = C.Values.Invalid_Value and then R.Charged = 0
     and then C.Retained_Results (Service) = 2, "released handle rejected");
   UUID := Retain (Service, "UUID");
   Args (0) := (C.Values.Retained_Argument, UUID);
   Args (2) := (C.Values.Immediate_Argument, 1);
   C.Invoke_With_Arguments (Service, C.Values.Position (DSM_Node), Args, 4, 0, R, Status);
   Check (R.Status = AML_Execute.Budget_Exceeded and then R.Charged = 0
     and then C.Retained_Results (Service) = 3, "zero budget keeps arguments");
   C.Invoke_Retained (Service, C.Values.Position (Scalar_Node), [20, 22, others => 0], 2, Routine_Budget, R, Status);
   Expect_Number (42);
   F.Invoke_Retained (Service, IO, Bindings, C.Values.Position (Scalar_Node), [20, 22, others => 0], 2, Routine_Budget, R, Status);
   Expect_Number (42);
   Check (IO.Calls = 0, "no hardware transactions");
   Release (Service, UUID); Release (Service, Package_Value); Release (Service, Buffer_Value);
   Release (Other, Foreign);
   Check (C.Retained_Results (Service) = 0 and then C.Retained_Results (Other) = 0, "all results released");
   -- The protocol caller constructs inputs directly, not by naming fixture data.
   declare
      Expected_UUID : constant AML_Decode.Bytes :=
        [16#F7#, 16#F6#, 16#DF#, 16#3C#, 16#67#, 16#42#, 16#55#, 16#45#,
         16#AD#, 16#05#, 16#B3#, 16#0A#, 16#3D#, 16#89#, 16#38#, 16#DE#];
      Input_UUID : AML_Decode.Bytes (9 .. 24) := Expected_UUID;
      Filled : array (1 .. 6) of C.Values.Value_Handle;
   begin
      C.Create_Buffer_Result (Service, Input_UUID, R, Status);
      Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned
        and then R.Charged = 0, "construct UUID buffer");
      UUID := R.Handle;
      Input_UUID := [others => 0];
      C.Create_Empty_Package_Result (Service, R, Status);
      Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned
        and then R.Charged = 0, "construct empty argument package");
      Package_Value := R.Handle;
      Args := [0 => (C.Values.Retained_Argument, UUID),
        1 => (C.Values.Immediate_Argument, 1), 2 => (C.Values.Immediate_Argument, 1),
        3 => (C.Values.Retained_Argument, Package_Value), others => (C.Values.Immediate_Argument, 0)];
      C.Invoke_With_Arguments (Service, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
      Expect_Number (16#1234#, 2);
      F.Invoke_With_Arguments (Service, IO, One_Binding, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
      Expect_Number (16#1234#, 2);
      F.Invoke_With_Arguments (Service, IO, Bindings, C.Values.Position (DSM_Node), Args, 4, Routine_Budget, R, Status);
      Expect_Number (16#1234#, 2);
      C.Create_Buffer_Result (Service, [], R, Status);
      Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned, "empty buffer");
      Returned := R.Handle;
      declare
         Description : C.Values.Value_Description;
      begin
         C.Describe_Result (Service, Returned, Description, Status);
         Check (Status = C.Values.Available and then Description.Kind = C.Values.Buffer_Description
           and then Description.Length = 0, "empty buffer shape");
      end;
      Release (Service, Returned);
      for I in Filled'Range loop
         C.Create_Empty_Package_Result (Service, R, Status);
         Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned, "fill retained capacity");
         Filled (I) := R.Handle;
      end loop;
      declare
         Before : constant C.Values.Audit_Value := C.Audit (Service);
      begin
         C.Create_Buffer_Result (Service, [1], R, Status);
         Check (Status = C.Values.Root_Limit and then C.Retained_Results (Service) = 8
           and then C.Audit (Service) = Before, "root refusal cannot allocate buffer");
         C.Create_Empty_Package_Result (Service, R, Status);
         Check (Status = C.Values.Root_Limit and then C.Retained_Results (Service) = 8
           and then C.Audit (Service) = Before, "root refusal cannot allocate package");
      end;
      for I in Filled'Range loop Release (Service, Filled (I)); end loop;
      declare
         Before : constant C.Values.Audit_Value := C.Audit (Service);
         Too_Long : constant AML_Decode.Bytes (1 .. AML_Objects.Max_Bytes + 1) := [others => 0];
      begin
         C.Create_Buffer_Result (Service, Too_Long, R, Status);
         Check (Status = C.Values.Available and then R.Status = AML_Execute.Value_Limit
           and then C.Retained_Results (Service) = 2 and then C.Audit (Service) = Before,
           "byte refusal releases placeholder and leaves store unchanged");
      end;
      C.Create_Buffer_Result (Service, [16#42#], R, Status);
      Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned, "constructor usable after quota refusal");
      Returned := R.Handle; Release (Service, Returned);
      Release (Service, UUID); Release (Service, Package_Value);
      Check (C.Retained_Results (Service) = 0 and then IO.Calls = 0, "constructed inputs released without hardware calls");
   end;
   Ada.Text_IO.Put_Line ("Retained service argument checks" & Checks'Image);
end Retained_Argument_Tests;
