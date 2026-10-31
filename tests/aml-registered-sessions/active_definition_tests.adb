pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_FADT.Transactions;
with AML_Identity;
with AML_Identity.Issuer;
procedure Active_Definition_Tests is
   package C renames ACPI_Service;
   package V renames C.Values;
   package N renames C.Namespace;
   use type Ada.Streams.Stream_Element_Offset;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type V.Access_Status, V.Module_Outcome;
   use type C.Stage_Status, C.Freeze_Status, C.Retained_Table_Phase, C.Import_Phase;
   use type N.Definition_Identity;
   package A renames AML_Field_Access;
   use type AML_Identity.Identity, A.Register_ID, A.Endpoint_ID;
   Named_Register : constant A.Register_ID := 16#F000_0000_0000_0041#;
   Named_Endpoint : constant A.Endpoint_ID := 7;
   type IO_Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Calls : Natural := 0;
   end record;
   function Pending (IO : IO_Context) return Boolean is
      pragma Unreferenced (IO);
   begin return False; end Pending;
   procedure Describe (IO : IO_Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 1),
        Byte_Offset => 0, Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := Target.Provider = IO.Owner and then Target.Endpoint = Named_Endpoint
        and then Target.Epoch = 1 and then Target.Register = Named_Register;
   end Describe;
   procedure Authorize (IO : IO_Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin Describe (IO, Target, Geometry, Accepted); Accepted := Accepted and then not For_Write; end Authorize;
   procedure Dispatch (IO : in out IO_Context; Item : A.Request; Reply : out A.Response) is
      Accepted : Boolean;
   begin
      Reply := (others => <>); Authorize (IO, Item.Destination, Item.For_Write, Accepted);
      if not Accepted then return; end if;
      IO.Calls := IO.Calls + 1; Reply := (A.Completed, IO.Owner, 42, A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : IO_Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Confirmed;
   package F is new C.Field_IO (IO_Context, Pending, Describe, Dispatch, Confirmed,
      Authorize_Target => Authorize, Maximum_Bindings => 1);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Read_Table (Argument : Positive) return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Data : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Result : Firmware_Tables.Bytes (1 .. Data'Length);
      begin
         IO.Read (File, Data, Last); IO.Close (File); Check (Last = Data'Last, "whole table");
         for I in Result'Range loop Result (I) := Firmware_Tables.Byte (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Result;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table (1);
   Parent : constant Firmware_Tables.Bytes := Read_Table (2);
   Child : constant Firmware_Tables.Bytes := Read_Table (3);
   Capacity : constant Positive := Boot'Length + 2 * Parent'Length + Child'Length;
   Limit : constant Positive := Positive'Max (Boot'Length, Positive'Max (Parent'Length, Child'Length));
   Mode : constant String := Ada.Command_Line.Argument (4);
   Field_Mode : constant Boolean := Mode in "field-direct" | "field-named" | "field-single" | "field-unbound";
   Named_Mode : constant Boolean := Mode in "named" | "field-named";
   Service : aliased C.State (4, Capacity, Limit, 4);
   IO : IO_Context;
   Bindings : F.Binding_Set;
   Single : F.Binding;

   Status : V.Access_Status;
   Result : V.Result;
   Args : V.Arguments := [others => (V.Immediate_Argument, 0)];
   Node, Destination, Before_Node, After_Node, Child_Node : N.Node_ID;
   Descriptor : C.Node_Descriptor;
   Boot_ID, Parent_ID, Child_ID, Observed : N.Definition_Identity;
   procedure Call_Start (Fuel : Natural) is
   begin
      if Mode = "named" then C.Evaluate_Named (Service, Descriptor, Fuel, Result, Status);
      elsif Mode = "field-named" then F.Evaluate_Named (Service, IO, Bindings, Descriptor, Fuel, Result, Status);
      elsif Mode = "field-single" then F.Invoke_With_Arguments (Service, IO, Single, Node, Args, 1, Fuel, Result, Status);
      elsif Field_Mode then F.Invoke_With_Arguments (Service, IO, Bindings, Node, Args, 1, Fuel, Result, Status);
      else C.Invoke_With_Arguments (Service, Node, Args, 1, Fuel, Result, Status); end if;
   end Call_Start;
   procedure Find (Scope : N.Node_ID; Name : String; Item : out N.Node_ID) is
   begin
      C.Find_Child (Service, Scope, Name, Item, Status);
      Check (Status = V.Available and then Item /= N.Root, "find " & Name);
   end Find;
   procedure Value_Is (Scope : N.Node_ID; Name : String; Expected : AML_Decode.Integer_Value) is
      Item : N.Node_ID;
   begin
      Find (Scope, Name, Item); C.Observe_Named_Value (Service, Item, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned and then Result.Number = Expected,
        "value " & Name);
   end Value_Is;
   procedure Definition_Is (Item : N.Node_ID; Expected : N.Definition_Identity) is
   begin
      C.Describe_Node (Service, Item, Descriptor, Status); Check (Status = V.Available, "describe definition");
      C.Describe_Definition (Service, Descriptor, Observed, Status);
      Check (Status = V.Available and then Observed = Expected, "definition restored across nested module");
   end Definition_Is;
begin
   declare
      Stage : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Initial : N.Initialization_Report;
   begin
      C.Stage_Table (Service, 11, C.DSDT, Boot, Stage); Check (Stage = C.Staged, "stage DSDT");
      C.Stage_Table (Service, 22, C.Description, Parent, Stage); Check (Stage = C.Staged, "stage parent");
      C.Stage_Table (Service, 33, C.Description, Child, Stage); Check (Stage = C.Staged, "stage child");
      C.Stage_Table (Service, 44, C.Description, Parent, Stage); Check (Stage = C.Staged, "stage duplicate bytes");
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (Service, 100000, Execution, Status);
      Check (Status = V.Available and then Execution.Modules.Outcome = V.Modules_Completed, "boot definitions");
      C.Initialize_Members (Service, Initial, Status); Check (Status = V.Available, "ready");
   end;
   if Field_Mode then
      declare
         OK : Boolean;
         Field_Node, Enable_Node : N.Node_ID;
         Key : V.Field_Key;
         Field : V.Field_Description;
         Enable : C.Node_Descriptor;
         Charged : Natural;
         Store_Outcome : AML_Execute.Execution_Status;
      begin
         AML_Identity.Issuer.Issue (IO.Owner, OK); Check (OK, "synthetic provider identity");
         Find (N.Root, "ENIO", Enable_Node); C.Describe_Node (Service, Enable_Node, Enable, Status);
         C.Store_Parameter_Data (Service, Enable, (V.Immediate_Argument, 1), 100, Charged, Store_Outcome, Status);
         Check (Status = V.Available and then Store_Outcome = AML_Execute.Returned, "enable module field reads");
         if Mode /= "field-unbound" then
            Find (N.Root, "FLD0", Field_Node); C.Read_Field_Descriptor (Service, Field_Node, Field, Key, Status);
            Check (Status = V.Available, "existing field descriptor");
            F.Add_Binding (Service, IO, Key, (IO.Owner, Named_Endpoint, 1, Named_Register), Bindings, Status);
            Check (Status = V.Available, "explicit read binding");
            F.Bind (Service, IO, Key, (IO.Owner, Named_Endpoint, 1, Named_Register), Single, Status);
            Check (Status = V.Available, "explicit single binding");
         end if;
      end;
   end if;
   Find (N.Root, (if Named_Mode then "EVAL" else "GO00"), Node); C.Describe_Node (Service, Node, Descriptor, Status);
   C.Describe_Definition (Service, Descriptor, Boot_ID, Status); Check (Status = V.Available and then N.Has_Definition (Boot_ID), "boot method provenance");
   Args (0).Number := 77;
   Call_Start (100);
   Check (Result.Status = AML_Execute.Budget_Exceeded, "pre-effect load budget refusal");
   Check (C.Retained_Load_State (Service, 2) = C.Retained and then C.Retained_Definition (Service, 2) = N.No_Definition,
     "budget refusal does not consume parent");
   Check (C.Current_Import_Phase (Service) = C.Catalog_Executed, "pre-effect failure keeps owner usable");
   Call_Start (100000);
   if Mode = "field-unbound" then
      Check (Result.Status in AML_Execute.Unsupported_Value | AML_Execute.Unsupported, "unbound field refused during module");
      Check (IO.Calls = 0, "no transport dispatch without explicit binding");
      Check (C.Retained_Load_State (Service, 2) = C.Failed_Definition
        and then C.Retained_Load_State (Service, 3) = C.Retained, "consumed parent closed; child never executed");
      Check (N.Has_Definition (C.Retained_Definition (Service, 2)) and then C.Retained_Definition (Service, 3) = N.No_Definition,
        "failed parent retains provenance; child unconsumed");
      Check (C.Current_Import_Phase (Service) = C.Catalog_Failed, "unbound field failure closes serving");
      Value_Is (N.Root, "ORD0", 1);
      Ada.Text_IO.Put_Line ("Active definition checks" & Natural'Image (Checks)); return;
   end if;
   if Field_Mode then Check (IO.Calls = 2, "same IO context reaches parent and child module reads");
   else Check (IO.Calls = 0, "ordinary methods do not dispatch fields"); end if;
   if Status /= V.Available or else Result.Status /= AML_Execute.Returned
     or else (Result.Status = AML_Execute.Returned and then Result.Number /= 11) then
      Ada.Text_IO.Put_Line ("observed access=" & Status'Image & " result=" & Result.Status'Image &
        " calls=" & IO.Calls'Image & " parent=" & C.Retained_Load_State (Service, 2)'Image &
        " child=" & C.Retained_Load_State (Service, 3)'Image);
      if Result.Status = AML_Execute.Returned then Ada.Text_IO.Put_Line ("observed number=" & Result.Number'Image); end if;
   end if;
   Check (Status = V.Available and then Result.Status = AML_Execute.Returned and then Result.Number = 11,
     "real LoadTable preserves caller local across nested modules");
   if Field_Mode then Value_Is (N.Root, "FVAL", 42); end if;
   Check (C.Retained_Load_State (Service, 2) = C.Loaded_Definition and then C.Retained_Load_State (Service, 3) = C.Loaded_Definition,
     "both actual modules share authoritative load registry");
   Parent_ID := C.Retained_Definition (Service, 2); Child_ID := C.Retained_Definition (Service, 3);
   Check (N.Has_Definition (Parent_ID) and then N.Has_Definition (Child_ID) and then Parent_ID /= Child_ID
     and then Parent_ID /= Boot_ID and then Child_ID /= Boot_ID, "independent declaration identities");
   Check (C.Retained_Load_State (Service, 4) = C.Retained and then C.Retained_Definition (Service, 4) = N.No_Definition,
     "alternate ID was never consumed");
   Value_Is (N.Root, "ORD0", 123);
   Find (N.Root, "DST0", Destination);
   Value_Is (Destination, "PRM0", 77);
   Find (Destination, "BEF0", Before_Node); Find (Destination, "AFT0", After_Node); Find (Destination, "CHLD", Child_Node);
   Definition_Is (Before_Node, Parent_ID); Definition_Is (After_Node, Parent_ID); Definition_Is (Child_Node, Child_ID);
   Find (N.Root, "DUP0", Node); C.Invoke_With_Arguments (Service, Node, Args, 0, 100000, Result, Status);
   Check (Result.Status = AML_Execute.Duplicate_Name, "duplicate bytecode load is an error");
   Value_Is (N.Root, "ORD0", 123);
   Check (C.Current_Import_Phase (Service) = C.Catalog_Executed, "duplicate refusal is pre-effect");
   Find (N.Root, "MISS", Node); C.Invoke_With_Arguments (Service, Node, Args, 0, 100000, Result, Status);
   Check (Status = V.Available and then Result.Status = AML_Execute.Returned and then Result.Number = 0,
     "missing table returns false before parsing malformed root path");
   Ada.Text_IO.Put_Line ("Active definition checks" & Natural'Image (Checks));
end Active_Definition_Tests;
