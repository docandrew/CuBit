pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Execution_Limits;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Names;
with Index_Width_Oracle;
procedure Index_Width_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
     Namespace_Node_Capacity => 32, Retained_Result_Capacity => 4);
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Values.Value_Kind;
   use type C.Values.Value_Handle;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Byte;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   Index_ID : constant A.Register_ID := 16#8000_0000_0000_0011#;
   Data_ID : constant A.Register_ID := 16#A000_0000_0000_0055#;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Calls, Reads, Selectors : Natural := 0;
      Last_Selector : AML_Decode.Integer_Value := 0;
      Need_Data : Boolean := False;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Owner = AML_Identity.No_Identity);
   procedure Describe (IO : Context; Target : A.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := ((Space => 0, Base => 16#1000#, Length => 16),
        (if Target.Register = Index_ID then 0 else 8),
        (if Target.Register = Index_ID then T.Qword_Access else T.Byte_Access));
      Accepted := IO.Owner /= AML_Identity.No_Identity and then Target.Provider = IO.Owner
        and then Target.Endpoint = 7 and then Target.Epoch = 1
        and then Target.Register in Index_ID | Data_ID;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; Writing : Boolean; Accepted : out Boolean) is
      G : AML_Field_Layout.Register_Geometry;
      pragma Unreferenced (Writing);
   begin
      Describe (IO, Target, G, Accepted);
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Accepted : Boolean;
   begin
      Authorize (IO, Item.Destination, Item.For_Write, Accepted);
      Check (Accepted and then Item.For_Write = (Item.Destination.Register = Index_ID), "large named target/direction");
      IO.Calls := IO.Calls + 1;
      if Item.For_Write then
         Check (not IO.Need_Data and then Item.Value = AML_Decode.Integer_Value (IO.Reads), "large ordered selector");
         IO.Last_Selector := Item.Value; IO.Selectors := IO.Selectors + 1; IO.Need_Data := True;
      else
         Check (IO.Need_Data and then Item.Value = 0, "large ordered data read");
         IO.Reads := IO.Reads + 1; IO.Need_Data := False;
      end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else 16#EF#), A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Maximum_Bindings => 2, Authorize_Target => Authorize,
     Maximum_Requests => AML_Execution_Limits.Field_Request_Limit'Last,
     Maximum_Work => AML_Execution_Limits.Field_Work_Limit'Last);
   Case_Name : constant String := Ada.Command_Line.Argument (1);
   Payload : constant Positive := (if Case_Name = "small" then 512 elsif Case_Name = "medium" then 4096
     elsif Case_Name = "large" then 16_384 else raise Program_Error with "unknown size");
   Name : constant AML_Names.Segment := (if Case_Name = "small" then "R512" elsif Case_Name = "medium" then "R4K0" else "R16K");
   Raw : constant AML_Decode.Bytes := Index_Width_Oracle.Table64;
   Service : aliased C.State (1, Raw'Length, Raw'Length);
   IO : Context;
   Items : F.Binding_Set;
   Stage : C.Stage_Status;
   Freeze : C.Freeze_Status;
   Status : C.Values.Access_Status;
   Execution : C.Execution_Report;
   Members : C.Namespace.Initialization_Report;
   Issued : Boolean;
   Node : C.Namespace.Node_ID;
   Descriptor : C.Node_Descriptor;
   Result : C.Values.Result;
   Description : C.Values.Value_Description;
   Handle : C.Values.Value_Handle;
   Data : AML_Decode.Bytes (1 .. Payload);
   Copied : Natural;
begin
   AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "large provider");
   C.Stage_Table (Service, 1, C.DSDT, Raw, Stage); Check (Stage = C.Staged, "large stage");
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "large freeze");
   C.Execute_Frozen (Service, 100_000, Execution, Status);
   Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "large module");
   C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "large members");
   F.Clear (Items);
   for Is_Index in Boolean loop
      declare
         N : C.Namespace.Node_ID; D : C.Values.Field_Description; Key : C.Values.Field_Key;
         Label : constant AML_Names.Segment := (if Is_Index then "IDX0" else "DAT0");
      begin
         C.Find_Child (Service, C.Namespace.Root, Label, N, Status);
         C.Read_Field_Descriptor (Service, N, D, Key, Status); Check (Status = C.Values.Available, "large key");
         F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, (if Is_Index then Index_ID else Data_ID)), Items, Status);
         Check (Status = C.Values.Available, "large binding");
      end;
   end loop;
   C.Find_Child (Service, C.Namespace.Root, Name, Node, Status); Check (Status = C.Values.Available, "large lookup");
   C.Describe_Node (Service, Node, Descriptor, Status); Check (Status = C.Values.Available, "large descriptor");
   F.Evaluate_Named (Service, IO, Items, Descriptor, 100_000, Result, Status);
   Check (Status = C.Values.Available and then Result.Status = AML_Execute.Object_Returned,
     "large result " & Result.Status'Image);
   Check (IO.Calls = 2 * Payload and then IO.Reads = Payload and then IO.Selectors = Payload
     and then IO.Last_Selector = AML_Decode.Integer_Value (Payload - 1) and then not IO.Need_Data, "large complete exact transfer");
   Handle := Result.Handle;
   C.Describe_Result (Service, Handle, Description, Status);
   Check (Status = C.Values.Available and then Description.Kind = C.Values.Buffer_Description
     and then Description.Length = Payload, "large exact result length");
   C.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
   Check (Status = C.Values.Available and then Copied = Payload, "large exact copy");
   for B of Data loop Check (B = 16#EF#, "large complete byte image"); end loop;
   Check (C.Retained_Results (Service) = 1, "large retained result");
   C.Release_Result (Service, Handle, Status);
   Check (Status = C.Values.Available and then Handle = C.Values.No_Value and then C.Retained_Results (Service) = 0, "large result release");
   Ada.Text_IO.Put_Line ("Index width checks" & Checks'Image & " bytes" & Payload'Image);
end Index_Width_Tests;
