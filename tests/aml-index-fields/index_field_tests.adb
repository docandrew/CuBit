pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Names;
with Firmware_Tables;
with Index_Field_Oracle;
procedure Index_Field_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
     Namespace_Node_Capacity => 64, Retained_Result_Capacity => 16);
   package A renames AML_Field_Access;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Namespace.Lookup_Status;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type A.Operation_ID;
   use type A.Observation;
   use type AML_Decode.Byte;
   use type AML_Decode.Status;
   use type AML_Names.Parse_Status;
   Index_Register : constant A.Register_ID := 16#8000_0000_0000_0011#;
   Data_Register : constant A.Register_ID := 16#9000_0000_0000_0022#;
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Index_Value, Data_Value : AML_Decode.Integer_Value := 0;
      Calls : Natural := 0;
      Index_Writes : Natural := 0;
      Allow_Index_Write, Allow_Data_Read, Allow_Data_Write : Boolean := True;
      Revoke_Data_After_Index, Unknown_Index, Unknown_Data, Fail_Data : Boolean := False;
      Receipt, Confirmed : A.Observation := A.No_Observation;
      Next_Operation : A.Operation_ID := 1;
   end record;
   function Pending (IO : Context) return Boolean is
      (IO.Receipt.Present);
   function Permission (IO : Context; Register : A.Register_ID; Writing : Boolean) return Boolean is
     (if Register = Index_Register then not Writing or else IO.Allow_Index_Write
      elsif Register = Data_Register then
        not (IO.Revoke_Data_After_Index and then IO.Index_Writes > 0)
        and then (if Writing then IO.Allow_Data_Write else IO.Allow_Data_Read)
      else False);
   procedure Describe (IO : Context; Target : A.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 1, Base => 16#1230#, Length => 2),
        Byte_Offset => (if Target.Register = Data_Register then 1 else 0),
        Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := IO.Owner /= AML_Identity.No_Identity and then Target.Provider = IO.Owner
        and then Target.Endpoint = 7 and then Target.Epoch = 1
        and then Target.Register in Index_Register | Data_Register;
   end Describe;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Geometry : AML_Field_Layout.Register_Geometry;
      Allowed : Boolean;
   begin
      Reply := (others => <>);
      Describe (IO, Item.Destination, Geometry, Allowed);
      if not Allowed or else (Item.For_Write and then Item.Value > 255) then return; end if;
      IO.Calls := IO.Calls + 1;
      if not Permission (IO, Item.Destination.Register, Item.For_Write) then return; end if;
      if (IO.Unknown_Index and then Item.Destination.Register = Index_Register)
        or else (IO.Unknown_Data and then Item.Destination.Register = Data_Register) then
         if IO.Next_Operation = A.Operation_ID'Last then return; end if;
         IO.Unknown_Index := False; IO.Unknown_Data := False;
         IO.Receipt := (True, IO.Owner, Item, IO.Next_Operation);
         Reply := (A.Indeterminate, IO.Owner, 0, IO.Next_Operation);
         IO.Next_Operation := IO.Next_Operation + 1;
         return;
      end if;
      if IO.Fail_Data and then Item.Destination.Register = Data_Register then
         IO.Fail_Data := False;
         Reply := (A.Failed_Completed, IO.Owner, 0, A.No_Operation); return;
      end if;
      if Item.For_Write then
         if Item.Destination.Register = Index_Register then
            IO.Index_Value := Item.Value; IO.Index_Writes := IO.Index_Writes + 1;
         else IO.Data_Value := Item.Value; end if;
      end if;
      Reply := (A.Completed, IO.Owner,
        (if Item.For_Write then 0 elsif Item.Destination.Register = Index_Register
         then IO.Index_Value else IO.Data_Value), A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected.Present and then not Pending (IO) and then Expected = IO.Confirmed; end Confirmed;
   procedure Authorize (IO : Context; Target : A.Target;
     For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then Permission (IO, Target.Register, For_Write);
   end Authorize;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize);
   Service : aliased C.State (1, Index_Field_Oracle.Raw_Table'Length,
     Index_Field_Oracle.Raw_Table'Length);
   IO : Context;
   Items : F.Binding_Set;
   Staged : C.Stage_Status;
   Frozen : C.Freeze_Status;
   Status : C.Values.Access_Status;
   Execution : C.Execution_Report;
   Members : C.Namespace.Initialization_Report;
   Issued : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   procedure Bind (Name : AML_Names.Segment; Register : A.Register_ID) is
      Node : C.Namespace.Node_ID;
      Description : C.Values.Field_Description;
      Key : C.Values.Field_Key;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " lookup");
      C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
      Check (Status = C.Values.Available, Name & " copied field descriptor");
      F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, Register), Items, Status);
      Check (Status = C.Values.Available, Name & " named leaf binding");
   end Bind;
   procedure Expect (Name : AML_Names.Segment; Expected : AML_Decode.Integer_Value) is
      Found : C.Namespace.Lookup_Result;
      Result : AML_Execute.Execution_Result;
   begin
      C.Find_Object (Service, C.Namespace.Root, Name, Found, Status);
      Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, Name & " lookup");
      F.Invoke_Scalar (Service, IO, Items, Found.Node, [others => 0], 0, 10_000, Result);
      Check (Result.Status = AML_Execute.Returned, Name & ": " & Result.Status'Image);
      Check (Result.Value = Expected, Name & " ACPICA result");
      Check (C.Retained_Results (Service) = 0, Name & " no retained roots");
   end Expect;
   procedure Fault (Name : AML_Names.Segment; Wanted : AML_Execute.Execution_Status;
     Budget : Natural := 10_000) is
      Found : C.Namespace.Lookup_Result;
      Result : AML_Execute.Execution_Result;
   begin
      C.Find_Object (Service, C.Namespace.Root, Name, Found, Status);
      Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, Name & " fault lookup");
      F.Invoke_Scalar (Service, IO, Items, Found.Node, [others => 0], 0, Budget, Result);
      Check (Result.Status = Wanted, Name & " fault: " & Result.Status'Image);
      Check (C.Retained_Results (Service) = 0, Name & " fault roots");
   end Fault;
   procedure Reset_Model is
   begin
      Check (not Pending (IO) and then not F.Uncertain (Service).Present, "model reset only after resolution");
      IO.Index_Value := 0; IO.Data_Value := 16#A5#; IO.Calls := 0; IO.Index_Writes := 0;
      IO.Allow_Index_Write := True; IO.Allow_Data_Read := True; IO.Allow_Data_Write := True;
      IO.Revoke_Data_After_Index := False; IO.Unknown_Index := False; IO.Unknown_Data := False; IO.Fail_Data := False;
      IO.Confirmed := A.No_Observation;
   end Reset_Model;
   procedure Confirm_Receipt is
      Saved : constant A.Observation := IO.Receipt;
      Before : constant Natural := IO.Calls;
   begin
      Check (Saved.Present and then F.Uncertain (Service) = Saved, "exact leaf completion observation");
      IO.Receipt := A.No_Observation; IO.Confirmed := Saved;
      F.Reconcile (Service, IO, Saved, Status);
      Check (Status = C.Values.Available and then not F.Uncertain (Service).Present, "authoritative completion resolution");
      Check (IO.Calls = Before, "reconciliation does not replay index/data effects");
   end Confirm_Receipt;
   procedure Reserved_Declaration_Access is
      Raw : AML_Decode.Bytes := Index_Field_Oracle.Raw_Table;
      Flag_Position : Natural := 0;
      Extended_Bytes : constant Positive := 2;
      Index_Extension : constant AML_Decode.Byte := 16#86#;
      Byte_Access_Code : constant AML_Decode.Byte := 1;
      Reserved_Access_Code : constant AML_Decode.Byte := 6;
      Checksum_Position : constant Positive := 10;
      Sum : AML_Decode.Byte := 0;
      Bad : aliased C.State (1, Raw'Length, Raw'Length);
      Bad_IO : Context;
      Bad_Items : F.Binding_Set;
      Report : C.Execution_Report;
      Init : C.Namespace.Initialization_Report;
      Found : C.Namespace.Lookup_Result;
      Description : C.Values.Field_Description;
      Key : C.Values.Field_Key;
      Result : AML_Execute.Execution_Result;
   begin
      -- Construct one malformed declaration from a normal compiled fixture.
      -- A valid AccessAs follows it, so effective-access validation alone
      -- cannot reject the reserved original declaration code.
      for Position in Raw'First + Firmware_Tables.Table_Header_Size .. Raw'Last - 1 loop
         if Raw (Position) = AML_Decode.Extended_Op and then Raw (Position + 1) = Index_Extension then
            declare
               P : constant AML_Decode.Package_Result := AML_Decode.Read_Package
                 (Raw (Position + Extended_Bytes .. Raw'Last));
               Cursor : Natural;
               Path : AML_Names.Name_Result;
            begin
               Check (P.Kind = AML_Decode.Accepted, "normal indexed fixture package");
               Cursor := Position + Extended_Bytes + P.Encoding_Bytes;
               for Operand in 1 .. 2 loop
                  Path := AML_Names.Read_Name (Raw (Cursor .. Raw'Last));
                  Check (Path.Kind = AML_Names.Accepted and then Path.Count = 1, "normal indexed fixture source name");
                  Cursor := Cursor + Path.Consumed;
               end loop;
               Flag_Position := Cursor; exit;
            end;
         end if;
      end loop;
      Check (Flag_Position > 0 and then Raw (Flag_Position) = Byte_Access_Code
        and then Raw (Flag_Position + 1) = 1 and then Raw (Flag_Position + 2) = Byte_Access_Code,
        "normal ByteAcc declaration followed by AccessAs ByteAcc");
      Raw (Flag_Position) := Reserved_Access_Code;
      Raw (Checksum_Position) := 0;
      for Octet of Raw loop Sum := Sum + Octet; end loop;
      Raw (Checksum_Position) := -Sum;
      AML_Identity.Issuer.Issue (Bad_IO.Owner, Issued); Check (Issued, "malformed fixture provider");
      C.Stage_Table (Bad, 1, C.DSDT, Raw, Staged); Check (Staged = C.Staged, "malformed AML remains a valid table envelope");
      C.Freeze_Catalog (Bad, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "malformed fixture freeze");
      C.Execute_Frozen (Bad, 100_000, Report, Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed,
        "raw field metadata does not authorize access");
      C.Initialize_Members (Bad, Init, Status); Check (Status = C.Values.Available, "malformed fixture members");
      F.Clear (Bad_Items);
      for Register in 1 .. 2 loop
         C.Find_Object (Bad, C.Namespace.Root, (if Register = 1 then "IDX0" else "DAT0"), Found, Status);
         Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "malformed fixture leaf");
         C.Read_Field_Descriptor (Bad, Found.Node, Description, Key, Status);
         Check (Status = C.Values.Available, "malformed fixture independent direct leaf descriptor");
         F.Add_Binding (Bad, Bad_IO, Key,
           (Bad_IO.Owner, 7, 1, (if Register = 1 then Index_Register else Data_Register)), Bad_Items, Status);
         Check (Status = C.Values.Available, "malformed fixture named leaf binding");
      end loop;
      C.Find_Object (Bad, C.Namespace.Root, "RDM0", Found, Status);
      Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "malformed fixture reader");
      F.Invoke_Scalar (Bad, Bad_IO, Bad_Items, Found.Node, [others => 0], 0, 10_000, Result);
      Check (Result.Status = AML_Execute.Unsupported_Value and then Bad_IO.Calls = 0,
        "AccessAs cannot rehabilitate a reserved declaration access code");
   end Reserved_Declaration_Access;
begin
   AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "provider identity");
   C.Stage_Table (Service, 1, C.DSDT, Index_Field_Oracle.Raw_Table, Staged);
   Check (Staged = C.Staged, "normal iASL table admission");
   C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "catalog freeze");
   C.Execute_Frozen (Service, 100_000, Execution, Status);
   Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed,
     "IndexField module: " & Execution.Modules.Outcome'Image & "/" & Execution.Modules.Failure'Image);
   C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "members");
   F.Clear (Items); Bind ("IDX0", Index_Register); Bind ("DAT0", Data_Register);
   Expect ("TST0", 16#A5_03_A5#);
   Expect ("TST1", 16#5A_01#);
   Expect ("TST2", 16#35_04#);
   Check (IO.Index_Value = 4 and then IO.Data_Value = 16#35#, "final leaf register values");
   Expect ("RDM0", 16#35#); Expect ("WRM0", 0); Expect ("WFM0", 0);
   Expect ("DYN0", 16#5A#); Expect ("DYN0", 16#5A#);
   Check (IO.Index_Value = 2, "method-local indexed field selects byte two");
   declare
      Parent, Child : C.Namespace.Node_ID;
   begin
      C.Find_Child (Service, C.Namespace.Root, "DYN0", Parent, Status);
      Check (Status = C.Values.Available and then Parent /= C.Namespace.Root, "dynamic method lookup");
      C.Find_Child (Service, Parent, "TMP0", Child, Status);
      Check (Status = C.Values.Available and then Child = C.Namespace.Root, "method-local indexed field removed");
   end;
   Reset_Model; IO.Allow_Index_Write := False;
   Fault ("RDM0", AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "indexed read needs index-write permission before effects");
   Reset_Model; IO.Allow_Data_Read := False;
   Fault ("RDM0", AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "denied data read does not select index");
   Reset_Model; IO.Allow_Data_Write := False;
   Fault ("WRM0", AML_Execute.Unsupported);
   Check (IO.Calls = 0, "denied partial data write has no index effect");
   Reset_Model; IO.Allow_Data_Read := False;
   Fault ("WRM0", AML_Execute.Unsupported);
   Check (IO.Calls = 0, "Preserve partial write preflights data read");
   Reset_Model; IO.Allow_Data_Read := False;
   Expect ("WFM0", 0);
   Check (IO.Calls = 2 and then IO.Index_Value = 1 and then IO.Data_Value = 16#5A#,
     "full write requires no data-read permission");
   Reset_Model; IO.Revoke_Data_After_Index := True;
   Fault ("RDM0", AML_Execute.Unsupported_Value);
   Check (IO.Calls = 1 and then IO.Index_Value = 3 and then IO.Data_Value = 16#A5#,
     "post-index revocation preserves known index effect and denies data");
   Fault ("RDM0", AML_Execute.Unsupported_Value);
   Check (IO.Calls = 1, "no automatic retry after post-index denial");
   Reset_Model; IO.Unknown_Index := True;
   Fault ("RDM0", AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 1 and then Pending (IO) and then IO.Receipt.Item.For_Write
     and then IO.Receipt.Item.Destination.Register = Index_Register, "unknown index write retained as leaf request");
   Fault ("RDM0", AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 1, "unknown index completion blocks further requests");
   Confirm_Receipt; Expect ("RDM0", 16#A5#);
   Check (IO.Calls = 3, "new invocation after confirmation performs one fresh pair");
   Reset_Model; IO.Unknown_Data := True;
   Fault ("RDM0", AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 2 and then IO.Index_Value = 3 and then Pending (IO)
     and then not IO.Receipt.Item.For_Write and then IO.Receipt.Item.Destination.Register = Data_Register,
     "unknown data read retains data leaf after known index write");
   Fault ("RDM0", AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 2, "unknown data completion blocks further requests");
   Confirm_Receipt;
   Reset_Model; IO.Unknown_Data := True;
   Fault ("WFM0", AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 2 and then IO.Index_Value = 1 and then Pending (IO)
     and then IO.Receipt.Item.For_Write and then IO.Receipt.Item.Destination.Register = Data_Register,
     "unknown data write propagates through Store with its leaf receipt");
   Fault ("WFM0", AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 2, "unknown data write blocks further requests");
   Confirm_Receipt; Expect ("WFM0", 0);
   Check (IO.Calls = 4 and then IO.Data_Value = 16#5A#, "deliberate write after confirmed completion");
   Reset_Model; IO.Fail_Data := True;
   Fault ("RDM0", AML_Execute.Unsupported_Value);
   Check (IO.Calls = 2 and then IO.Index_Value = 3 and then not Pending (IO)
     and then not F.Uncertain (Service).Present, "known failed data result is not an unknown completion");
   Expect ("RDM0", 16#A5#);
   Check (IO.Calls = 4, "known failure permits a deliberate new invocation");
   Reset_Model; Fault ("RDB0", AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "out-of-range index rejected before effects");
   Fault ("RDL0", AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "unimplemented Global Lock rejected before effects");
   Fault ("RDM0", AML_Execute.Budget_Exceeded, Budget => 0);
   Check (IO.Calls = 0, "exhausted AML budget performs no field request");
   Reserved_Declaration_Access;
   Ada.Text_IO.Put_Line ("Index field checks" & Natural'Image (Checks));
end Index_Field_Tests;
