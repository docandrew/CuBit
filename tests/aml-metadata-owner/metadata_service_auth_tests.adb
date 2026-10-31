with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Metadata_Owner_Support;
procedure Metadata_Service_Auth_Tests is
   package Core is new ACPI_Service_Core (Perform_Delay => AML_Delays.Unavailable_Provider);
   use Core;
   use type Values.Access_Status;
   use type Values.Value_Kind;
   use type Values.Value_Handle;
   use type Values.Audit_Value;
   use type AML_Execute.Execution_Status;
   use type AML_Execute.Named_Metadata_Type;
   use type AML_Decode.Byte;
   type Service_Access is access all State;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Setup (Target : in out State; Revision : Positive) is
      Loaded : Install_Status;
      Report : Namespace.Initialization_Report;
      Status : Values.Access_Status;
   begin
      Install (Target, 1, DSDT, Metadata_Owner_Support.Table (Metadata_Owner_Support.Fixture, Revision), Loaded);
      Check (Loaded = Installed, "service table installed");
      Initialize_Members (Target, Report, Status);
      Check (Status = Values.Available and then Report.Missing = 0 and then Report.Unsupported = 0,
        "service initialized completely");
   end Setup;
   procedure Call (Target : aliased in out State; Name : String; Result : out Values.Result) is
      Node : Namespace.Node_ID;
      Status : Values.Access_Status;
   begin
      Find_Child (Target, Namespace.Root, Name, Node, Status);
      Check (Status = Values.Available and then Node /= Namespace.Root, "service method lookup");
      Invoke_Retained (Target, Node, [others => 0], 0, 100_000, Result, Status);
      Check (Status = Values.Available, "service retained invocation");
   end Call;
begin
   for Revision in 1 .. 2 loop
      declare
         A : constant Service_Access := new State (1, 4096, 4096, Load_Capacity => 1);
         B : constant Service_Access := new State (1, 4096, 4096, Load_Capacity => 1);
         Result : Values.Result;
         Description : Values.Value_Description;
         Handle, Temporary, Derived, Stale : Values.Value_Handle;
         Status : Values.Access_Status;
         Data : AML_Decode.Bytes (1 .. 8);
         Copied : Natural;
      begin
         Setup (A.all, Revision); Setup (B.all, Revision);
         Call (A.all, "CAPT", Result);
         Check (Result.Status = AML_Execute.Metadata_Returned, "service capture positive control");
         Handle := Result.Handle;
         Describe_Result (A.all, Handle, Description, Status);
         Check (Status = Values.Available and then Description.Kind = Values.Metadata_Description
           and then Description.Object_Type = AML_Execute.Device_Metadata,
           "service describes captured class without raw identity");
         declare Before : constant Values.Audit_Value := Audit (B.all); begin
            Describe_Result (B.all, Handle, Description, Status);
            Check (Status = Values.Invalid_Value, "foreign service description rejects handle");
            Read_Result_Bytes (B.all, Handle, 0, Data, Copied, Status);
            Check (Status = Values.Invalid_Value and then Copied = 0
              and then (for all Byte of Data => Byte = 0), "foreign read returns no payload");
            Dereference_Result (B.all, Handle, Derived, Status);
            Check (Status = Values.Invalid_Value and then Derived = Values.No_Value,
              "foreign service cannot dereference captured handle");
            Temporary := Handle; Release_Result (B.all, Temporary, Status);
            Check (Status = Values.Invalid_Value and then Temporary = Handle
              and then Retained_Results (A.all) = 1 and then Retained_Results (B.all) = 0
              and then Audit (B.all) = Before, "foreign release preserves authority and both pin counts");
         end;
         Read_Result_Bytes (A.all, Handle, 0, Data, Copied, Status);
         Check (Status = Values.Wrong_Kind and then Copied = 0,
           "captured metadata confers no byte access");
         Call (A.all, "REPL", Result); Check (Result.Status = AML_Execute.Returned, "replace source through AML");
         Describe_Result (A.all, Handle, Description, Status);
         Check (Status = Values.Available and then Description.Kind = Values.Metadata_Description
           and then Description.Object_Type = AML_Execute.Device_Metadata,
           "service retained capture independent of source replacement");
         Stale := Handle; Release_Result (A.all, Handle, Status);
         Check (Status = Values.Available and then Handle = Values.No_Value
           and then Retained_Results (A.all) = 0, "service releases captured result");
         Describe_Result (A.all, Stale, Description, Status);
         Check (Status = Values.Invalid_Value, "released service handle stays invalid");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Metadata service auth checks" & Checks'Image);
end Metadata_Service_Auth_Tests;
