with Ada.Text_IO;
with ACPI_Service; use ACPI_Service;
with AML_Decode;
with AML_Execute; use AML_Execute;
with ACPI_Test_Results;
with Firmware_Tables;
procedure Service_Retention_Tests is
   use type Firmware_Tables.Byte;
   use type Firmware_Tables.Bytes;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Bytes;
   use type Values.Access_Status;
   use type Namespace.Bind_Status;
   use type Values.Value_Handle;
   use type Values.Value_Description;
   use type Values.Value_Kind;
   use type Values.Audit_Value;
   subtype Bytes is Firmware_Tables.Bytes;
   type State_Access is access all State;
   S : constant State_Access := new State (4, 4096, 4096);
   Other : constant State_Access := new State (4, 4096, 4096);
   Root, Saved : Values.Value_Handle;
   Retention : Values.Access_Status;
   Released : Values.Access_Status;
   R : Values.Result;
   Scalar : Execution_Result;
   V, Prior_Value : Values.Value_Description;
   Read_Status : Values.Access_Status;
   Installed_Result : Install_Status;
   Checks : Natural := 0;
   function Enc (Text : String) return Bytes is
      B : Bytes (1 .. Text'Length);
   begin
      for I in B'Range loop B (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return B;
   end Enc;
   function Method (Name : String; Body_Code : Bytes) return Bytes is
     (Bytes'[16#14#, Firmware_Tables.Byte (6 + Body_Code'Length)] & Enc (Name) & [0] & Body_Code);
   Code : constant Bytes :=
     [16#08#] & Enc ("CNT0") & [0] &
     [16#08#] & Enc ("BUF0") & [16#11#,4,16#0A#,1,16#A5#] &
     [16#08#] & Enc ("PKG0") & [16#12#,3,1,1] &
     Method ("BUFF", [16#A4#] & Enc ("BUF0")) &
     Method ("PACK", [16#A4#] & Enc ("PKG0")) &
     Method ("REFR", [16#A4#,16#71#] & Enc ("BUF0")) &
     Method ("ONEX", [16#A4#,1]) &
     Method ("BUMP", [16#75#] & Enc ("CNT0") & [16#A4#] & Enc ("BUF0"));
   function Table return Bytes is
      B : Bytes (1 .. 36 + Code'Length) := [others => 0];
      Sum : Firmware_Tables.Byte := 0;
   begin
      B (1 .. 4) := Enc ("DSDT"); B (5) := Firmware_Tables.Byte (B'Length mod 256);
      B (6) := Firmware_Tables.Byte (B'Length / 256); B (9) := 2; B (37 .. B'Last) := Code;
      for Item of B loop Sum := Sum + Item; end loop;
      B (10) := 0 - Sum; return B;
   end Table;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   function Node (Name : String) return Namespace.Node_ID is
     (ACPI_Test_Results.Child (S.all, Namespace.Root, Name));
   procedure Call (Name : String) is
   begin Invoke_Retained (S.all, Node (Name), [others => 0], 0, 100, R, Retention);
      Root := (if R.Status in Object_Returned | Reference_Returned then R.Handle else Values.No_Value);
   end Call;
   procedure Drop is
   begin
      Release_Result (S.all, Root, Released);
      Check (Released = Values.Available and then Root = Values.No_Value, "release");
   end Drop;
begin
   Invoke_Retained (S.all, 0, [others => 0], 0, 100, R, Retention);
   Check (Retention = Values.Wrong_Phase and then R.Status = No_Return, "uninitialized service");
   Install (S.all, 1, DSDT, Table, Installed_Result); Check (Installed_Result = Installed, "install");
   Install (Other.all, 1, DSDT, Table, Installed_Result); Check (Installed_Result = Installed, "other install");
   declare
      Before : constant Values.Audit_Value := Audit (S.all);
   begin
      Observe_Named_Value (S.all, Node ("CNT0"), R, Retention);
      Check (Retention = Values.Available and then R.Status = Returned and then R.Number = 0,
         "loading copied integer");
      Observe_Named_Value (S.all, Node ("BUF0"), R, Retention);
      Check (Retention = Values.Wrong_Phase and then Retained_Results (S.all) = 0
        and then Audit (S.all) = Before, "loading cannot publish compound pin");
      Invoke_Retained (S.all, Node ("BUMP"), [others => 0], 0, 100, R, Retention);
      Check (Retention = Values.Wrong_Phase and then R.Charged = 0 and then Audit (S.all) = Before,
         "unsealed invocation has no effects");
   end;
   ACPI_Test_Results.Seal (S.all); ACPI_Test_Results.Seal (Other.all);
   declare
      Report : Namespace.Initialization_Report;
      Before : constant Values.Audit_Value := Audit (S.all);
      Bound : Namespace.Bind_Status;
      N : Namespace.Node_ID;
   begin
      Initialize_Members (S.all, Report, Retention);
      Check (Retention = Values.Wrong_Phase and then Audit (S.all) = Before, "second seal rejected");
      Install (S.all, 2, SSDT, Table, Installed_Result);
      Check (Installed_Result = Wrong_Order and then Audit (S.all) = Before, "late load rejected");
      Declare_Table_Region (S.all, Namespace.Root, "LATE", (Name => "DSDT", others => <>), N, Bound);
      Check (Bound = Namespace.Binding_Invalid and then N = Namespace.Root and then Audit (S.all) = Before,
         "late metadata declaration rejected");
      Observe_Named_Value (S.all, Node ("ONEX"), R, Retention);
      Check (Retention = Values.Wrong_Kind and then Retained_Results (S.all) = 0,
         "named observation never executes method");
   end;
   for Kind in 1 .. 3 loop
      Call ((case Kind is when 1 => "BUFF", when 2 => "PACK", when others => "REFR"));
      Check (Retention = Values.Available and then Retained_Results (S.all) = 1, "compound retained");
      Saved := Root;
      Describe_Result (S.all, Root, Prior_Value, Read_Status);
      Check (Read_Status = Values.Available, "read before later invocation");
      begin
         Invoke_Retained (S.all, Node ("ONEX"), [others => 0], 0, 100, R, Retention);
         Check (R.Status = Returned and then R.Number = 1
           and then Retained_Results (S.all) = 1, "later call retains prior root");
      end;
      Describe_Result (S.all, Root, V, Read_Status);
      Check (Read_Status = Values.Available, "retained read");
      Check (V = Prior_Value and then Root = Saved, "same opaque pin and description survive later invocation");
      if Kind = 1 then
         Check (V.Kind = Values.Buffer_Description and then ACPI_Test_Results.Bytes
           (S.all, Root) = AML_Decode.Bytes'[16#A5#], "buffer bytes");
      elsif Kind = 2 then Check (V.Kind = Values.Package_Description, "package");
      else Check (V.Kind = Values.Reference_Description, "reference retained"); end if;
      declare
         Before : constant Values.Audit_Value := Audit (Other.all);
      begin
         Describe_Result (Other.all, Root, V, Read_Status);
         Check (Read_Status = Values.Invalid_Value, "foreign read");
         Release_Result (Other.all, Root, Released);
         Check (Released = Values.Invalid_Value and then Root = Saved
           and then Audit (Other.all) = Before and then Retained_Results (Other.all) = 0, "foreign release");
      end;
      Drop;
      Describe_Result (S.all, Saved, V, Read_Status); Check (Read_Status = Values.Invalid_Value, "stale read");
      Release_Result (S.all, Saved, Released); Check (Released = Values.Invalid_Value, "stale release");
   end loop;
   Call ("PACK");
   declare
      Element : Values.Value_Handle;
      Before_Count : constant Natural := Retained_Results (S.all);
   begin
      Read_Result_Element (S.all, Root, 1, Element, Retention);
      Check (Retention = Values.Out_Of_Bounds and then Element = Values.No_Value
         and then Retained_Results (S.all) = Before_Count, "package bounds preserve pins");
      Read_Result_Element (S.all, Root, 0, Element, Retention);
      Check (Retention = Values.Available and then Retained_Results (S.all) = Before_Count + 1,
         "element owns independent pin");
      Drop;
      Describe_Result (S.all, Element, V, Read_Status);
      Check (Read_Status = Values.Available and then V.Kind = Values.Integer_Description
         and then V.Number = 1, "element survives parent release");
      Release_Result (S.all, Element, Released);
      Check (Released = Values.Available, "release child pin");
   end;
   Call ("REFR");
   declare
      Target : Values.Value_Handle;
   begin
      Dereference_Result (S.all, Root, Target, Retention);
      Check (Retention = Values.Available, "explicit dereference pin");
      Drop;
      Check (ACPI_Test_Results.Bytes (S.all, Target) = AML_Decode.Bytes'[16#A5#],
         "dereferenced target survives descriptor release");
      Release_Result (S.all, Target, Released); Check (Released = Values.Available, "release target");
   end;
   Call ("BUFF");
   declare
      Data : AML_Decode.Bytes (10 .. 12) := [others => 255];
      Copied : Natural;
   begin
      Read_Result_Bytes (S.all, Root, 0, Data, Copied, Retention);
      Check (Retention = Values.Available and then Copied = 1 and then Data = AML_Decode.Bytes'[16#A5#,0,0],
         "copied bytes zero tail and arbitrary lower bound");
      declare
         High : AML_Decode.Bytes (Positive'Last - 2 .. Positive'Last);
         Empty : AML_Decode.Bytes (1 .. 0);
      begin
         Read_Result_Bytes (S.all, Root, 0, High, Copied, Retention);
         Check (Retention = Values.Available and then Copied = 1 and then High = AML_Decode.Bytes'[16#A5#,0,0],
            "high-bound copied bytes");
         Read_Result_Bytes (S.all, Root, 0, Empty, Copied, Retention);
         Check (Retention = Values.Available and then Copied = 0, "null destination");
         Read_Result_Bytes (S.all, Root, Natural'Last, Data, Copied, Retention);
         Check (Retention = Values.Out_Of_Bounds and then Copied = 0 and then Data = AML_Decode.Bytes'[0,0,0],
            "huge offset denied before arithmetic");
      end;
      Read_Result_Bytes (Other.all, Root, 0, Data, Copied, Retention);
      Check (Retention = Values.Invalid_Value and then Copied = 0 and then Data = AML_Decode.Bytes'[0,0,0],
         "foreign bytes denied and cleared");
      Saved := Root; Drop;
      Read_Result_Bytes (S.all, Saved, 0, Data, Copied, Retention);
      Check (Retention = Values.Invalid_Value and then Copied = 0, "stale bytes denied");
   end;
   Call ("BUFF");
   begin
      raise Constraint_Error with "simulated serialization failure";
   exception
      when Constraint_Error => Drop;
   end;
   Check (Retained_Results (S.all) = 0, "consumer failure releases result");
   Invoke_Scalar (S.all, Node ("BUFF"), [others => 0], 0, 100, Scalar);
   Check (Scalar.Status = Unsupported_Value and then Retained_Results (S.all) = 0, "scalar compound cleanup");
   Invoke_Scalar (S.all, Node ("ONEX"), [others => 0], 0, 100, Scalar);
   Check (Scalar.Status = Returned and then Scalar.Value = 1 and then Retained_Results (S.all) = 0, "scalar value");
   declare
      Roots : array (1 .. Max_Namespace_Nodes) of Values.Value_Handle;
   begin
      for I in Roots'Range loop
         Call ("BUMP"); Check (Retention = Values.Available, "fill roots"); Roots (I) := Root;
      end loop;
      declare
         Before : constant Values.Audit_Value := Audit (S.all);
      begin
         Observe_Named_Value (S.all, Node ("BUF0"), R, Retention);
         Check (Retention = Values.Root_Limit and then Retained_Results (S.all) = Max_Namespace_Nodes
           and then Audit (S.all) = Before, "named observation root quota atomic");
         Call ("BUMP");
         Check (Retention = Values.Root_Limit and then R.Status = Value_Limit
           and then R.Charged = 0 and then Root = Values.No_Value and then Audit (S.all) = Before,
           "quota before side effects");
      end;
      for Item of Roots loop
         Release_Result (S.all, Item, Released); Check (Released = Values.Available, "release quota roots");
      end loop;
   end;
   Check (Retained_Results (S.all) = 0, "no leaked roots");
   Ada.Text_IO.Put_Line ("SERVICE-RETENTION PASS" & Checks'Image);
end Service_Retention_Tests;
