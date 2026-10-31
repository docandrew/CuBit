with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces;
with Firmware_Tables;
with AML_Delays;
with AML_Execute;
with ACPI_Service_Core;
with ACPI_EC_Boot_Table;
with ACPI_EC_Boot_Binding;
with ACPI_EC_Discovery;
with ACPI_EC_Boot_Resources;
with ACPI_EC_Resources;
with ACPI_Resource_Data;
procedure EC_Boot_Binding_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 128, Aggregate_Method_Capacity => 4096,
      Retained_Result_Capacity => 4);
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status;
   use type C.Values.Module_Outcome, C.Node_Descriptor, C.Values.Value_Handle;
   use type AML_Execute.Execution_Status;
   use type Firmware_Tables.Byte, Interfaces.Unsigned_32;
   Service, Other : aliased C.State (1, 4096, 4096, 1);
   Checks, Calls, Releases, Release_Failures : Natural := 0;
   Overcharge, Unknown, Empty_Handle, Force_Failure : Boolean := False;
   procedure Check (Condition : Boolean) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with "EC UID check" & Checks'Image; end if; end Check;
   procedure Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
      Handle : C.Values.Value_Handle;
      Released : C.Values.Access_Status;
   begin
      Calls := Calls + 1;
      if Force_Failure then
         Result := (Status => AML_Execute.Unsupported, Charged => 1);
         Status := C.Values.Available;
      elsif Unknown then
         Result := (Status => AML_Execute.Field_Indeterminate, Charged => 1);
         Status := C.Values.Available;
      else C.Evaluate_Named (State, Item, Budget, Result, Status);
      end if;
      if Empty_Handle and then Result.Status = AML_Execute.Object_Returned then
         Handle := Result.Handle; C.Release_Result (State, Handle, Released);
         Check (Released = C.Values.Available and Handle = C.Values.No_Value);
         Result := (Status => AML_Execute.Object_Returned, Charged => Result.Charged, Handle => C.Values.No_Value);
      end if;
      if Overcharge then Result.Charged := Budget + 1; end if;
   end Evaluate;
   procedure Release (State : in out C.State; Handle : in out C.Values.Value_Handle;
                       Status : out C.Values.Access_Status) is
   begin
      Releases := Releases + 1;
      if Release_Failures > 0 then
         Release_Failures := Release_Failures - 1; Status := C.Values.Invalid_Value;
      else C.Release_Result (State, Handle, Status); end if;
   end Release;
   package Binding is new ACPI_EC_Boot_Binding (C, Evaluate, Release);
   use type Binding.Outcome, Binding.Action_Result;
   procedure Fix (Data : in out Firmware_Tables.Bytes) is
      Sum : Firmware_Tables.Byte := 0;
   begin
      Data (Data'First + 9) := 0;
      for Byte of Data loop Sum := Sum + Byte; end loop;
      Data (Data'First + 9) := -Sum;
   end Fix;
   function Load_Boot_Template return Firmware_Tables.Bytes is
      File : Ada.Streams.Stream_IO.File_Type;
      Bytes : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (2));
      Ada.Streams.Stream_IO.Read (File, Bytes, Last); Ada.Streams.Stream_IO.Close (File);
      return Data : Firmware_Tables.Bytes (1 .. Natural (Last)) do
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
      end return;
   end Load_Boot_Template;
   Boot_Template : constant Firmware_Tables.Bytes := Load_Boot_Template;
   function Table (Path : String; UID : Interfaces.Unsigned_32) return Firmware_Tables.Bytes is
      Fixed : constant := ACPI_EC_Boot_Table.Fixed_Size;
   begin
      return Data : Firmware_Tables.Bytes (1 .. Fixed + Path'Length + 1) := [others => 0] do
         Data (1 .. Fixed) := Boot_Template (Boot_Template'First .. Boot_Template'First + Fixed - 1);
         for I in 0 .. 3 loop
            Data (5 + I) := Firmware_Tables.Byte (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Data'Length), I * 8) and 16#FF#);
            Data (61 + I) := Firmware_Tables.Byte (Interfaces.Shift_Right (UID, I * 8) and 16#FF#);
         end loop;
         for I in Path'Range loop Data (Fixed + 1 + I - Path'First) := Character'Pos (Path (I)); end loop;
         Fix (Data);
      end return;
   end Table;
   procedure Expect (Path : String; UID : Interfaces.Unsigned_32; Outcome : Binding.Outcome;
                     Evaluated : Boolean := True; Budget : Natural := 1000) is
      Run : Binding.Session;
      Action : Binding.Action_Result;
      Before : constant Natural := Calls;
      Data : constant Firmware_Tables.Bytes := Table (Path, UID);
   begin
      Binding.Match (Run, Service, Data, Budget, Action);
      Check (Action = Binding.Accepted and Binding.Consumed (Run));
      Check (Binding.Observe (Run).Status = Outcome and not Binding.Pending (Run));
      Check (Calls = Before + (if Evaluated then 1 else 0));
      Check (Binding.Observe (Run).Charged <= Budget);
      Check ((Binding.Device (Run) /= C.Values.No_Node) = (Outcome = Binding.UID_Matched));
      Check (Binding.Is_Current (Run, Service) = (Outcome = Binding.UID_Matched));
      Check (not Binding.Is_Current (Run, Other));
      Binding.Match (Run, Service, Table ("\EC05", 99), Budget, Action);
      Check (Action = Binding.Already_Consumed and Calls = Before + (if Evaluated then 1 else 0));
      Binding.Close (Run, Service, Action); Check (Action = Binding.No_Pending_Result);
   end Expect;
begin
   Check (Ada.Command_Line.Argument_Count = 2);
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Bytes : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
      Stage : C.Stage_Status; Freeze : C.Freeze_Status;
      Report : C.Execution_Report; Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Bytes, Last); Ada.Streams.Stream_IO.Close (File);
      declare Data : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 1, C.DSDT, Data, Stage); Check (Stage = C.Staged);
      end;
      C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK);
      C.Execute_Frozen (Service, 1000, Report, Status);
      Check (Status = C.Values.Available and Report.Modules.Outcome = C.Values.Modules_Completed);
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available);
   end;
   Expect ("\EC00", 0, Binding.UID_Matched);
   Expect ("\ECOA", 0, Binding.UID_Matched);
   Expect ("\EC01", 1, Binding.UID_Matched);
   Expect ("\EC02", 2, Binding.UID_Matched);
   Expect ("\BUS0.EC00", 7, Binding.UID_Matched);
   Expect ("\EC00", 1, Binding.UID_Mismatch);
   Expect ("\EC06", Interfaces.Unsigned_32'Last, Binding.UID_Matched);
   Expect ("\EC07", 0, Binding.UID_Mismatch);
   Expect ("\EC03", 2, Binding.Wrong_UID_Type);
   Expect ("\EC04", 2, Binding.Wrong_UID_Type);
   Expect ("\EC05", 0, Binding.Missing_UID, False);
   Expect ("\NONE", 0, Binding.Missing_Path, False);
   Expect ("\CNT0", 0, Binding.Not_A_Device, False);
   Expect ("\EC0?", 0, Binding.Invalid_Path, False);
   Expect ("\EC00", 0, Binding.Budget_Exhausted, False, 0);
   for Fault in 1 .. 5 loop
      declare Run : Binding.Session; Action : Binding.Action_Result; begin
         Overcharge := Fault in 1 | 4; Unknown := Fault in 3 | 4; Empty_Handle := Fault = 2;
         Force_Failure := Fault = 5;
         Binding.Match (Run, Service, Table ((if Fault = 2 then "\EC04" else "\EC00"), 0), 1000, Action);
         Check (Action = Binding.Accepted and Binding.Observe (Run).Status =
           (if Fault = 3 then Binding.Indeterminate elsif Fault = 5 then Binding.Evaluation_Failed else Binding.Provider_Contract_Error));
         Check (Binding.Observe (Run).Field_Uncertainty = (Fault in 3 | 4));
         Check (Binding.Device (Run) = C.Values.No_Node and not Binding.Pending (Run));
         Overcharge := False; Unknown := False; Empty_Handle := False; Force_Failure := False;
      end;
   end loop;
   declare Run : Binding.Session; Action : Binding.Action_Result; Before, Before_Releases : Natural; begin
      Release_Failures := 2;
      Binding.Match (Run, Service, Table ("\EC04", 2), 1000, Action);
      Check (Binding.Observe (Run).Status = Binding.Cleanup_Failed and Binding.Pending (Run));
      Check (Binding.Observe (Run).Before_Cleanup = Binding.Wrong_UID_Type and C.Retained_Results (Service) = 1);
      Before := Calls; Before_Releases := Releases;
      Binding.Close (Run, Other, Action);
      Check (Action = Binding.Wrong_Context and Releases = Before_Releases and Binding.Pending (Run));
      Binding.Close (Run, Service, Action);
      Check (Action = Binding.Accepted and Binding.Observe (Run).Status = Binding.Cleanup_Failed and Binding.Pending (Run));
      Binding.Close (Run, Service, Action);
      Check (Action = Binding.Accepted and Binding.Observe (Run).Status = Binding.Closed and not Binding.Pending (Run));
      Check (Calls = Before and C.Retained_Results (Service) = 0 and Binding.Device (Run) = C.Values.No_Node);
   end;
   for Fault in 1 .. 2 loop
      declare Run : Binding.Session; Action : Binding.Action_Result; Data : Firmware_Tables.Bytes := Table ("\EC00", 0);
         Before : constant Natural := Calls;
      begin
         if Fault = 1 then Data (10) := Data (10) + 1;
         else Data (38) := 16; Fix (Data); end if;
         Binding.Match (Run, Service, Data, 1000, Action);
         Check (Binding.Observe (Run).Status = (if Fault = 1 then Binding.Invalid_Table else Binding.Unsupported_Registers));
         Check (Calls = Before and Binding.Device (Run) = C.Values.No_Node);
      end;
   end loop;
   declare
      package Discovery is new ACPI_EC_Discovery (C, Evaluate, Release);
      use type Discovery.Action_Result, Discovery.Outcome, Discovery.Source_Kind;
      use type ACPI_EC_Boot_Resources.Outcome, ACPI_EC_Boot_Table.Decode_Status;
      use type ACPI_Resource_Data.Byte_Count;
      UID : Binding.Session;
      Namespace, No_Fuel : Discovery.Session;
      UID_Action : Binding.Action_Result;
      Action : Discovery.Action_Result;
      Wire : constant Firmware_Tables.Bytes := Table ("\EC00", 0);
      Boot : constant ACPI_EC_Boot_Table.Description := ACPI_EC_Boot_Table.Decode (Wire);
      Small : Firmware_Tables.Bytes (1 .. 1);
      Large : Firmware_Tables.Bytes (1 .. 1024);
      Initial_Budget : constant Natural := 1000;
      Remaining, Before_Calls : Natural;
      Metadata : Discovery.Metadata;
      Compared : ACPI_EC_Boot_Resources.Report;
      Value : C.Values.Result;
      Node : C.Namespace.Node_ID;
      Status : C.Values.Access_Status;
      function Current_CRS_Count return Interfaces.Unsigned_64 is
      begin
         C.Find_Child (Service, C.Namespace.Root, "RCNT", Node, Status);
         Check (Status = C.Values.Available);
         C.Observe_Named_Value (Service, Node, Value, Status);
         Check (Status = C.Values.Available and Value.Status = AML_Execute.Returned);
         return Value.Number;
      end Current_CRS_Count;
      use type Interfaces.Unsigned_64;
   begin
      Check (Boot.Status = ACPI_EC_Boot_Table.Decoded and Current_CRS_Count = 0);
      Binding.Match (UID, Service, Wire, Initial_Budget, UID_Action);
      Check (UID_Action = Binding.Accepted and Binding.Observe (UID).Status = Binding.UID_Matched);
      Check (Binding.Observe (UID).Charged <= Initial_Budget);
      Remaining := Initial_Budget - Binding.Observe (UID).Charged;
      Discovery.Discover (Namespace, Service, Binding.Device (UID), False, Remaining, Small, Action);
      Check (Action = Discovery.Accepted and Discovery.Observe (Namespace).Status = Discovery.Capacity_Required);
      Check (Discovery.Pending (Namespace) and Discovery.Observe (Namespace).Required > Small'Length);
      Check (Discovery.Observe (Namespace).Charged <= Remaining
        and Binding.Observe (UID).Charged + Discovery.Observe (Namespace).Charged <= Initial_Budget);
      Check (Current_CRS_Count = 1 and Discovery.Description (Namespace).Device = C.Values.No_Node);
      Before_Calls := Calls;
      Discovery.Copy_Resources (Namespace, Other, Large, Action);
      Check (Action = Discovery.Wrong_Context and Discovery.Pending (Namespace) and Calls = Before_Calls);
      Discovery.Copy_Resources (Namespace, Service, Large, Action);
      Check (Action = Discovery.Accepted and Discovery.Observe (Namespace).Status = Discovery.Described);
      Check (Calls = Before_Calls and Current_CRS_Count = 1 and not Discovery.Pending (Namespace));
      Check (Binding.Is_Current (UID, Service) and Discovery.Is_Current (Namespace, Service));
      Metadata := Discovery.Description (Namespace);
      Check (Metadata.Device = Binding.Device (UID) and not Metadata.Lock_Declared and not Metadata.Lock_Required);
      Check (Metadata.Source = Discovery.FADT_GPE and Firmware_Tables.Byte (Metadata.Event) = Boot.GPE_Bit);
      Compared := ACPI_EC_Boot_Resources.Compare
        (Wire, Large (1 .. Natural (Discovery.Observe (Namespace).Required)));
      Check (Compared.Status = ACPI_EC_Boot_Resources.Claims_Agree);
      Check (Metadata.Registers (ACPI_EC_Resources.Data_Register).Address.Address = Boot.Data.Address
        and Metadata.Registers (ACPI_EC_Resources.Control_Register).Address.Address = Boot.Control.Address);
      Discovery.Discover (Namespace, Service, Binding.Device (UID), False, Initial_Budget, Large, Action);
      Check (Action = Discovery.Already_Consumed and Calls = Before_Calls and Current_CRS_Count = 1);
      Discovery.Copy_Resources (Namespace, Service, Large, Action);
      Check (Action = Discovery.Not_Awaiting_Capacity and Calls = Before_Calls);
      Discovery.Discover (No_Fuel, Service, Binding.Device (UID), False, 0, Large, Action);
      Check (Action = Discovery.Accepted and Discovery.Observe (No_Fuel).Status = Discovery.Budget_Exhausted);
      Check (Calls = Before_Calls and Current_CRS_Count = 1 and not Discovery.Pending (No_Fuel));
      Check (C.Retained_Results (Service) = 0);
   end;
   Check (C.Valid (Service) and C.Retained_Results (Service) = 0);
   Ada.Text_IO.Put_Line ("EC boot binding checks:" & Checks'Image);
end EC_Boot_Binding_Tests;
