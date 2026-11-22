pragma Ada_2022;
with Interfaces;with System;
with ACPI_Service;with ACPI_One_Service;with ACPI_Control_Method_Button;
with ACPI_Requests.Power_Buttons;with ACPI_Native_Power_Buttons;
with ACPI_Fallible_Heap;with ACPI_Backend_Kernel_Handles;with ACPI_Backend_Binding_Types;
with ACPI_Native_Diagnostic_Output;
package body ACPI_Native_Button_Discovery is
   package Q renames ACPI_Requests;package C renames ACPI_Service;package One renames ACPI_One_Service;
   package W renames ACPI_Native_Power_Buttons;package B renames ACPI_Backend_Binding_Types;
   use type Interfaces.Unsigned_64,System.Address,Q.Phase,One.Runtime.Status.Phase;
   package Identity is new ACPI_Control_Method_Button (C, One.Runtime.Evaluate_Discovery);
   package Buttons is new Q.Power_Buttons
     (Identity, One.Runtime.Begin_Event, One.Runtime.End_Event, W.Control_Method_Press);
   use type Buttons.Bridge.Outcome;
   type Run_Access is access all Buttons.Run;
   Inventory : Run_Access;
   Attempted, Working, Stopped : Boolean := False;
   Discovery_Fuel : constant Positive := 100_000;
   Discovery_Window_Microseconds : constant Interfaces.Unsigned_64 := 500_000;
   Minimum_Heap_Alignment : constant Interfaces.Unsigned_64 := 16;
   function Quiet return Boolean is
     (not Working and then (Inventory = null or else Buttons.Quiet (Inventory.all)));
   function Create (Capacity : Positive) return Run_Access is
      subtype Shape is Buttons.Run (Capacity);
      Bits : constant Interfaces.Unsigned_64 := Shape'Object_Size;
      Bytes : constant Interfaces.Unsigned_64 := Bits / System.Storage_Unit +
        (if Bits mod System.Storage_Unit = 0 then 0 else 1);
      Address : constant System.Address := ACPI_Fallible_Heap.Reserve
        (Bytes, Interfaces.Unsigned_64'Max (Minimum_Heap_Alignment, Shape'Alignment));
   begin
      if Address = System.Null_Address then return null;end if;
      declare
         -- Fresh fallible heap block, initialized in place as the snapshot owner is.
         pragma Warnings (Off, "default initialization of * may modify overlaid storage");
         Value : aliased Shape with Address => Address;
         pragma Warnings (On, "default initialization of * may modify overlaid storage");
      begin return Value'Unchecked_Access;end;
   end Create;
   procedure Start (Server : in out Q.State; Fixed_Available : Boolean; Result : out Outcome) is
      Capacity : Positive := 1;
      procedure Extent (Service : C.State; Completed : out Boolean) is
      begin Capacity := Positive'Max (1, Natural (C.Namespace_Node_Count (Service)));Completed := True;end Extent;
      package Metadata is new Q.With_Metadata_Continuation (Extent);
      Metadata_OK, Clock_OK : Boolean;
      Now : B.Tick;
      Deadline : Interfaces.Unsigned_64;
      Report : Buttons.Report;
      procedure Finish (Status : Outcome) is
      begin
         Result := Status;Working := False;
         if Status not in Fixed_Selected | Control_Method_Selected then
            W.Configure_Control_Method (False);
         end if;
         ACPI_Native_Diagnostic_Output.Write
           ("acpi: stage power-button " &
             (if Status in Fixed_Selected | Control_Method_Selected then "ok " else "fail ") & Status'Image &
             " slots=" & Capacity'Image & " bound=" & Report.Bound'Image & " fuel=" & Report.Charged'Image,
            not Q.Notifications_Pending (Server) and then not Q.Retained_Results_Pending (Server)
              and then One.Runtime.Current = One.Runtime.Status.Idle and then One.Root.Fields.Control_Idle
              and then One.Root.Fields.EC_Commands.Quiescent,
            Status in Fixed_Selected | Control_Method_Selected);
         if Status = Discovery_Failed then
            ACPI_Native_Diagnostic_Output.Write
              ("acpi: stage power-button-detail fail " & Report.Status'Image &
               " scan=" & Report.Scanned'Image &
               " id=" & Report.Last_Binding.Identity.Outcome'Image &
               " exec=" & Report.Last_Binding.Identity.Execution'Image &
               " close=" & Report.Close_Result'Image, False);
         end if;
      end Finish;
   begin
      Result := Not_Ready;
      if Stopped then Result := Stopping;return;end if;
      if Attempted or Working then Result := Already_Attempted;return;end if;
      Attempted := True;W.Configure_Fixed (Fixed_Available);
      if Fixed_Available then Finish (Fixed_Selected);return;end if;
      if Q.Current (Server) /= Q.Complete or else Q.Notifications_Pending (Server)
        or else Q.Retained_Results_Pending (Server) or else One.Runtime.Current /= One.Runtime.Status.Idle
        or else not One.Root.Hardware.Admitting or else not One.Root.Fields.Control_Idle
        or else not One.Root.Fields.EC_Commands.Quiescent then Finish (Not_Ready);return;end if;
      Working := True;Metadata.Advance (Server, Metadata_OK);
      if not Metadata_OK or Stopped then Finish ((if Stopped then Stopping else Not_Ready));return;end if;
      ACPI_Backend_Kernel_Handles.Read_Clock (Now, Clock_OK);
      if not Clock_OK or else Interfaces.Unsigned_64 (Now) >= Interfaces.Unsigned_64'Last - Discovery_Window_Microseconds then
         Finish (Clock_Unavailable);return;
      end if;
      Deadline := Interfaces.Unsigned_64 (Now) + Discovery_Window_Microseconds;
      Inventory := Create (Capacity);
      if Inventory = null then Finish (Allocation_Refused);return;end if;
      Buttons.Discover (Server, Inventory.all, Discovery_Fuel, Deadline, Report);
      if not Stopped and then Report.Status = Buttons.Bridge.Ready and then Buttons.Available (Server, Inventory.all) then
         W.Configure_Control_Method (True);Finish (Control_Method_Selected);
      else
         W.Configure_Control_Method (False);
         Finish ((if Stopped then Stopping elsif Report.Status = Buttons.Bridge.Unavailable then Source_Unavailable else Discovery_Failed));
      end if;
   end Start;
   procedure Route (Server : Q.State; Accepted : out Boolean) is
   begin
      Accepted := False;if Working then return;end if;
      if Inventory = null then Accepted := True;return;end if;
      Buttons.Route (Server, Inventory.all, Accepted);
      if not Stopped and then not Buttons.Available (Server, Inventory.all) then W.Withdraw_Control_Method;end if;
   end Route;
   procedure Begin_Stop is
   begin Stopped := True;if Inventory /= null then Buttons.Retire (Inventory.all);end if;end Begin_Stop;
   procedure Close (Released : out Boolean) is
   begin
      Released := False;if not Stopped or not Quiet then return;end if;
      if Inventory /= null then ACPI_Fallible_Heap.Release (Inventory.all'Address);Inventory := null;end if;
      Released := True;
   end Close;
end ACPI_Native_Button_Discovery;
