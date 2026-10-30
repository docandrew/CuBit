pragma Ada_2022;
generic
   -- Trusted provider observation for an independently admitted EC/GPIO device.
   -- Only EC/GPIO space codes are supplied. Metadata does not establish this
   -- association or grant authority. Keep the provider installed through both
   -- directions; callbacks preserve Core/control ownership and do not reenter.
   with function Provider_Ready
     (Context : Controller; Device : Core.Node_Descriptor;
      Space : Core.Namespace.Region_Space_ID) return Boolean;
   with procedure Release_Result
     (Service : in out Core.State; Handle : in out Core.Values.Value_Handle;
      Status : out Core.Values.Access_Status) is Core.Release_Result;
package ACPI_Region_Initialization.Orphans with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type Address_Space is (Embedded_Controller, GPIO_Controller);
   type Phase is (Fresh, Checking, Connecting, Connected, Disconnecting,
      Disconnected, Skipped, Failed);
   type Outcome is (Not_Started, Notice_Completed, No_Method, Region_Present,
      Deferred_Region, Invalid_Device, Provider_Unavailable, Lookup_Failed,
      Unsupported_Method, Identity_Changed, Budget_Exhausted, Evaluation_Failed,
      Indeterminate, Provider_Contract_Error, Cleanup_Failed);
   type Report is record
      Stage : Phase := Fresh;
      Status : Outcome := Not_Started;
      Charged, Reported_Charge : Natural := 0;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Fatal : AML_Fatal.Observation;
   end record;
   type Action_Result is (Accepted, Wrong_Phase, Wrong_Context, No_Pending_Result);
   type State is limited private;
   function Observe (Run : State) return Report;
   function Pending (Run : State) return Boolean;
   -- One trusted controller-owned binding, not a wire method/space selector.
   -- Before any AML, verify the current Device, local _REG and absence of any
   -- matching direct OperationRegion. A deferred direct region cannot certify
   -- absence. Other spaces do not prevent orphan EC/GPIO notifications.
   -- Zero budget is terminal without invoking AML. Connect cannot be replayed.
   procedure Connect
     (Run : in out State; Service : aliased in out Core.State;
      Context : in out Controller; Device : Core.Node_Descriptor;
      Space : Address_Space; Budget : Natural; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Action = Accepted then Observe (Run).Charged <= Budget
         else Observe (Run) = Observe (Run)'Old and then Pending (Run) = Pending (Run)'Old);
   -- Disconnect only the exact connected owner/binding while provider readiness
   -- still holds. Refusal/uncertainty never replays a method or removes backing.
   procedure Disconnect
     (Run : in out State; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Action = Accepted then Observe (Run).Charged <= Budget
         else Observe (Run) = Observe (Run)'Old and then Pending (Run) = Pending (Run)'Old);
   -- A failed result release remains owned here. Cleanup authenticates the Core
   -- lifetime, and may proceed after device/method identity has changed. Never
   -- discard this state or replace Core while Pending is true.
   procedure Close
     (Run : in out State; Service : in out Core.State; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Action /= Accepted then Observe (Run) = Observe (Run)'Old
         and then Pending (Run) = Pending (Run)'Old);
private
   type State is limited record
      Owner, Device, Method : Core.Node_Descriptor := Core.Values.No_Node;
      Space : Address_Space := Embedded_Controller;
      Handle : Core.Values.Value_Handle := Core.Values.No_Value;
      Info : Report;
   end record;
end ACPI_Region_Initialization.Orphans;
