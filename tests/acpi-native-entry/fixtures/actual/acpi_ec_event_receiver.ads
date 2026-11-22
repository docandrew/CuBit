pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_EC_Methods;
with ACPI_EC_Event_Types;
with ACPI_EC_Event_Policy;
with AML_Fatal;
with CuBit.Process_IDs;
--  One serialized receiver instance per owning Core/event domain. Enrollment
--  is trusted controller work; Sender is authenticated receive metadata, not
--  a notice field. Transport must preserve per-controller event order.
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Methods is new ACPI_EC_Methods (Core => Core, others => <>);
   Maximum_Controllers : Positive := 8;
package ACPI_EC_Event_Receiver with SPARK_Mode is
   type Enrollment is (Installed, Invalid_Identity, Invalid_Scope,
      Scope_Conflict, Already_Active, Old_Epoch, Route_Held, Capacity_Exhausted,
      Receiver_Busy, Receiver_Recovery_Required);
   -- A Fatal result closes this receiver domain to new events/enrollment.
   -- Revocation and retained-result cleanup remain available. This does not
   -- stop other Core callers or other receiver instances; the owning service
   -- must observe recovery and stop all further AML in its execution domain.
   -- No reset/retry API: diagnostics remain process-lived.
   function Recovery_Needed return Boolean;
   function Fatal_Diagnostic return AML_Fatal.Observation;
   procedure Install
     (Service : Core.State; Controller : ACPI_EC_Event_Types.Controller_ID;
      Epoch : ACPI_EC_Event_Types.Epoch_ID; Sender : CuBit.Process_IDs.Process_ID;
      Scope : Core.Node_Descriptor; Result : out Enrollment)
     with Pre => Core.Valid (Service);
   --  Revocation disables new events without releasing retained invocation or
   --  uncertainty. Used slots/epoch high-water marks are never recycled.
   procedure Revoke
     (Controller : ACPI_EC_Event_Types.Controller_ID;
      Epoch : ACPI_EC_Event_Types.Epoch_ID; Accepted : out Boolean);
   type Outcome is (Not_Started, Claimed, Completed, Handler_Missing,
      Resolution_Failed, Invocation_Failed, Cleanup_Pending, Cleanup_Complete, Field_Uncertain,
      Already_Consumed, Unknown_Controller, Disabled, Wrong_Sender, Wrong_Epoch,
      Invalid_Token, Duplicate, Token_Conflict, Old_Token, Busy, Scope_Changed, Context_Failed,
      Recovery_Required);
   type Report is record
      Status : Outcome := Not_Started;
      Resolution : Methods.Resolution_Report;
      Invocation : Methods.Invocation_Report;
      Field_Uncertainty : Boolean := False;
   end record;
   type Delivery is limited private;
   function Consumed (Run : Delivery) return Boolean;
   function Pending (Run : Delivery) return Boolean;
   function Observe (Run : Delivery) return Report;
   --  Exact original claimed event only. Unused/rejected deliveries have no
   --  receipt; redispatch never changes the original event. This attests claim,
   --  not successful AML, root cleanup, hardware quiescence or a new payload.
   function Receipt (Run : Delivery) return ACPI_EC_Event_Types.Notice_Option;
   --  Strictly increasing nonzero tokens per admitted epoch; gaps are legal.
   --  Duplicate/older events perform no AML. Consume before lookup/invocation,
   --  including missing handlers,zero fuel,and terminal failures. Rejected new
   --  deliveries remain unused and cannot advance the source high-water mark.
   procedure Dispatch
     (Run : in out Delivery; Service : aliased in out Core.State;
      Sender : CuBit.Process_IDs.Process_ID; Item : ACPI_EC_Event_Types.Notice;
      Budget : Natural; Result : out Outcome)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   type Close_Result is (Closed, Cleanup_Failed, No_Pending_Result, Wrong_Context, Receiver_Busy);
   procedure Close
     (Run : in out Delivery; Service : in out Core.State; Result : out Close_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   --  Pending counts retained result handles only. Field uncertainty holds the
   --  route independently and cannot be cleared by Close/revoke/re-enrollment.
private
   subtype Slot_Count is Natural range 0 .. Maximum_Controllers;
   subtype Slot_Index is Slot_Count range 1 .. Slot_Count'Last;
   type Route is record
      Logic : ACPI_EC_Event_Policy.State;
      Scope : Core.Node_Descriptor := Core.Values.No_Node;
   end record;
   type Route_Array is array (Slot_Index) of Route;
   Routes : Route_Array;
   In_Call : Boolean := False;
   Fatal_Seen : Boolean := False;
   Fatal_Info : AML_Fatal.Observation;
   type Delivery is limited record
      Used : Boolean := False;
      Slot : Slot_Count := 0;
      Event : ACPI_EC_Event_Types.Notice;
      Owner : Core.Node_Descriptor := Core.Values.No_Node;
      Handler : Methods.Invocation;
      Info : Report;
   end record;
end ACPI_EC_Event_Receiver;
