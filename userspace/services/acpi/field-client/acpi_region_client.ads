pragma Ada_2022;
with Interfaces;
with AML_Identity;
with ACPI_Region_Protocol;
-- Userspace client only. The transport maps an opaque endpoint to actual
-- scoped authority and calls down the service layering with a finite deadline.
-- It must authenticate and associate replies with this exact call, and never
-- retry an uncertain call. Times use the native monotonic millisecond clock.
generic
   type Transport_State is limited private;
   type Endpoint is private;
   No_Endpoint : Endpoint;
   with procedure Exchange
     (Transport : in out Transport_State; Destination : Endpoint;
      Deadline : ACPI_Region_Protocol.Time_Instant;
      Item : ACPI_Region_Protocol.Packet;
      Reply : out ACPI_Region_Protocol.Packet;
      Delivered : out ACPI_Region_Protocol.Delivery);
package ACPI_Region_Client with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type AML_Identity.Identity;
   use type Interfaces.Unsigned_64;
   type Request_ID is new Interfaces.Unsigned_64;
   No_Request : constant Request_ID := 0;
   type Observation is record
      Present : Boolean := False;
      -- Process-local context identity, not a capability or wire credential.
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Operation : Request_ID := No_Request;
      Item : ACPI_Region_Protocol.Register_Request;
   end record;
   No_Observation : constant Observation := (others => <>);
   type Completion is (Rejected, Completed, Failed_Completed, Indeterminate);
   type Result is record
      Status : Completion := Rejected;
      Operation : Request_ID := No_Request;
      Value : Interfaces.Unsigned_64 := 0;
   end record;
   type Context is limited private;
   type State_View is record
      Installed : Boolean;
      Identity : AML_Identity.Identity;
      Destination : Endpoint;
      High_Water : Request_ID;
      In_Flight : Boolean;
      Captured : Observation;
      Confirmed : Boolean;
   end record with Ghost;
   function State_Of (IO : Context) return State_View with Ghost, Global => null;
   function Coherent (IO : Context) return Boolean with Ghost, Global => null;
   function Dispatchable
     (IO : Context; Now, Deadline : ACPI_Region_Protocol.Time_Instant;
      Item : ACPI_Region_Protocol.Register_Request) return Boolean with Ghost, Global => null;
   function Initialize_Transition
     (Before, After : State_View; Destination : Endpoint; Accepted : Boolean)
      return Boolean with Ghost, Global => null;
   function Dispatch_Transition
     (Before, After : State_View; Now, Deadline : ACPI_Region_Protocol.Time_Instant;
      Item : ACPI_Region_Protocol.Register_Request; Reply : Result)
      return Boolean with Ghost, Global => null;
   function Confirm_Transition
     (Before, After : State_View; Expected : Observation; Evidence, Accepted : Boolean)
      return Boolean with Ghost, Global => null;
   function Retire_Transition
     (Before, After : State_View; Expected : Observation; Consumed, Accepted : Boolean)
      return Boolean with Ghost, Global => null;
   -- One installation for the context lifetime. No retarget/reset API.
   procedure Initialize
     (IO : in out Context; Destination : Endpoint; Accepted : out Boolean)
     with Post => Coherent (IO)
       and then Initialize_Transition (State_Of (IO)'Old, State_Of (IO), Destination, Accepted);
   function Pending (IO : Context) return Boolean;
   function Uncertain (IO : Context) return Observation;
   procedure Dispatch
     (IO : in out Context; Transport : in out Transport_State;
      Now, Deadline : ACPI_Region_Protocol.Time_Instant;
      Item : ACPI_Region_Protocol.Register_Request; Reply : out Result)
     with Post => Coherent (IO)
       and then Dispatch_Transition (State_Of (IO)'Old, State_Of (IO), Now, Deadline, Item, Reply);
   function Reconciled (IO : Context; Expected : Observation) return Boolean;
   -- Trusted controller supplies evidence for this exact observation.
   -- Confirming never sends IPC, reinstalls authority or replays a request.
   procedure Confirm_Completion
     (IO : in out Context; Expected : Observation;
      Terminal_Evidence : Boolean; Accepted : out Boolean)
     with Post => Coherent (IO)
       and then Confirm_Transition (State_Of (IO)'Old, State_Of (IO), Expected, Terminal_Evidence, Accepted)
       and then (if Accepted then Reconciled (IO, Expected));
   -- Retain the confirmation until its consumer has consumed it. The client
   -- cannot dispatch any further operation before this exact retirement.
   procedure Retire_Confirmation
     (IO : in out Context; Expected : Observation;
      Consumer_Reconciled : Boolean; Accepted : out Boolean)
     with Post => Coherent (IO)
       and then Retire_Transition (State_Of (IO)'Old, State_Of (IO), Expected, Consumer_Reconciled, Accepted)
       and then (if Accepted then not Pending (IO));
private
   function Internal_Coherent (IO : Context) return Boolean with Ghost, Global => null;
   type Context is limited record
      Installed : Boolean := False;
      Identity : AML_Identity.Identity := AML_Identity.No_Identity;
      Destination : Endpoint := No_Endpoint;
      High_Water : Request_ID := No_Request;
      In_Flight : Boolean := False;
      Captured : Observation;
      Confirmed : Boolean := False;
   end record with Type_Invariant => Internal_Coherent (Context);
end ACPI_Region_Client;
