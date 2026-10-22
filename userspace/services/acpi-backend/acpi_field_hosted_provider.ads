pragma Ada_2022;
with Interfaces;
with AML_Identity;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_Region_Policy;
with ACPI_Region_IO;
-- Deterministic hosted adapter of the existing named-ID policy endpoint.
-- Not native transport and not an AML-installable authority interface.
generic
   type Hardware_State is private;
   with procedure Transact
     (Hardware : in out Hardware_State;
      Space : ACPI_Region_Policy.Address_Space; Address : Interfaces.Unsigned_64;
      Width : ACPI_Region_Policy.Access_Width; For_Write : Boolean;
      Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64;
      Completed : out Boolean);
package ACPI_Field_Hosted_Provider is
   type Context is limited private;
   procedure Install
     (IO : in out Context; Endpoint : AML_Field_Access.Endpoint_ID;
      Configuration : ACPI_Region_Policy.Configuration;
      Exact_Write_Safe : Boolean; Accepted : out Boolean);
   procedure Describe
     (IO : Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   function Identity (IO : Context) return AML_Identity.Identity;
   function Pending (IO : Context) return Boolean;
   function Uncertain (IO : Context) return AML_Field_Access.Observation;
   function Completed_Fault (IO : Context) return AML_Field_Access.Observation;
   function Observe_Hardware (IO : Context) return Hardware_State;
   procedure Dispatch
     (IO : in out Context; Item : AML_Field_Access.Request;
      Reply : out AML_Field_Access.Response);
   -- Trusted test controller alone supplies attributable completion evidence.
   -- Clearing a pending receipt does not reinstall/re-enable revoked policy.
   procedure Confirm_Completion
     (IO : in out Context; Expected : AML_Field_Access.Observation;
      Terminal_Evidence : Boolean; Accepted : out Boolean);
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
private
   -- Region_IO's model contracts require equality on the hardware state;
   -- the actual state is therefore nonlimited while Context remains limited.
   package Backend is new ACPI_Region_IO (Hardware_State, Transact);
   type Context is limited record
      Identity : AML_Identity.Identity := AML_Identity.No_Identity;
      Region : ACPI_Region_Policy.State;
      Hardware : Hardware_State;
      Endpoint : AML_Field_Access.Endpoint_ID := AML_Field_Access.No_Endpoint;
      Exact_Write_Safe : Boolean := False;
      Captured : AML_Field_Access.Observation;
      Confirmed : AML_Field_Access.Observation;
      Fault : AML_Field_Access.Observation;
      Pending_Ticket : Interfaces.Unsigned_64 := 0;
   end record;
end ACPI_Field_Hosted_Provider;
