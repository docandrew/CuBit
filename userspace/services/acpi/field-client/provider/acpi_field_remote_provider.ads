pragma Ada_2022;
with Interfaces;
with AML_Identity;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_Region_Protocol;
with ACPI_Region_Client;
-- Trusted association cache and AML adapter, never hardware authority.
-- Backend admission remains independent of every descriptor installed here.
generic
   type Transport_State is limited private;
   type Endpoint is private;
   No_Endpoint : Endpoint;
   with procedure Exchange
     (Transport : in out Transport_State; Destination : Endpoint;
      Deadline : ACPI_Region_Protocol.Time_Instant;
      Item : ACPI_Region_Protocol.Packet; Reply : out ACPI_Region_Protocol.Packet;
      Delivered : out ACPI_Region_Protocol.Delivery);
   with procedure Read_Window
     (Now, Deadline : out ACPI_Region_Protocol.Time_Instant);
   Maximum_Registers : Positive := AML_Field_Access.Default_Maximum_Registers;
   Maximum_Confirmations : Positive := AML_Field_Access.Default_Maximum_Confirmations;
package ACPI_Field_Remote_Provider with SPARK_Mode is
   type Descriptor is record
      Register : AML_Field_Access.Register_ID := AML_Field_Access.No_Register;
      Epoch : AML_Field_Access.Counter := 0;
      Geometry : AML_Field_Layout.Register_Geometry;
      Readable, Writable, Exact_Write_Safe : Boolean := False;
      Write_Mask : Interfaces.Unsigned_64 := 0;
   end record;
   function Valid (Item : Descriptor) return Boolean with Global => null;
   type Context is limited private;
   procedure Initialize
     (IO : in out Context; Destination : Endpoint;
      Symbol : AML_Field_Access.Endpoint_ID; Accepted : out Boolean);
   -- Used named slots retain epoch high-water marks. Replacement requires
   -- revocation and a strictly newer nonzero epoch, never a reset.
   procedure Install (IO : in out Context; Item : Descriptor; Accepted : out Boolean);
   procedure Revoke_Register
     (IO : in out Context; Target : AML_Field_Access.Target; Accepted : out Boolean);
   function Identity (IO : Context) return AML_Identity.Identity;
   function Pending (IO : Context) return Boolean;
   function Uncertain (IO : Context) return AML_Field_Access.Observation;
   function Completed_Fault (IO : Context) return AML_Field_Access.Observation;
   procedure Describe
     (IO : Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   procedure Authorize
     (IO : Context; Target : AML_Field_Access.Target;
      For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch
     (IO : in out Context; Item : AML_Field_Access.Request;
      Reply : out AML_Field_Access.Response);
   procedure Confirm_Completion
     (IO : in out Context; Expected : AML_Field_Access.Observation;
      Terminal_Evidence : Boolean; Accepted : out Boolean);
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
   procedure Retire_Confirmation
     (IO : in out Context; Expected : AML_Field_Access.Observation;
      Namespace_Reconciled : Boolean; Accepted : out Boolean);
private
   package Client is new ACPI_Region_Client
     (Transport_State, Endpoint, No_Endpoint, Exchange);
   type Slot_Count is new Natural range 0 .. Maximum_Registers;
   subtype Slot_Index is Slot_Count range 1 .. Slot_Count'Last;
   type Entry_State is record
      Used, Active : Boolean := False;
      Item : Descriptor;
   end record;
   type Catalog is array (Slot_Index) of Entry_State;
   type Confirmation_Count is new Natural range 0 .. Maximum_Confirmations;
   subtype Confirmation_Index is Confirmation_Count range 1 .. Confirmation_Count'Last;
   type History is array (Confirmation_Index) of AML_Field_Access.Observation;
   type Context is limited record
      Initialized : Boolean := False;
      Identity : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : AML_Field_Access.Endpoint_ID := AML_Field_Access.No_Endpoint;
      Lower : Client.Context;
      Transport : Transport_State;
      Entries : Catalog;
      Dispatching : Boolean := False;
      Captured, Fault : AML_Field_Access.Observation;
      Lower_Receipt : Client.Observation;
      Confirmed : History;
      Reserved : Confirmation_Count := 0;
   end record;
end ACPI_Field_Remote_Provider;
