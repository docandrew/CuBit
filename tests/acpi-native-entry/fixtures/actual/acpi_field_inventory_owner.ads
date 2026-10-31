pragma Ada_2022;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_Inventory_Client;
with ACPI_Inventory_Protocol;
with ACPI_Field_Remote_Provider;
generic
   type Transport_State is limited private;
   type Endpoint is private;
   No_Endpoint : Endpoint;
   with procedure Read_Clock
     (Now : out ACPI_Inventory_Protocol.Wire.Time_Instant; Available : out Boolean);
   with procedure Exchange
     (Transport : in out Transport_State; Destination : Endpoint;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      Item : ACPI_Inventory_Protocol.Wire.Packet;
      Reply : out ACPI_Inventory_Protocol.Wire.Packet;
      Delivered : out ACPI_Inventory_Protocol.Wire.Delivery);
   with procedure Read_Window
     (Now, Deadline : out ACPI_Inventory_Protocol.Wire.Time_Instant);
   Maximum_Registers : Positive := AML_Field_Access.Default_Maximum_Registers;
   Maximum_Confirmations : Positive := AML_Field_Access.Default_Maximum_Confirmations;
package ACPI_Field_Inventory_Owner is
   type Startup_State is (Fresh, Initializing, Ready, Failed);
   type Context is limited private;
   function Status (IO : Context) return Startup_State;
   -- One lifetime attempt. Keep the entire lower provider hidden until the
   -- bounded inventory copy, descriptor validation and all installs succeed.
   -- The same Destination is used for discovery and scalar operations.
   procedure Initialize
     (IO : in out Context; Destination : Endpoint;
      Symbol : AML_Field_Access.Endpoint_ID;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant; Accepted : out Boolean);
   function Identity (IO : Context) return AML_Identity.Identity;
   -- Bounded metadata export for early module Field_IO. A target remains
   -- subject to lower provider/backend checks; no new authority is minted.
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target
     (IO : Context; Ordinal : Positive; Target : out AML_Field_Access.Target; Accepted : out Boolean);
   -- Match the exact admitted transaction, including space and scalar width.
   -- Enclosing region metadata may differ only when both contain that same
   -- complete transaction. Canonical provider geometry remains unchanged. AML facts
   -- select copied metadata; they cannot create or alter backend authority.
   -- Multiple usable IDs for the same geometry are refused as ambiguous.
   procedure Select_Target
     (IO : Context; Geometry : AML_Field_Layout.Register_Geometry;
      For_Write : Boolean; Target : out AML_Field_Access.Target; Accepted : out Boolean);
   procedure Describe
     (IO : Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   procedure Authorize
     (IO : Context; Target : AML_Field_Access.Target; For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch
     (IO : in out Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response);
   procedure Revoke_Register
     (IO : in out Context; Target : AML_Field_Access.Target; Accepted : out Boolean);
   function Pending (IO : Context) return Boolean;
   function Uncertain (IO : Context) return AML_Field_Access.Observation;
   function Completed_Fault (IO : Context) return AML_Field_Access.Observation;
   procedure Confirm_Completion
     (IO : in out Context; Expected : AML_Field_Access.Observation;
      Terminal_Evidence : Boolean; Accepted : out Boolean);
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
   procedure Retire_Confirmation
     (IO : in out Context; Expected : AML_Field_Access.Observation;
      Namespace_Reconciled : Boolean; Accepted : out Boolean);
private
   package Inventory is new ACPI_Inventory_Client
     (Transport_State, Endpoint, No_Endpoint, Read_Clock, Exchange, Maximum_Registers);
   package Provider is new ACPI_Field_Remote_Provider
     (Transport_State, Endpoint, No_Endpoint, Exchange, Read_Window,
      Maximum_Registers, Maximum_Confirmations);
   type Descriptors is array (Inventory.Register_Index) of Provider.Descriptor;
   type Context is limited record
      Stage : Startup_State := Fresh;
      Symbol : AML_Field_Access.Endpoint_ID := AML_Field_Access.No_Endpoint;
      Lower : Provider.Context;
      Metadata : Transport_State;
      Count : Inventory.Register_Count := 0;
      Entries : Descriptors;
   end record;
end ACPI_Field_Inventory_Owner;
