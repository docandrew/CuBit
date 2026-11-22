pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Catalog;
with ACPI_Inventory_Protocol;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
-- One lexical hardware owner, no peer, transport, packet loop or caller VA.
generic
   with package Catalog is new ACPI_Backend_Catalog (<>);
   Hardware : in out Catalog.Context;
   Scope : Interfaces.Unsigned_64;
   Maximum_Registers : Positive := AML_Field_Access.Default_Maximum_Registers;
package ACPI_Local_Register_Provider with SPARK_Mode is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   type Phase is (Fresh, Initializing, Ready, Failed);
   type Context is limited private;
   function Status (IO : Context) return Phase;
   function Is_Ready (IO : Context) return Boolean;
   function Identity (IO : Context) return AML_Identity.Identity;
   function Pending (IO : Context) return Boolean;
   function Uncertain (IO : Context) return A.Observation;
   function Completed_Fault (IO : Context) return A.Observation;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   -- Scope is established by startup, never by AML/external messages. Copy
   -- sealed catalog metadata once; no partial cache is exposed on failure.
   -- Any catalog revision change makes this context stale. No reset/rebind
   -- discards observations; hotplug must retain the old owner and establish
   -- a separate checked generation at a safe boundary.
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target (IO : Context; Ordinal : Positive;
      Target : out A.Target; Accepted : out Boolean);
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response);
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean);
   -- No speculative terminal-evidence Boolean. An indeterminate local access
   -- retains its observation and underlying catalog reservation. Recovery
   -- needs the actual local hardware owner's authoritative cleanup mechanism;
   -- this scalar provider cannot manufacture it or replay the operation.
private
   subtype Index is Positive range 1 .. Maximum_Registers;
   type Register_Entry is record
      Description : ACPI_Inventory_Protocol.Description;
      Geometry : L.Register_Geometry;
   end record;
   type Entries is array (Index) of Register_Entry;
   type Context is limited record
      Stage : Phase := Fresh;
      Symbol : A.Endpoint_ID := A.No_Endpoint;
      Issued : AML_Identity.Identity := AML_Identity.No_Identity;
      Revision : Catalog.Revision_Number := Catalog.No_Revision;
      Count : Natural range 0 .. Maximum_Registers := 0;
      Items : Entries;
      Working : Boolean := False;
      Sequence : A.Operation_ID := A.No_Operation;
      Captured, Fault : A.Observation;
      Hardware_Ticket : Interfaces.Unsigned_64 := 0;
   end record;
end ACPI_Local_Register_Provider;
