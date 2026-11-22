pragma Ada_2022;
with Interfaces;
-- Passive diagnostics only. One serialized context; tokens stay with it.
-- No result, deadline, hardware authority or recovery policy depends on this.
package ACPI_Startup_Timing with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   type Kind is (Startup, Provider_Init, Snapshot_Load, Module_Execution, Work_Lookup, Work_Store, Work_Clone, Work_Allocation, Work_Snapshot,
      Inventory_Header, Inventory_Part, NVS_Header, NVS_Describe, NVS_Read,
      PCI_Header, PCI_Describe, PCI_Read, Scalar_Read, Scalar_Write,
      Lock_Acquire, Lock_Release, Lock_Other, Scoped_Select, Scoped_Read,
      Scoped_Query, Other_RPC, AML_Sleep, AML_Stall);
   function Name (Item : Kind) return String;
   function External (Item : Kind) return Boolean is (Item >= Inventory_Header);
   type Counters is record
      Calls, Samples, Total_US, Worst_US, External_US : Word := 0;
   end record;
   type Context is private;
   type Token is private;
   No_Token : constant Token;
   procedure Start (State : in out Context);
   function Active (State : Context) return Boolean;
   procedure Enter (State : in out Context; Item : Kind; Now : Word; Available : Boolean; Handle : out Token);
   procedure Leave (State : in out Context; Handle : in out Token; Now : Word; Available : Boolean);
   procedure Stop (State : in out Context);
   function Read (State : Context; Item : Kind) return Counters;
   function Complete (State : Context) return Boolean;
   function Open_Scopes (State : Context) return Natural;
   function Clock_Samples (State : Context) return Word;
   function Classify (Label : Interfaces.Unsigned_32) return Kind;
private
   Maximum_Depth : constant := 16;
   subtype Depth is Natural range 0 .. Maximum_Depth;
   subtype Index is Depth range 1 .. Maximum_Depth;
   type Token is record Serial : Word := 0; Slot : Depth := 0; end record;
   No_Token : constant Token := (0,0);
   type Frame is record
      Serial : Word := 0;
      Item : Kind := Startup;
      Began, External_Before : Word := 0;
      Valid : Boolean := False;
   end record;
   type Stack is array (Index) of Frame;
   type Entries is array (Kind) of Counters;
   type Context is record
      Started, Recording, Seen_Clock, Clock_Broken, Malformed, Saturated : Boolean := False;
      Last, Serial, External_Total, External_Began, Clock_Reads : Word := 0;
      Used, External_Depth : Depth := 0;
      Frames : Stack;
      Items : Entries;
   end record;
end ACPI_Startup_Timing;
