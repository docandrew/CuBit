pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Mapping_Chunks;
-- Trusted startup only. Adapts an immutable startup deadline to the existing
-- Map_Memory callback shape. No IPC admission, VA reservation or new authority.
-- The binding owner independently validates exact geometry/rights and retains
-- handles/VA extents. Callbacks return normally and do not reenter.
generic
   type Authority is private;
   No_Authority : Authority;
   with procedure Map_Chunk
     (Held : Authority; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean; Completed : out Boolean);
   with procedure Read_Clock
     (Now : out ACPI_Backend_Binding_Types.Tick; Available : out Boolean);
   Maximum_Windows, Maximum_Pages, Maximum_Per_Call : Positive;
   Physical_Limit, Virtual_Limit : Interfaces.Unsigned_64;
package ACPI_Backend_Mapping_Startup with SPARK_Mode is
   package B renames ACPI_Backend_Binding_Types;
   package Chunks is new ACPI_Backend_Mapping_Chunks
     (Authority, No_Authority, Map_Chunk, Read_Clock,
      Maximum_Pages, Maximum_Per_Call, Physical_Limit, Virtual_Limit);
   subtype Window_Count is Natural range 0 .. Maximum_Windows;
   type Phase is (Fresh, Open, Sealed, Failed);
   type Outcome is (Configured, Window_Mapped, Not_Configured,
     Already_Configured, Invalid_Deadline, Missing_Authority,
     Duplicate_Authority, Capacity_Exceeded, Mapping_Failed,
     Startup_Sealed, Startup_Failed, Reentered);
   type Report is record
      Status : Outcome := Not_Configured;
      Slot : Window_Count := 0;
      Mapping : Chunks.Outcome := Chunks.Not_Attempted;
   end record;
   type Statistics is record
      Stage : Phase := Fresh;
      Stored, Completed : Window_Count := 0;
      Working : Boolean := False;
   end record;
   type Context is limited private;
   function Observe (State : Context) return Statistics;
   procedure Configure (State : in out Context; Deadline : B.Tick; Result : out Outcome);
   -- No deadline parameter here: every window uses the one captured above.
   -- A failed/invalid operation closes further mapping across this session.
   -- Successful old mappings and all backing are retained, not released.
   procedure Map_Window
     (State : in out Context; Held : Authority;
      Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean; Result : out Report);
   procedure Seal (State : in out Context; Accepted : out Boolean);
   -- Seal closes mapping only. The caller must gate catalog publication on
   -- complete startup success; this owner cannot revoke another owner's views.
private
   subtype Window_Index is Window_Count range 1 .. Window_Count'Last;
   type Slot is limited record
      Held : Authority := No_Authority;
      Mapping : Chunks.Context;
   end record;
   type Slots is array (Window_Index) of Slot;
   type Context is limited record
      Stage : Phase := Fresh;
      Working : Boolean := False;
      Deadline : B.Tick := B.No_Finite_Deadline;
      Used, Completed : Window_Count := 0;
      Items : Slots;
   end record;
end ACPI_Backend_Mapping_Startup;
