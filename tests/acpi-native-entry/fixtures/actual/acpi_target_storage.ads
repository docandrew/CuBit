pragma Ada_2022;
with ACPI_Capacity_Growth;
with System;
with Interfaces;
with ACPI_Capacity_Diagnostics;
generic
   type Element is private;
   Initial_Capacity : Positive := 16;
package ACPI_Target_Storage is
   subtype Count is ACPI_Capacity_Growth.Count;
   type Outcome is (Stored, Allocation_Refused, Count_Representation_Exhausted,
                    Byte_Representation_Exhausted);
   type Report is record
      Status : Outcome := Stored;
      Demand : Interfaces.Unsigned_64 := 0;
      Capacity : Count := 0;
      Requested_Bytes : Interfaces.Unsigned_64 := 0;
   end record;
   type Context is limited private;
   function Length (Store : Context) return Count;
   function Capacity (Store : Context) return Count;
   function Observe (Store : Context) return Report;
   function Capacity_Failure
     (Store : Context; Cache : ACPI_Capacity_Diagnostics.Cache_Kind)
      return ACPI_Capacity_Diagnostics.Report;
   function Read (Store : Context; Index : Positive) return Element
     with Pre => Index <= Length (Store);
   procedure Append
     (Store : in out Context; Item : Element;
      Index : out Count; Result : out Outcome);
   -- Process-lifetime owners keep storage until termination. Release is for
   -- an owner that has already invalidated all externally issued identities.
   procedure Release (Store : in out Context);
private
   type Context is limited record
      Base : System.Address := System.Null_Address;
      Reserved : Count := 0;
      Used : Count := 0;
      Last : Report;
   end record;
end ACPI_Target_Storage;
