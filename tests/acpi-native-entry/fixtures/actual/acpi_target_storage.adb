pragma Ada_2022;
with ACPI_Target_Heap;
with System.Storage_Elements;
package body ACPI_Target_Storage is
   type Elements is array (Positive range <>) of Element;
   use System.Storage_Elements;
   use type System.Address;
   use type Interfaces.Unsigned_64;
   -- Actuals are plain metadata records. No controlled/access-bearing
   -- element or borrowed element pointer is supported at this boundary.
   Element_Bytes : constant Storage_Count :=
     (Elements'Component_Size + System.Storage_Unit - 1) / System.Storage_Unit;
   function Length (Store : Context) return Count is (Store.Used);
   function Capacity (Store : Context) return Count is (Store.Reserved);
   function Observe (Store : Context) return Report is (Store.Last);
   function Capacity_Failure
     (Store : Context; Cache : ACPI_Capacity_Diagnostics.Cache_Kind)
      return ACPI_Capacity_Diagnostics.Report is
      package D renames ACPI_Capacity_Diagnostics;
   begin
      if Store.Last.Status = Stored then return D.No_Report; end if;
      return (Cache => Cache,
        Failure => (case Store.Last.Status is
          when Allocation_Refused => D.Process_Heap,
          when Count_Representation_Exhausted => D.Entry_Representation,
          when Byte_Representation_Exhausted => D.Byte_Representation,
          when Stored => D.No_Failure),
        Demand => D.Entry_Count (Store.Last.Demand),
        Capacity => D.Entry_Count (Store.Last.Capacity),
        Entry_Limit => D.Entry_Count (Count'Last),
        Requested_Bytes => D.Byte_Count (Store.Last.Requested_Bytes),
        Byte_Limit => D.Byte_Count (Storage_Count'Last));
   end Capacity_Failure;
   function Read (Store : Context; Index : Positive) return Element is
      Items : Elements (1 .. Store.Reserved) with Import, Address => Store.Base;
   begin return Items (Index); end Read;
   procedure Replace (Store : in out Context; Index : Positive; Item : Element) is
      Items : Elements (1 .. Store.Reserved) with Import, Address => Store.Base;
   begin Items (Index) := Item; end Replace;
   procedure Append
     (Store : in out Context; Item : Element;
      Index : out Count; Result : out Outcome) is
      Replacement : System.Address;
      Needed, Next : Count;
   begin
      Index := 0;
      if Store.Used = Count'Last then
         Result := Count_Representation_Exhausted;
         Store.Last := (Result, Interfaces.Unsigned_64 (Store.Used) + 1, Store.Reserved, 0); return;
      end if;
      Needed := Store.Used + 1;
      if Needed > Store.Reserved then
         Next := ACPI_Capacity_Growth.Next_Capacity
           (Store.Reserved, Needed, Initial_Capacity);
         if Element_Bytes = 0 or else
           Storage_Count (Next) > Storage_Count'Last / Element_Bytes
         then
            Result := Byte_Representation_Exhausted;
            Store.Last := (Result, Interfaces.Unsigned_64 (Needed), Store.Reserved, 0); return;
         end if;
         Replacement := ACPI_Target_Heap.Allocate
           (Storage_Count (Next) * Element_Bytes, Element'Alignment);
         if Replacement = System.Null_Address then
            Result := Allocation_Refused;
            Store.Last := (Result, Interfaces.Unsigned_64 (Needed), Store.Reserved,
              Interfaces.Unsigned_64 (Storage_Count (Next) * Element_Bytes)); return;
         end if;
         declare
            Fresh : Elements (1 .. Next) with Import, Address => Replacement;
            Old : Elements (1 .. Store.Reserved) with Import, Address => Store.Base;
         begin
            if Store.Used > 0 then Fresh (1 .. Store.Used) := Old (1 .. Store.Used); end if;
         end;
         ACPI_Target_Heap.Release (Store.Base);
         Store.Base := Replacement; Store.Reserved := Next;
      end if;
      declare Items : Elements (1 .. Store.Reserved) with Import, Address => Store.Base; begin
         Items (Needed) := Item;
      end;
      Store.Used := Needed; Index := Needed; Result := Stored;
      Store.Last := (Stored, Interfaces.Unsigned_64 (Needed), Store.Reserved, 0);
   end Append;
   procedure Release (Store : in out Context) is
   begin
      ACPI_Target_Heap.Release (Store.Base);
      Store.Base := System.Null_Address; Store.Reserved := 0;
      Store.Used := 0; Store.Last := (others => <>);
   end Release;
end ACPI_Target_Storage;
