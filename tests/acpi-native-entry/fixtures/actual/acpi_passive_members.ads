pragma Ada_2022;
with ACPI_Service_Core;
with AML_Decode;
with Firmware_Tables;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Passive_Members with SPARK_Mode is
   use type Firmware_Tables.Byte;
   type Member_Name is (Hardware_ID, Compatible_ID, Unique_ID, Address, Current_Resources, Device_Status);
   type Outcome is (Not_Observed, Static_Value, Method_Valued, Missing,
      Unsupported, Invalid_Device, Unavailable, Capacity_Required,
      Size_Overflow, Read_Failed, Cleanup_Failed);
   type Atom_Kind is (No_Atom, Integer_Atom, Text_Atom);
   type Atom is record
      Kind : Atom_Kind := No_Atom;
      Number : AML_Decode.Integer_Value := 0;
      Offset, Length : Natural := 0;
   end record;
   type Atoms is array (Natural range <>) of Atom;
   type Report is record
      Status : Outcome := Not_Observed;
      Required_Atoms, Required_Bytes, Copied_Atoms, Copied_Bytes : Natural := 0;
      Size_Known : Boolean := False;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
   end record;
   -- Fixed, direct members only. Never invokes a method, evaluates a Field,
   -- dereferences a returned reference handle, runs initialization or confers
   -- HW authority. Core may resolve passive named metadata while reading data.
   -- Integer/text atoms describe HID/CID/UID; ADR and STA accept integers only.
   -- Missing STA stays Missing; the query adapter owns the explicit default
   -- marker. Zero is an observed integer, never mistaken for missing.
   -- CID alone accepts a flat
   -- package of integer/text atoms. CRS copies a buffer verbatim, with zero
   -- atoms; framing and resource authorization are separate owner operations.
   -- Text offsets are zero-based in Data's copied prefix; values are raw facts,
   -- not a claim of valid ID syntax, device presence or initialized resources.
   -- Caller storage bounds package traversal. If atom capacity is insufficient,
   -- Required_Atoms is known but Size_Known is false (no partial size claim).
   -- Owned roots are released before return, or Cleanup_Failed is reported.
   -- Owner must serialize calls
   -- with namespace/AML mutation; no borrowed handle escapes this operation.
   procedure Read
     (Service : in out Core.State; Device : Core.Node_Descriptor; Member : Member_Name;
      Data : out Firmware_Tables.Bytes; Items : out Atoms; Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Result.Status = Static_Value then Result.Size_Known
         and then Result.Copied_Atoms = Result.Required_Atoms
         and then Result.Copied_Bytes = Result.Required_Bytes)
       and then (if Result.Status /= Static_Value then
         Result.Copied_Atoms = 0 and then Result.Copied_Bytes = 0
         and then (for all Byte of Data => Byte = 0)
         and then (for all Item of Items => Item.Kind = No_Atom));
end ACPI_Passive_Members;
