pragma Ada_2022;
with Interfaces;
package ACPI_Backend_Binding_Types with Pure, SPARK_Mode is
   use Interfaces;
   -- Local monotonic microseconds for startup; not an IPC millisecond deadline.
   type Tick is new Unsigned_64;
   No_Finite_Deadline : constant Tick := Tick'Last;
   User_Virtual_Limit : constant Unsigned_64 := 16#0000_8000_0000_0000#;
   type Resource_Kind is (No_Resource, Memory_Resource, Port_Resource);
   -- Description from the current process's actual kernel handle, not PVAL.
   type Description is record
      Current : Boolean := False;
      Kind : Resource_Kind := No_Resource;
      Base, Length : Unsigned_64 := 0;
      Readable, Writable, Delegable : Boolean := False;
   end record;
   type Outcome is
     (Bound, Invalid_Description, Invalid_Arena, Unsupported_Memory_Rights,
      Missing_Handle, Duplicate_ID, Overlapping_Window, Capacity_Exceeded, Mapping_Limit,
      Arena_Exhausted, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Not_Current, Wrong_Kind, Range_Mismatch, Rights_Mismatch,
      Mapping_Refused, Reentered);
   type Report is record
      Status : Outcome := Invalid_Description;
      Stored, Map_Attempted : Boolean := False;
   end record;
end ACPI_Backend_Binding_Types;
