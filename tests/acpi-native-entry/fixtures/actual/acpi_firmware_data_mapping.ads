pragma Ada_2022;
with Interfaces;
with ACPI_Firmware_Data_Arena;
with ACPI_Firmware_Data_Requests;
generic
   with package Arena is new ACPI_Firmware_Data_Arena (<>);
   Reservations : in out Arena.Context;
   type Capability is private;
   with procedure Inspect (Item : Capability; Evidence : out ACPI_Firmware_Data_Requests.Grant_Evidence);
   with procedure Map_Chunk (Held : Capability; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Completed : out Boolean);
   with procedure Clock (Now : out ACPI_Firmware_Data_Requests.IPC.Time_Instant; Available : out Boolean);
   Maximum_Per_Call : Positive;
-- Total extent/counts remain 64-bit; Maximum_Per_Call is the kernel syscall's
-- per-call bound, not a cap on a firmware declaration. No replay or release.
package ACPI_Firmware_Data_Mapping with SPARK_Mode is
   package R renames ACPI_Firmware_Data_Requests;
   package P renames R.P;
   subtype Word is Interfaces.Unsigned_64;
   type Outcome is (Mapped, Not_Attempted, Invalid_Request, Invalid_Reservation,
      Already_Attempted, Invalid_Deadline, Clock_Unavailable, Clock_Reversed,
      Expired, Invalid_Grant, Mapping_Indeterminate, Late_Completed, Reentered);
   type Report is record
      Status : Outcome := Not_Attempted;
      Calls, Confirmed_Pages : Word := 0;
      Reservation_Claimed : Boolean := False;
   end record;
   type Context is limited private;
   function Observe (Owner : Context) return Report;
   procedure Map_Exact (Owner : in out Context; Held : Capability; Item : Arena.Reservation;
      Physical, Virtual, Pages : Word; Deadline : R.IPC.Time_Instant; Result : out Report);
private
   type Context is limited record
      Working, Seen_Clock, Clock_Broken : Boolean := False;
      Last_Clock : R.IPC.Time_Instant := 0;
      Last : Report;
   end record;
end ACPI_Firmware_Data_Mapping;
