pragma Ada_2022;
with Interfaces;
with AML_Identity;
with ACPI_Target_Storage;
generic
   Initial_Reservations : Positive := 1;
-- One retained owner for the exclusive unused tail of a sealed backend device
-- arena. Configure is trusted process-local handoff, never a wire address API.
-- No owned-memory/DMA reservation is used, no reservation is released/reused.
package ACPI_Firmware_Data_Arena is
   subtype Word is Interfaces.Unsigned_64;
   type Context is limited private;
   type Reservation is private;
   No_Reservation : constant Reservation;
   type Outcome is (Configured, Reserved, Claimed, Invalid_Arena, Already_Configured,
      Not_Ready, Invalid_Size, Arena_Exhausted, Allocation_Refused,
      Identity_Exhausted, Wrong_Reservation, Already_Attempted, Reentered);
   type Statistics is record
      Configured, Working : Boolean;
      First, Next, Limit : Word;
      Reservations, Capacity : Natural;
   end record;
   function Observe (Owner : Context) return Statistics;
   procedure Configure (Owner : in out Context; First, Limit : Word; Result : out Outcome);
   procedure Reserve (Owner : in out Context; Pages : Word; Item : out Reservation; Result : out Outcome);
   procedure Describe (Owner : Context; Item : Reservation; Base, Pages : out Word; Accepted : out Boolean);
   -- Must precede the first possible kernel mapping. A claimed ticket can
   -- still be described but cannot authorize a second map attempt.
   procedure Claim (Owner : in out Context; Item : Reservation; Accepted : out Boolean);
private
   type Entry_Record is record
      Base, Pages : Word := 0;
      Attempted : Boolean := False;
   end record;
   package Store is new ACPI_Target_Storage (Entry_Record, Initial_Reservations);
   type Reservation is record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Index : Natural := 0;
   end record;
   No_Reservation : constant Reservation := (others => <>);
   type Context is limited record
      Consumed, Ready, Working : Boolean := False;
      Identity : AML_Identity.Identity := AML_Identity.No_Identity;
      First, Cursor, Limit : Word := 0;
      Items : Store.Context;
   end record;
end ACPI_Firmware_Data_Arena;
