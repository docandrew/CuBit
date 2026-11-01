pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Bindings;
with ACPI_Backend_FACS_Binding;
with ACPI_Backend_Global_Signal;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Sessions;
-- Trusted singleton composition for one retained platform Global Lock domain.
-- No public IPC accepts addresses or these callback entry points. Hardware,
-- FACS and PM1 owners must be serialized and retained throughout every lease,
-- including failed/uncertain cleanup. Never instantiate competing owners for
-- aliases of the same physical FACS word; domain uniqueness is external.
generic
   with package Bindings is new ACPI_Backend_Bindings (<>);
   type Hardware_Context is limited private;
   with package FACS is new ACPI_Backend_FACS_Binding
     (Bindings => Bindings, Hardware_State => Hardware_Context, others => <>);
   with package Signaling is new ACPI_Backend_Global_Signal
     (Bindings => Bindings, Hardware_State => Hardware_Context, others => <>);
   Hardware : in out Hardware_Context;
   FACS_State : in out FACS.Context;
   Signal_State : in out Signaling.Context;
   type Principal is private;
   No_Principal : Principal;
   with procedure Read_Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
   Maximum_Attempts : Positive := 16;
   Maximum_Leases : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
package ACPI_Backend_Global_Lock is
   type Atomic_Outcome is (Exchanged, Not_Exchanged, Not_Attempted, Atomic_Unknown);
   type Signal_Outcome is (Complete, Not_Sent, Signal_Unknown);
   -- Only the software ownership engine uses these nonblocking callbacks.
   -- They resolve a fresh retained view per operation, never cache a pointer.
   package Callbacks is
      procedure Read_Word (Word : out Interfaces.Unsigned_32; Available : out Boolean);
      procedure Compare_Exchange
        (Expected, Replacement : Interfaces.Unsigned_32;
         Observed : out Interfaces.Unsigned_32; Result : out Atomic_Outcome);
      procedure Signal_Firmware (Result : out Signal_Outcome);
   end Callbacks;
   package Owner is new ACPI_Global_Lock_Owner
     (Principal, No_Principal, Read_Clock, Callbacks.Read_Word,
      Atomic_Outcome, Exchanged, Not_Exchanged, Not_Attempted, Atomic_Unknown,
      Callbacks.Compare_Exchange, Signal_Outcome, Complete, Not_Sent, Signal_Unknown,
      Callbacks.Signal_Firmware, Maximum_Attempts, Maximum_Leases);
   -- Instantiate alongside Owner so the same private principal/null actuals
   -- bind the retained request session without recreating its lease type.
   package Sessions is new ACPI_Global_Lock_Sessions (Principal, No_Principal, Owner);
end ACPI_Backend_Global_Lock;
