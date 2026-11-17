pragma Ada_2022;
with Interfaces;
with ACPI_FADT;
with ACPI_Global_Lock_Protocol;
with AML_Identity;
-- Serialized local lease for an explicitly hardware-reduced platform. It
-- provides no memory/register authority and never emulates a firmware lock on
-- a conventional platform merely because the physical backend is unavailable.
package ACPI_Software_Global_Lock with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package P renames ACPI_Global_Lock_Protocol;
   use type P.Lock_Outcome, AML_Identity.Identity, Interfaces.Unsigned_64;
   subtype Microseconds is Interfaces.Unsigned_64;
   type Context is limited private;
   type Lease is limited private;
   type Report is record
      Configured, Stopping, Held, Clock_Broken : Boolean := False;
   end record;
   function Observe (State : Context) return Report with Global => null;
   function Needs_Cleanup (Item : Lease) return Boolean with Global => null;
   function Owns (State : Context; Item : Lease) return Boolean with Global => null;
   function Claim_Of (Item : Lease) return AML_Identity.Identity with Ghost, Global => null;
   function Claim_Of (State : Context) return AML_Identity.Identity with Ghost, Global => null;
   function Deadline_Of (Item : Lease) return Microseconds with Ghost, Global => null;
   procedure Configure (State : in out Context; Facts : ACPI_FADT.Description;
      Result : out P.Lock_Outcome) with
      Post => (if Result = P.Configured then Observe (State).Configured
        and then ACPI_FADT.Hardware_Reduced (Facts) and then not Observe (State).Held);
   -- Clock and deadline are the original trusted owner's microsecond domain.
   -- This unit does not open a window, wait, touch hardware or run callbacks.
   procedure Acquire (State : in out Context; Item : in out Lease;
      Now : Microseconds; Clock_Available : Boolean; Deadline : Microseconds;
      Result : out P.Lock_Outcome) with
      Post => (if Result = P.Acquired then Owns (State, Item)
        and then Needs_Cleanup (Item) and then Observe (State).Held
        and then Observe (State).Configured and then not Observe (State).Stopping
        and then Clock_Available and then Now < Deadline
        and then Deadline_Of (Item) = Deadline
      else Claim_Of (Item) = Claim_Of (Item)'Old
        and then Deadline_Of (Item) = Deadline_Of (Item)'Old
        and then Claim_Of (State) = Claim_Of (State)'Old
        and then Observe (State).Held = Observe (State).Held'Old);
   procedure Check_Access (State : in out Context; Item : Lease;
      Now : Microseconds; Clock_Available : Boolean; Result : out P.Lock_Outcome) with
      Post => (Needs_Cleanup (Item) = Needs_Cleanup (Item)'Old
        and then Observe (State).Held = Observe (State).Held'Old
        and then Claim_Of (State) = Claim_Of (State)'Old
        and then (if Result = P.Configured then Owns (State, Item)
          and then Clock_Available and then Now < Deadline_Of (Item)
          and then not Observe (State).Stopping and then not Observe (State).Clock_Broken));
   -- Original ownership suffices for cleanup, including after stop, clock
   -- failure or expiration. A foreign/stale lease cannot clear the owner.
   procedure Release (State : in out Context; Item : in out Lease;
      Result : out P.Lock_Outcome) with
      Post => (if Result = P.Released then not Needs_Cleanup (Item)
        and then not Observe (State).Held
        and then Claim_Of (State) = AML_Identity.No_Identity
        and then Claim_Of (Item) = AML_Identity.No_Identity
        and then Deadline_Of (Item) = 0
      else Observe (State) = Observe (State)'Old
        and then Claim_Of (State) = Claim_Of (State)'Old
        and then Claim_Of (Item) = Claim_Of (Item)'Old
        and then Deadline_Of (Item) = Deadline_Of (Item)'Old);
   procedure Begin_Stop (State : in out Context) with
      Post => Observe (State).Stopping
        and then Observe (State).Held = Observe (State).Held'Old;
private
   type Context is limited record
      Info : Report;
      Attempted, Seen_Clock : Boolean := False;
      Last_Clock : Microseconds := 0;
      Claim : AML_Identity.Identity := AML_Identity.No_Identity;
   end record;
   type Lease is limited record
      Claim : AML_Identity.Identity := AML_Identity.No_Identity;
      Deadline : Microseconds := 0;
   end record;
end ACPI_Software_Global_Lock;
