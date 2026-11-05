pragma Ada_2022;
with Interfaces;
package ACPI_Global_Lock_Policy with SPARK_Mode, Pure is
   subtype Lock_Word is Interfaces.Unsigned_32;
   use type Lock_Word;
   Pending_Bit : constant Lock_Word := 1;
   Owned_Bit : constant Lock_Word := 2;
   Protocol_Bits : constant Lock_Word := Pending_Bit or Owned_Bit;
   Reserved_Bits : constant Lock_Word := not Protocol_Bits;
   type Acquire_Proposal is record
      Replacement : Lock_Word;
      Would_Acquire : Boolean;
   end record;
   -- Predictions only. Ownership exists only after a real successful atomic
   -- compare/exchange against the same observed DWORD. Failed CAS must reload.
   function Propose_Acquire (Observed : Lock_Word) return Acquire_Proposal
     with Post =>
       (Propose_Acquire'Result.Replacement and Reserved_Bits) = (Observed and Reserved_Bits)
       and then (Propose_Acquire'Result.Replacement and Owned_Bit) /= 0
       and then Propose_Acquire'Result.Would_Acquire = ((Observed and Owned_Bit) = 0)
       and then ((Propose_Acquire'Result.Replacement and Pending_Bit) /= 0) =
         ((Observed and Owned_Bit) /= 0);
   type Release_Proposal is record
      Replacement : Lock_Word;
      Signal_Required : Boolean;
   end record;
   -- Caller must independently own the scoped software token. This bit test
   -- alone never authenticates the caller or licenses a release.
   function Propose_Release (Observed : Lock_Word) return Release_Proposal
     with Pre => (Observed and Owned_Bit) /= 0,
       Post =>
         (Propose_Release'Result.Replacement and Protocol_Bits) = 0
         and then (Propose_Release'Result.Replacement and Reserved_Bits) = (Observed and Reserved_Bits)
         and then Propose_Release'Result.Signal_Required = ((Observed and Pending_Bit) /= 0);
end ACPI_Global_Lock_Policy;
