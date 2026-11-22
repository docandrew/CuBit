pragma Ada_2022;
with Interfaces;
-- Diagnostic provenance only. The ordered fingerprint is not a capability,
-- an integrity/authentication primitive or a proof of firmware side effects.
generic
   Maximum_Invocations : Positive;
package ACPI_Initializer_History with Pure, SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type Invocation_Number is new Natural;
   subtype Invocation_Count is Invocation_Number range 0 .. Invocation_Number (Maximum_Invocations);
   type Node_Position is new Interfaces.Unsigned_32;
   subtype Method_Position is Node_Position range 1 .. Node_Position'Last;
   type Outcome_Code is new Interfaces.Unsigned_16;
   type Fuel_Charge is new Interfaces.Unsigned_64;
   subtype Digest_Value is Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_64;
   type Invocation is record
      Scope : Node_Position := 0;
      Method : Method_Position := 1;
      Access_Result, Execution : Outcome_Code := 0;
      Reported_Charge : Fuel_Charge := 0;
   end record;
   -- FNV-1a64, canonical version 1: seed below; each invocation contributes
   -- scope:u32, method:u32, access:u16, execution:u16, charge:u64, in this order
   -- and little-endian. The fixed 20-byte entry format preserves ordering.
   Encoding_Version : constant := 1;
   Empty_Digest : constant Digest_Value := 16#CBF2_9CE4_8422_2325#;
   function Next_Digest (Previous : Digest_Value; Item : Invocation) return Digest_Value
      with Global => null;
   type State is private with Default_Initial_Condition => Initially_Empty (State);
   function Count (Run : State) return Invocation_Count with Global => null;
   function Digest (Run : State) return Digest_Value with Global => null;
   function Complete (Run : State) return Boolean with Global => null;
   procedure Invalidate (Run : in out State)
      with Global => null, Post => not Complete (Run)
        and then Count (Run) = Count (Run)'Old and then Digest (Run) = Digest (Run)'Old;
   function Initially_Empty (Run : State) return Boolean with Ghost,
      Post => Initially_Empty'Result = (Count (Run) = 0 and then Complete (Run)
        and then Digest (Run) = Empty_Digest);
   -- No reset/rearm. Exhaustion preserves the recorded prefix and permanently
   -- marks it incomplete, so callers cannot attest a truncated history.
   procedure Append (Run : in out State; Item : Invocation; Accepted : out Boolean)
      with Global => null,
      Contract_Cases =>
        (not Complete (Run) => not Accepted and then Run = Run'Old,
         Complete (Run) and then Count (Run) = Invocation_Count'Last =>
            not Accepted and then not Complete (Run)
            and then Count (Run) = Count (Run)'Old and then Digest (Run) = Digest (Run)'Old,
         others => Accepted and then Complete (Run)
            and then Count (Run) = Count (Run)'Old + 1
            and then Digest (Run) = Next_Digest (Digest (Run)'Old, Item));
private
   type State is record
      Used : Invocation_Count := 0;
      Ordered : Digest_Value := Empty_Digest;
      All_Observed : Boolean := True;
   end record;
   function Count (Run : State) return Invocation_Count is (Run.Used);
   function Digest (Run : State) return Digest_Value is (Run.Ordered);
   function Complete (Run : State) return Boolean is (Run.All_Observed);
   function Initially_Empty (Run : State) return Boolean is
     (Run.Used = 0 and then Run.All_Observed and then Run.Ordered = Empty_Digest);
end ACPI_Initializer_History;
