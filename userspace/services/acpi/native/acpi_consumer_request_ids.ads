pragma Ada_2022;
-- Both observers are total and side-effect-free for every initialized State;
-- eager snapshots remain valid in the conditional transition contract.
pragma Unevaluated_Use_Of_Old (Allow);
with Interfaces;
package ACPI_Consumer_Request_IDs with Pure, SPARK_Mode is
   type Request_ID is new Interfaces.Unsigned_64;
   No_Request : constant Request_ID := 0;
   GPIO_Admission_Request : constant Request_ID := 1;
   Initialization_Completion_Request : constant Request_ID := 2;
   First_Dynamic_Request : constant Request_ID := 3;
   subtype Dynamic_Request_ID is Request_ID range First_Dynamic_Request .. Request_ID'Last;
   type Cursor is record
      Next : Dynamic_Request_ID := First_Dynamic_Request;
      Exhausted : Boolean := False;
   end record;
   -- A total boundary operation; no wrap even at the last 64-bit ID.
   function After_Allocation (Allocated : Dynamic_Request_ID) return Cursor
     with Global => null,
       Post => After_Allocation'Result.Exhausted = (Allocated = Request_ID'Last)
         and then After_Allocation'Result.Next =
           (if Allocated = Request_ID'Last then Allocated else Allocated + 1);
   -- The native producer must own exactly one process-lived State for every
   -- later consumer request, including failed/unknown calls. Allocating another
   -- State would start another sequence; this unit does not certify ownership.
   type State is limited private;
   function Exhausted (Run : State) return Boolean with Global => null;
   function Next_Value (Run : State) return Dynamic_Request_ID with Global => null;
   procedure Take (Run : in out State; Request : out Request_ID; Granted : out Boolean)
     with Global => null,
       Depends => (Run => Run, Request => Run, Granted => Run),
       Post => Granted = not Exhausted (Run)'Old
         and then
           (if Exhausted (Run)'Old then
              Request = No_Request and then Exhausted (Run)
              and then Next_Value (Run) = Next_Value (Run)'Old
            else Request = Next_Value (Run)'Old
              and then Exhausted (Run) = (Next_Value (Run)'Old = Request_ID'Last)
              and then Next_Value (Run) =
                (if Next_Value (Run)'Old = Request_ID'Last
                 then Next_Value (Run)'Old else Next_Value (Run)'Old + 1));
private
   type State is limited record
      Position : Cursor;
   end record;
end ACPI_Consumer_Request_IDs;
