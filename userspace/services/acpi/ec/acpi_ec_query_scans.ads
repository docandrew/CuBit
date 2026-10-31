pragma Ada_2022;
with ACPI_EC_Types;
-- Logical allowance accounting only. Hardware completion, publication and
-- source provenance are independently established by the serialized owner.
package ACPI_EC_Query_Scans with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type ACPI_EC_Types.Work_Budget;
   subtype Allowance is ACPI_EC_Types.Work_Budget
     range 1 .. ACPI_EC_Types.Work_Budget'Last;
   type Phase is (Dormant, Ready, Waiting, Exhausted, Failed, Complete);
   type Empty_Response is (No_Empty_Response, Status_Clear, Zero_Response);
   subtype Observed_Empty is Empty_Response range Status_Clear .. Zero_Response;
   type Description is record
      Stage : Phase := Dormant;
      Remaining : ACPI_EC_Types.Work_Budget := 0;
      Empty : Empty_Response := No_Empty_Response;
   end record;
   type State is private with Default_Initial_Condition => Valid (State);
   function View (Item : State) return Description with Global => null;
   function Valid (Item : State) return Boolean is
     ((if View (Item).Stage = Dormant then View (Item).Remaining = 0)
      and then (if View (Item).Stage = Ready then View (Item).Remaining > 0)
      and then (if View (Item).Stage = Exhausted then View (Item).Remaining = 0)
      and then ((View (Item).Stage = Complete) =
        (View (Item).Empty in Observed_Empty)))
     with Global => null;
   function Can_Start (Item : State) return Boolean is
     (View (Item).Stage = Ready) with Global => null;
   -- Fresh queue-slot construction, never renewal of an existing operation.
   function Create (Queries : Allowance) return State
     with Global => null,
       Post => Valid (Create'Result)
         and then View (Create'Result) = Description'
           (Stage => Ready, Remaining => Queries, Empty => No_Empty_Response);
   -- Called only after lower-layer acceptance, not on an attempted submission.
   procedure Start_Iteration (Item : in out State)
     with Global => null, Always_Terminates,
       Pre => Valid (Item) and then Can_Start (Item),
       Post => Valid (Item) and then View (Item) =
         (View (Item)'Old with delta
           Stage => Waiting, Remaining => View (Item)'Old.Remaining - 1);
   -- Event means the attributable query and its publication have settled.
   procedure Record_Event (Item : in out State)
     with Global => null, Always_Terminates,
       Pre => Valid (Item) and then View (Item).Stage = Waiting,
       Post => Valid (Item) and then View (Item) =
         (View (Item)'Old with delta
           Stage => (if View (Item)'Old.Remaining = 0 then Exhausted else Ready));
   procedure Record_Empty (Item : in out State; Kind : Observed_Empty)
     with Global => null, Always_Terminates,
       Pre => Valid (Item) and then View (Item).Stage = Waiting,
       Post => Valid (Item) and then View (Item) =
         (View (Item)'Old with delta Stage => Complete, Empty => Kind);
   -- Failure retains spent allowance; there is no retry/reset transition.
   procedure Fail (Item : in out State)
     with Global => null, Always_Terminates,
       Pre => Valid (Item) and then View (Item).Stage in Ready | Waiting,
       Post => Valid (Item) and then View (Item) =
         (View (Item)'Old with delta Stage => Failed);
   -- The owner separately validates publication, serving and recovery state.
   -- This can fund future distinct queries only after successful exhaustion.
   procedure Extend (Item : in out State; Queries : Allowance; Accepted : out Boolean)
     with Global => null, Always_Terminates, Pre => Valid (Item),
       Post => Valid (Item)
         and then Accepted = (View (Item)'Old.Stage = Exhausted)
         and then View (Item) =
           (if Accepted then (View (Item)'Old with delta Stage => Ready, Remaining => Queries)
            else View (Item)'Old);
private
   type State is record
      Data : Description;
   end record;
end ACPI_EC_Query_Scans;
