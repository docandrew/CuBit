with CCL.Ownership;

package CCL.Imports with
   SPARK_Mode => On
is
   type Transfer_Mode is
     (Copy_Argument, Move_Argument, Borrowed_RO_Argument,
      Borrowed_RW_Argument);

   type Cancellation_Mode is
     (Not_Cancellable, Best_Effort_Cancellation,
      Guaranteed_Cancellation_Request);

   type Completion_Outcome is
     (Import_Succeeded, Import_Failed, Import_Cancelled);

   type Import_Phase is
     (Import_Idle, Import_Offered, Import_Accepted,
      Cancellation_Requested, Import_Completed);

   type Import_Error is
     (Import_Valid, Invalid_Import_Phase, Cancellation_Not_Supported,
      Missing_Cancellation_Verb, Import_Ownership_Failure);

   type Lifecycle is private;

   function Phase (Item : Lifecycle) return Import_Phase;

   procedure Initialize (Item : out Lifecycle) with
     Post => Phase (Item) = Import_Idle;

   procedure Offer
     (Item          : in out Lifecycle;
      Local         : CCL.Ownership.Binding_Id;
      Mode          : Transfer_Mode;
      Cancellation  : Cancellation_Mode;
      Success_Verb  : CCL.Ownership.Disposition_Id;
      Failure_Verb  : CCL.Ownership.Disposition_Id;
      Cancel_Verb   : CCL.Ownership.Disposition_Id;
      Error         : out Import_Error) with
     Post =>
       (if Error = Import_Valid then Phase (Item) = Import_Offered
        else Phase (Item) = Phase (Item'Old));

   --  Local queue rejection returns to Idle without changing ownership.
   procedure Reject_Submission
     (Item : in out Lifecycle; Error : out Import_Error) with
     Post =>
       (if Error = Import_Valid then Phase (Item) = Import_Idle
        else Phase (Item) = Phase (Item'Old));

   --  Acceptance is the ownership boundary.  Borrows become active and a
   --  moved value becomes unavailable until a terminal completion.
   procedure Accept_Submission
     (Item  : in out Lifecycle;
      Env   : in out CCL.Ownership.Environment;
      Types : CCL.Ownership.Type_Table;
      Error : out Import_Error) with
     Post =>
       (if Error = Import_Valid then Phase (Item) = Import_Accepted
        else Phase (Item) = Phase (Item'Old));

   procedure Request_Cancellation
     (Item : in out Lifecycle; Error : out Import_Error);

   procedure Complete
     (Item    : in out Lifecycle;
      Env     : in out CCL.Ownership.Environment;
      Types   : CCL.Ownership.Type_Table;
      Outcome : Completion_Outcome;
      Error   : out Import_Error) with
     Post =>
       (if Error = Import_Valid then Phase (Item) = Import_Completed
        else Phase (Item) = Phase (Item'Old));

private
   type Lifecycle is record
      Current       : Import_Phase := Import_Idle;
      Local         : CCL.Ownership.Binding_Id := 0;
      Mode          : Transfer_Mode := Copy_Argument;
      Cancellation  : Cancellation_Mode := Not_Cancellable;
      Success_Verb  : CCL.Ownership.Disposition_Id := 0;
      Failure_Verb  : CCL.Ownership.Disposition_Id := 0;
      Cancel_Verb   : CCL.Ownership.Disposition_Id := 0;
      Before_Accept : CCL.Ownership.Environment;
   end record;

   function Phase (Item : Lifecycle) return Import_Phase is (Item.Current);
end CCL.Imports;
