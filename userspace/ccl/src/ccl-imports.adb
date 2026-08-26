with Interfaces;

package body CCL.Imports with
   SPARK_Mode => On
is
   use type CCL.Ownership.Ownership_Error;
   use type Interfaces.Unsigned_8;

   procedure Initialize (Item : out Lifecycle) is
   begin
      Item := (others => <>);
      CCL.Ownership.Initialize (Item.Before_Accept);
   end Initialize;

   procedure Offer
     (Item          : in out Lifecycle;
      Local         : CCL.Ownership.Binding_Id;
      Mode          : Transfer_Mode;
      Cancellation  : Cancellation_Mode;
      Success_Verb  : CCL.Ownership.Disposition_Id;
      Failure_Verb  : CCL.Ownership.Disposition_Id;
      Cancel_Verb   : CCL.Ownership.Disposition_Id;
      Error         : out Import_Error)
   is
   begin
      if Item.Current /= Import_Idle then
         Error := Invalid_Import_Phase;
      elsif Cancellation /= Not_Cancellable and then Cancel_Verb = 0 then
         Error := Missing_Cancellation_Verb;
      else
         Item.Current := Import_Offered;
         Item.Local := Local;
         Item.Mode := Mode;
         Item.Cancellation := Cancellation;
         Item.Success_Verb := Success_Verb;
         Item.Failure_Verb := Failure_Verb;
         Item.Cancel_Verb := Cancel_Verb;
         Error := Import_Valid;
      end if;
   end Offer;

   procedure Reject_Submission
     (Item : in out Lifecycle; Error : out Import_Error) is
   begin
      if Item.Current /= Import_Offered then
         Error := Invalid_Import_Phase;
      else
         Item.Current := Import_Idle;
         Error := Import_Valid;
      end if;
   end Reject_Submission;

   procedure Accept_Submission
     (Item  : in out Lifecycle;
      Env   : in out CCL.Ownership.Environment;
      Types : CCL.Ownership.Type_Table;
      Error : out Import_Error)
   is
      Own_Error : CCL.Ownership.Ownership_Error;
      Trial     : CCL.Ownership.Environment;
   begin
      if Item.Current /= Import_Offered then
         Error := Invalid_Import_Phase;
         return;
      end if;
      Item.Before_Accept := Env;
      case Item.Mode is
         when Copy_Argument =>
            CCL.Ownership.Copy_Value
              (Env, Types, Item.Local, Own_Error);
         when Move_Argument =>
            Trial := Env;
            CCL.Ownership.Move_Value (Trial, Item.Local, Own_Error);
            if Own_Error = CCL.Ownership.Ownership_Valid then
               Env := Trial;
            end if;
         when Borrowed_RO_Argument =>
            CCL.Ownership.Borrow_RO (Env, Item.Local, Own_Error);
         when Borrowed_RW_Argument =>
            CCL.Ownership.Borrow_RW (Env, Item.Local, Own_Error);
      end case;
      if Own_Error /= CCL.Ownership.Ownership_Valid then
         Error := Import_Ownership_Failure;
      else
         Item.Current := Import_Accepted;
         Error := Import_Valid;
      end if;
   end Accept_Submission;

   procedure Request_Cancellation
     (Item : in out Lifecycle; Error : out Import_Error) is
   begin
      if Item.Current /= Import_Accepted then
         Error := Invalid_Import_Phase;
      elsif Item.Cancellation = Not_Cancellable then
         Error := Cancellation_Not_Supported;
      else
         Item.Current := Cancellation_Requested;
         Error := Import_Valid;
      end if;
   end Request_Cancellation;

   procedure Complete
     (Item    : in out Lifecycle;
      Env     : in out CCL.Ownership.Environment;
      Types   : CCL.Ownership.Type_Table;
      Outcome : Completion_Outcome;
      Error   : out Import_Error)
   is
      Own_Error : CCL.Ownership.Ownership_Error :=
        CCL.Ownership.Ownership_Valid;
      Verb : CCL.Ownership.Disposition_Id;
   begin
      if Item.Current not in Import_Accepted | Cancellation_Requested then
         Error := Invalid_Import_Phase;
         return;
      elsif Outcome = Import_Cancelled and then
        Item.Current /= Cancellation_Requested
      then
         Error := Invalid_Import_Phase;
         return;
      elsif Item.Current = Cancellation_Requested and then
        Item.Cancellation = Guaranteed_Cancellation_Request and then
        Outcome /= Import_Cancelled
      then
         Error := Invalid_Import_Phase;
         return;
      end if;

      case Item.Mode is
         when Copy_Argument =>
            null;
         when Move_Argument =>
            Env := Item.Before_Accept;
            Verb :=
              (case Outcome is
                  when Import_Succeeded => Item.Success_Verb,
                  when Import_Failed => Item.Failure_Verb,
                  when Import_Cancelled => Item.Cancel_Verb);
            CCL.Ownership.Apply_Disposition
              (Env, Types, Item.Local, Verb, Own_Error);
         when Borrowed_RO_Argument =>
            CCL.Ownership.Return_RO (Env, Item.Local, Own_Error);
         when Borrowed_RW_Argument =>
            CCL.Ownership.Return_RW (Env, Item.Local, Own_Error);
      end case;

      if Own_Error /= CCL.Ownership.Ownership_Valid then
         Error := Import_Ownership_Failure;
      else
         Item.Current := Import_Completed;
         Error := Import_Valid;
      end if;
   end Complete;
end CCL.Imports;
