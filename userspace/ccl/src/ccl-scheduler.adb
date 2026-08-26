with Interfaces; use Interfaces;
with CCL.VM; use CCL.VM;

package body CCL.Scheduler with
   SPARK_Mode => On
is
   function Make_Token
     (Item : Isolate_Index;
      Generation : Unsigned_32) return Unsigned_64
   is
     (Shift_Left (Unsigned_64 (Generation), 32) or
      Unsigned_64 (Item + 1));

   procedure Initialize (State : out Scheduler_State) is
   begin
      State := (others => <>);
   end Initialize;

   procedure Start
     (State   : in out Scheduler_State;
      Program : Validated_Program;
      Fuel    : Natural;
      Started : out Boolean;
      Isolate : out Isolate_Index)
   is
   begin
      Started := False;
      Isolate := 0;
      for Item in Isolate_Index loop
         if State.Isolates (Item).Status = Empty or else
           State.Isolates (Item).Status = Finished or else
           State.Isolates (Item).Status = Failed
         then
            if State.Isolates (Item).Generation = Unsigned_32'Last then
               State.Isolates (Item).Generation := 1;
            else
               State.Isolates (Item).Generation :=
                 State.Isolates (Item).Generation + 1;
            end if;
            State.Isolates (Item).Program := Program;
            CCL.VM.Initialize
              (Program, Fuel, State.Isolates (Item).Machine);
            State.Isolates (Item).Token := 0;
            State.Isolates (Item).Status := Runnable;
            Started := True;
            Isolate := Item;
            exit;
         end if;
      end loop;
   end Start;

   procedure Dispatch_One
     (State : in out Scheduler_State;
      Event : out Scheduler_Event)
   is
      Selected : Isolate_Index := State.Next;
      Found    : Boolean := False;
      Outcome  : Execution_Result;
   begin
      Event := (others => <>);
      for Offset in 0 .. MAX_ISOLATES - 1 loop
         Selected := Isolate_Index
           ((Natural (State.Next) + Offset) mod MAX_ISOLATES);
         if State.Isolates (Selected).Status = Runnable then
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         return;
      end if;

      State.Next := Isolate_Index ((Natural (Selected) + 1) mod MAX_ISOLATES);
      Continue_Execution
        (State.Isolates (Selected).Program,
         State.Isolates (Selected).Machine,
         Outcome);

      case Outcome.Status is
         when Waiting_For_Host =>
            State.Isolates (Selected).Token := Make_Token
              (Selected, State.Isolates (Selected).Generation);
            State.Isolates (Selected).Status := Waiting;
            Event :=
              (Kind => Host_Request,
               Isolate => Selected,
               Token => State.Isolates (Selected).Token,
               Import => Outcome.Requested_Import,
               Authority => Outcome.Requested_Authority,
               Binding => Outcome.Requested_Binding,
               Argument => Outcome.Request_Argument,
               others => <>);
         when Completed =>
            State.Isolates (Selected).Status := Finished;
            Event :=
              (Kind => Isolate_Completed,
               Isolate => Selected,
               Has_Value => Outcome.Has_Value,
               Value => Outcome.Result_Value,
               others => <>);
         when others =>
            State.Isolates (Selected).Status := Failed;
            Event :=
              (Kind => Isolate_Failed,
               Isolate => Selected,
               Failure => Outcome.Status,
               others => <>);
      end case;
   end Dispatch_One;

   procedure Complete
     (State    : in out Scheduler_State;
      Token    : Unsigned_64;
      Response : Value;
      Accepted : Boolean;
      Matched  : out Boolean)
   is
   begin
      Matched := False;
      for Item in Isolate_Index loop
         if State.Isolates (Item).Status = Waiting and then
           State.Isolates (Item).Token = Token
         then
            Complete_Host_Call
              (State.Isolates (Item).Program,
               State.Isolates (Item).Machine,
               Response,
               Accepted);
            State.Isolates (Item).Token := 0;
            State.Isolates (Item).Status := Runnable;
            Matched := True;
            exit;
         end if;
      end loop;
   end Complete;

   function Status
     (State : Scheduler_State;
      Item  : Isolate_Index) return Isolate_Status
   is (State.Isolates (Item).Status);
end CCL.Scheduler;
