package body Config_Object_Client.Resources.Runs is
   package R renames CCL.Resources;
   package V renames CCL.VM;
   package N renames V.Native_Objects;
   package D renames Config_Object_Interfaces;
   package W renames Config_Object_Messages;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type R.Outcome;
   use type R.Reference;
   use type V.Execution_Status;
   use type V.Value;
   use type V.Value_Kind;
   use type W.Status;
   use type CCL.Types.Shape;
   use type D.Operation;
   use type Calls.Resume_State;

   function Can_Replace (Host : Runner) return Boolean is
     (Host.Pending = No_IO and State (Host.Object) = Vacant and R.Empty (Host.Owner));
   function Background_Work (Host : Runner) return Boolean is
     (Host.Pending /= No_IO or else (not Host.Running and not Can_Replace (Host)));
   function Waiting_For_IO (Host : Runner) return Boolean is (Host.Pending /= No_IO);
   function Cleanup_Retry_Needed (Host : Runner) return Boolean is
     (Background_Work (Host) and Host.Pending = No_IO and State (Host.Object) /= Quarantined);
   function Snapshot (Host : Runner) return V.Machine_Snapshot is (N.Snapshot (Host.Machine));

   procedure Configure
     (Host : in out Runner; Types : CCL.Types.Registry;
      Kind : CCL.Types.Type_Reference; Contract : CCL.Objects.Binding;
      Reads : Config_Read_Outcomes.Description; Bindings : Binding_Table;
      Endpoint : CuBit.Messages.CapabilitySlot; Name : String;
      Accepted : out Boolean)
   is
      Description : constant CCL.Types.Description := CCL.Types.Describe (Types, Kind);
   begin
      Accepted := False;
      if Host.Loaded or not Can_Replace (Host) or Endpoint = 0 or
        Name'Length not in 1 .. Host.Name'Length or
        Description.Form /= CCL.Types.Resource or Description.Count /= 1
      then return; end if;
      if not CCL.Objects.Matches_Type (Contract, Types, Description.Parts (1).Payload) or else
        not Config_Read_Outcomes.Matches (Reads, Contract)
      then return; end if;
      for Action in D.Operation loop
         if Bindings (Action) = 0 then return; end if;
         for Other in D.Operation loop
            if Action /= Other and Bindings (Action) = Bindings (Other) then return; end if;
         end loop;
      end loop;
      Host.Types := Types; Host.Kind := Kind; Host.Contract := Contract;
      Host.Reads := Reads; Host.Bindings := Bindings; Host.Endpoint := Endpoint;
      Host.Name_Length := Name'Length; Host.Name (1 .. Name'Length) := Name;
      Host.Configured := True; Accepted := True;
   end Configure;

   procedure Load
     (Host : in out Runner; Item : V.Validated_Program; Fuel : Natural;
      Accepted : out Boolean)
   is
      Outcome : R.Outcome;
   begin
      Accepted := False;
      if not Can_Replace (Host) then return; end if;
      if Host.Running then R.Stop (Host.Owner, Host.Session, Outcome); end if;
      R.Start (Host.Owner, Host.Types, Host.Session, Outcome);
      if Outcome /= R.Succeeded then return; end if;
      Host.Program := Item;
      N.Initialize (Host.Program, Fuel, Host.Machine);
      Host.Last := (others => <>);
      Host.Loaded := True; Host.Running := True;
      Host.Revision := 0; Accepted := True;
   end Load;

   function Next_Token (Tokens : in out Number) return Number is
   begin
      if Tokens >= Number'Last - 1 then return 0; end if;
      Tokens := Tokens + 1; return Tokens;
   end Next_Token;

   procedure Retire_Run (Host : in out Runner) is
      Outcome : R.Outcome;
   begin
      if Host.Running then
         R.Stop (Host.Owner, Host.Session, Outcome);
         Retire (Host.Object, Host.Owner);
         Host.Running := False;
      end if;
   end Retire_Run;

   procedure Maintain (Host : in out Runner; Tokens : in out Number) is
      Result : Cleanup_Result;
   begin
      if Host.Pending /= No_IO or State (Host.Object) in Vacant | Quarantined then return; end if;
      -- A completed close can retire its storage during an otherwise live
      -- run. An available collection must remain usable until Stop/termination.
      if Host.Running and State (Host.Object) = Available then return; end if;
      Cleanup (Host.Object, Host.Owner, Next_Token (Tokens), Result);
      if Result = Close_Submitted then Host.Pending := Cleanup_Call; end if;
      -- Uncertain acquisition/close remains quarantined; Can_Replace stays
      -- false. Never reset the registry just to make a subsequent Run succeed.
   end Maintain;

   procedure Observe (Host : in out Runner; Tokens : in out Number) is
   begin
      N.Continue_Execution_For (Host.Program, Host.Machine, 0, Host.Last);
      if Snapshot (Host).Terminal then Retire_Run (Host); end if;
      Maintain (Host, Tokens);
   end Observe;

   procedure Advance
     (Host : in out Runner; Instructions : Natural; Tokens : in out Number;
      Result : out V.Execution_Result)
   is
      Sent : Submission;
      Token : Number;
      Action : D.Operation := D.Open_Collection;
      Found : Boolean := False;
   begin
      if not Host.Loaded then Result := (others => <>); return; end if;
      if not Host.Running or Host.Pending /= No_IO then Result := Host.Last; return; end if;
      N.Continue_Execution_For (Host.Program, Host.Machine, Instructions, Host.Last);
      if Host.Last.Status = V.Waiting_For_Host and Host.Configured then
         for Op in D.Operation loop
            if Host.Last.Requested_Binding = Host.Bindings (Op) then Action := Op; Found := True; end if;
         end loop;
         if Found then
            Sent := Invalid_Request;
            Token := Next_Token (Tokens);
            if Token /= 0 then
               case Action is
                  when D.Open_Collection =>
                     if State (Host.Object) = Vacant and not Host.Last.Request_Owned and
                       Host.Last.Request_Argument = V.Integer_Constant (0)
                     then
                        Create (Host.Object, Host.Owner, Host.Session, Host.Kind, Host.Endpoint,
                          Host.Name (1 .. Host.Name_Length), Host.Contract, W.Read_Write, 0, Token, Sent);
                        if Sent = Submitted then Host.Pending := Opening; end if;
                     end if;
                  when D.Read_Value | D.Write_Value =>
                     Calls.Submit (Host.Call, Host.Object, Host.Owner,
                       (if Action = D.Read_Value then Calls.Read_Value else Calls.Write_Value),
                       Host.Bindings (Action), Host.Program, Host.Machine, Host.Reads, Host.Revision, Token, Sent);
                     if Sent = Submitted then Host.Pending := Data_Call; end if;
                  when D.Close_Collection =>
                     if Host.Last.Request_Owned and then Host.Last.Request_Argument.Kind = V.Resource_Value and then
                       Host.Last.Request_Argument.Resource = Reference_Of (Host.Object, Host.Owner)
                     then
                        Close (Host.Object, Host.Owner, Host.Last.Request_Argument.Resource, Token, Sent);
                        if Sent = Submitted then
                           N.Acknowledge_Host_Submission (Host.Program, Host.Machine, True);
                           Host.Pending := Closing_Call;
                        end if;
                     end if;
               end case;
            end if;
            if Sent /= Submitted then
               if Host.Last.Request_Owned then
                  N.Acknowledge_Host_Submission (Host.Program, Host.Machine, False);
               end if;
               N.Complete_Scalar (Host.Program, Host.Machine, V.Integer_Constant (0), False);
               Observe (Host, Tokens);
            end if;
         end if;
      end if;
      if Snapshot (Host).Terminal then Retire_Run (Host); end if;
      Maintain (Host, Tokens);
      Result := Host.Last;
   end Advance;

   procedure Complete
     (Host : in out Runner; Receipt : CuBit.Messages.CompletionEntry;
      Tokens : in out Number; Accepted : out Boolean)
   is
      Done : Completion_Result;
      Answer : Response;
      Taken, Good : Boolean;
      Resumed : Calls.Resume_State;
   begin
      Accepted := False;
      if Host.Pending = No_IO then return; end if;
      Complete (Host.Object, Host.Owner, Receipt, Done);
      if Done /= Completed then return; end if;
      Accepted := True;
      if not Host.Running or Host.Pending = Cleanup_Call then
         if Host.Pending = Data_Call then
            Calls.Drain (Host.Call, Host.Object, Host.Owner, Answer, Taken);
         else Take_Result (Host.Object, Host.Owner, Answer, Taken); end if;
         if not Taken then return; end if;
      elsif Host.Pending = Data_Call then
         -- The client has already validated the receipt against its retained
         -- operation. Track the last observed revision; conflicts are returned
         -- to the script, not transparently retried with a newer revision.
         if Host.Object.Backend.Output.Valid and then
           Host.Object.Backend.Output.Code in W.Success | W.Stale | W.Missing
         then Host.Revision := Host.Object.Backend.Output.Revision; end if;
         Calls.Resume (Host.Call, Host.Object, Host.Owner, Host.Program, Host.Machine, Resumed);
         if Resumed /= Calls.Resumed then
            Stop (Host, Tokens);
            Calls.Drain (Host.Call, Host.Object, Host.Owner, Answer, Taken);
            if not Taken then return; end if;
         end if;
      else
         Take_Result (Host.Object, Host.Owner, Answer, Taken);
         if not Taken then return; end if;
         Good := Answer.Valid and Answer.Code = W.Success;
         if Host.Pending = Opening and Good then
            Host.Revision := 0;
            N.Complete_Resource (Host.Program, Host.Machine, Host.Owner,
              Reference_Of (Host.Object, Host.Owner), Good);
         else
            N.Complete_Scalar (Host.Program, Host.Machine, V.Integer_Constant (0), Good);
         end if;
         if not Good then N.Complete_Scalar (Host.Program, Host.Machine, V.Integer_Constant (0), False); end if;
      end if;
      Host.Pending := No_IO;
      if Host.Running then Observe (Host, Tokens); else Maintain (Host, Tokens); end if;
   end Complete;

   procedure Stop (Host : in out Runner; Tokens : in out Number) is
      Position : V.Machine_Snapshot;
   begin
      if not Host.Loaded then return; end if;
      N.Stop (Host.Machine);
      Position := Snapshot (Host);
      Host.Last := (Status => Position.Status, Fuel_Remaining => Position.Fuel_Remaining,
        Steps => Position.Steps, others => <>);
      Retire_Run (Host);
      Maintain (Host, Tokens);
   end Stop;

   procedure Complete_Scalar
     (Host : in out Runner; Value : V.Value; Accepted : Boolean) is
   begin
      if Host.Loaded and Host.Running and Host.Pending = No_IO then
         N.Complete_Scalar (Host.Program, Host.Machine, Value, Accepted);
      end if;
   end Complete_Scalar;

   procedure Inspect (Host : Runner; Result : out V.Inspection_Snapshot) is
   begin
      Result := (others => <>);
      if Host.Loaded then N.Inspect (Host.Program, Host.Machine, Result); end if;
   end Inspect;
end Config_Object_Client.Resources.Runs;
