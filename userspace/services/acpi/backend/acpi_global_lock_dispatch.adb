pragma Ada_2022;
package body ACPI_Global_Lock_Dispatch with SPARK_Mode is
   package P renames ACPI_Global_Lock_Protocol;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, P.Operation, Requests.Reply_Kind;
   function Outcome_For (Item : Owner.Outcome) return P.Lock_Outcome is
     (case Item is
        when Owner.Configured => P.Configured,
        when Owner.Invalid_Configuration => P.Invalid_Configuration,
        when Owner.Already_Configured => P.Already_Configured,
        when Owner.Acquired => P.Acquired,
        when Owner.Acquired_After_Deadline => P.Acquired_After_Deadline,
        when Owner.Acquired_Clock_Failed => P.Acquired_Clock_Failed,
        when Owner.Released => P.Released,
        when Owner.Waiting_For_Firmware => P.Waiting_For_Firmware,
        when Owner.Contended => P.Contended,
        when Owner.Expired => P.Expired,
        when Owner.Clock_Failed => P.Clock_Failed,
        when Owner.Clock_Reversed => P.Clock_Reversed,
        when Owner.Invalid_Deadline => P.Invalid_Deadline,
        when Owner.Unauthorized => P.Unauthorized,
        when Owner.Not_Ready => P.Not_Ready,
        when Owner.Already_Held => P.Already_Held,
        when Owner.Stale_Lease => P.Stale_Lease,
        when Owner.Lease_Exhausted => P.Lease_Exhausted,
        when Owner.Read_Unavailable => P.Read_Unavailable,
        when Owner.Atomic_Refused => P.Atomic_Refused,
        when Owner.Atomic_Indeterminate => P.Atomic_Indeterminate,
        when Owner.Signal_Deferred => P.Signal_Deferred,
        when Owner.Signal_Indeterminate => P.Signal_Indeterminate,
        when Owner.Reentered => P.Reentered);
   function Kind_For (Item : Requests.Reply_Kind) return P.Reply_Kind is
     (case Item is
        when Requests.Ready => P.Ready,
        when Requests.Recorded => P.Recorded,
        when Requests.Cached => P.Cached,
        when Requests.Checked => P.Checked,
        when Requests.No_Result => P.No_Result,
        when Requests.Denied => P.Denied,
        when Requests.Busy => P.Busy,
        when Requests.Stale_Ticket => P.Stale_Ticket,
        when Requests.Conflict => P.Conflict,
        when Requests.Invalid_Request => P.Invalid_Request,
        when Requests.Unavailable => P.Unavailable,
        when Requests.Exhausted => P.Exhausted,
        when Requests.Reentered => P.Reentered);
   procedure Handle
     (State : in out Requests.Context; Scope : Scope_Tag; Caller : Principal;
      Request : P.Packet; Response : out P.Packet; Handled : out Boolean) is
      Input : constant P.Request := P.Decode_Request (Request);
      Kind : P.Operation;
      Output : P.Reply := (Valid => True, Kind => P.Malformed, others => <>);
      Result : Requests.Reply;
      Status : Requests.Reply_Kind;
      Token : Requests.Ticket;
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      Response := (others => <>); Handled := P.Owned_Label (Request.Label);
      if not Handled then return; end if;
      Kind := P.Operation'Val (Request.Label - P.First_Operation);
      if not Input.Valid then Response := P.Encode_Reply (Kind, Output); return; end if;
      if Kind = P.Prepare then
         if Interfaces.Unsigned_64 (Input.Budget) > Interfaces.Unsigned_64 (Owner.Attempt_Budget'Last) then
            Output.Kind := P.Invalid_Request;
         else
            Clock (Now, Available);
            Requests.Prepare (State, Scope, Caller, Input.Nonce, Now, Available,
              Input.Deadline, Owner.Attempt_Budget (Input.Budget), Token, Status);
            Output.Kind := Kind_For (Status);
            if Status = Requests.Ready then Output.Token := (Token.Domain, Token.Sequence); end if;
         end if;
      else
         Token := (Input.Token.Domain, Input.Token.Sequence);
         case Kind is
            when P.Prepare => null;
            when P.Acquire => Requests.Acquire (State, Scope, Caller, Token, Result);
            when P.Check_Access => Requests.Check_Access (State, Scope, Caller, Token, Result);
            when P.Release => Requests.Release (State, Scope, Caller, Token, Result);
            when P.Query_Acquisition => Requests.Query (State, Scope, Caller, Token, Requests.Acquisition, Result);
            when P.Query_Release => Requests.Query (State, Scope, Caller, Token, Requests.Release_Request, Result);
            when P.Query_Termination => Requests.Query (State, Scope, Caller, Token, Requests.Termination, Result);
         end case;
         Output.Kind := Kind_For (Result.Kind); Output.Outcome := Outcome_For (Result.Outcome);
      end if;
      Response := P.Encode_Reply (Kind, Output);
   end Handle;
end ACPI_Global_Lock_Dispatch;
