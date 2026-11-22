pragma Ada_2022;
with ACPI_Global_Lock_Policy;
package body ACPI_Global_Lock_Owner with SPARK_Mode is
   use type Interfaces.Unsigned_32;
   package P renames ACPI_Global_Lock_Policy;
   function Observe (State : Context) return Statistics is
     ((State.Stage, State.Issued, State.Working));
   function Matches (State : Context; Caller : Principal; Token : Lease) return Boolean is
     (Caller /= No_Principal and then Caller = State.Authorized
      and then Token /= No_Lease and then Token = State.Current);
   function Holds (State : Context; Caller : Principal; Token : Lease) return Boolean is
     (State.Stage = Held and then not State.Working and then Matches (State, Caller, Token));
   procedure Configure
     (State : in out Context; Authorized : Principal; Domain : Counter; Result : out Outcome) is
   begin
      if State.Working then Result := Reentered;
      elsif State.Stage /= Fresh then Result := Already_Configured;
      elsif Authorized = No_Principal or else Domain = No_Domain
        or else Maximum_Leases = 0
        or else Exchanged = Not_Exchanged or else Exchanged = Not_Attempted
        or else Exchanged = Atomic_Unknown or else Not_Exchanged = Not_Attempted
        or else Not_Exchanged = Atomic_Unknown or else Not_Attempted = Atomic_Unknown
        or else Signal_Complete = Signal_Not_Sent or else Signal_Complete = Signal_Unknown
        or else Signal_Not_Sent = Signal_Unknown
      then Result := Invalid_Configuration;
      else
         State.Authorized := Authorized; State.Domain := Domain;
         State.Stage := Ready; Result := Configured;
      end if;
   end Configure;
   procedure Check_Time
     (State : in out Context; Deadline : Counter; Result : out Outcome) is
      Now : Counter;
      Available : Boolean;
   begin
      if State.Clock_Broken then Result := Clock_Reversed; return; end if;
      Read_Clock (Now, Available);
      if not Available then Result := Clock_Failed; return; end if;
      if State.Seen_Clock and then Now < State.Last_Clock then
         State.Clock_Broken := True; Result := Clock_Reversed; return;
      end if;
      State.Seen_Clock := True; State.Last_Clock := Now;
      Result := (if Now >= Deadline then Expired else Configured);
   end Check_Time;
   procedure Check_Access
     (State : in out Context; Caller : Principal; Token : Lease; Result : out Outcome) is
   begin
      if State.Working then Result := Reentered;
      elsif Caller = No_Principal or else Caller /= State.Authorized then Result := Unauthorized;
      elsif not Matches (State, Caller, Token) then Result := Stale_Lease;
      elsif State.Stage /= Held or else not State.Access_Admitted then Result := Not_Ready;
      else
         State.Working := True; Check_Time (State, State.Deadline, Result); State.Working := False;
      end if;
   end Check_Access;
   procedure Acquire
     (State : in out Context; Caller : Principal; Deadline : Counter;
      Budget : Attempt_Budget; Token : out Lease; Result : out Outcome)
   is
      procedure Run is
         Word, Seen : Interfaces.Unsigned_32;
         Available : Boolean;
         Atomic : Atomic_Outcome;
         Timing : Outcome;
      begin
         Check_Time (State, Deadline, Result);
         if Result /= Configured then return; end if;
         Read_Word (Word, Available);
         if not Available then Result := Read_Unavailable; return; end if;
         -- Reserve one nonwrapping sequence for this whole bounded attempt.
         State.Issued := State.Issued + 1;
         State.Current := (State.Domain, State.Issued);
         State.Deadline := Deadline; State.Access_Admitted := False;
         for Attempt in 1 .. Budget loop
            Check_Time (State, Deadline, Result);
            if Result /= Configured then State.Current := No_Lease; return; end if;
            declare Proposal : constant P.Acquire_Proposal := P.Propose_Acquire (Word); begin
               Compare_Exchange (Word, Proposal.Replacement, Seen, Atomic);
               if Atomic = Atomic_Unknown
                 or else (Atomic = Exchanged and then Seen /= Word)
                 or else (Atomic = Not_Exchanged and then Seen = Word)
                 or else (Atomic /= Exchanged and Atomic /= Not_Exchanged and Atomic /= Not_Attempted)
               then
                  State.Stage := Acquire_Uncertain; Result := Atomic_Indeterminate; return;
               elsif Atomic = Not_Attempted then
                  State.Current := No_Lease; Result := Atomic_Refused; return;
               elsif Atomic = Exchanged then
                  if not Proposal.Would_Acquire then
                     State.Current := No_Lease; Result := Waiting_For_Firmware; return;
                  end if;
                  State.Stage := Held; Token := State.Current;
                  Check_Time (State, Deadline, Timing);
                  State.Access_Admitted := Timing = Configured;
                  Result := (case Timing is
                    when Configured => Acquired,
                    when Expired => Acquired_After_Deadline,
                    when others => Acquired_Clock_Failed);
                  return;
               else Word := Seen;
               end if;
            end;
         end loop;
         State.Current := No_Lease; Result := Contended;
      end Run;
   begin
      Token := No_Lease;
      if State.Working then Result := Reentered;
      elsif Caller = No_Principal or else Caller /= State.Authorized then Result := Unauthorized;
      elsif State.Stage = Held then Result := Already_Held;
      elsif State.Stage /= Ready then Result := Not_Ready;
      elsif Deadline = No_Finite_Deadline then Result := Invalid_Deadline;
      elsif State.Issued >= Maximum_Leases then Result := Lease_Exhausted;
      else State.Working := True; Run; State.Working := False;
      end if;
   end Acquire;
   procedure Release
     (State : in out Context; Caller : Principal; Token : Lease;
      Budget : Attempt_Budget; Result : out Outcome)
   is
      procedure Finish_Signal is
         Signaling : Signal_Outcome;
      begin
         Signal_Firmware (Signaling);
         if Signaling = Signal_Complete then
            State.Stage := Ready; State.Current := No_Lease; Result := Released;
         elsif Signaling = Signal_Not_Sent then
            State.Stage := Signal_Pending; Result := Signal_Deferred;
         else State.Stage := Signal_Uncertain; Result := Signal_Indeterminate;
         end if;
      end Finish_Signal;
      procedure Run is
         Word, Seen : Interfaces.Unsigned_32;
         Available : Boolean;
         Atomic : Atomic_Outcome;
      begin
         if State.Stage = Signal_Pending then Finish_Signal; return; end if;
         Read_Word (Word, Available);
         if not Available then Result := Read_Unavailable; return; end if;
         for Attempt in 1 .. Budget loop
            if (Word and P.Owned_Bit) = 0 then
               State.Stage := Release_Uncertain; Result := Atomic_Indeterminate; return;
            end if;
            declare Proposal : constant P.Release_Proposal := P.Propose_Release (Word); begin
               Compare_Exchange (Word, Proposal.Replacement, Seen, Atomic);
               if Atomic = Atomic_Unknown
                 or else (Atomic = Exchanged and then Seen /= Word)
                 or else (Atomic = Not_Exchanged and then Seen = Word)
                 or else (Atomic /= Exchanged and Atomic /= Not_Exchanged and Atomic /= Not_Attempted)
               then
                  State.Stage := Release_Uncertain; Result := Atomic_Indeterminate; return;
               elsif Atomic = Not_Attempted then Result := Atomic_Refused; return;
               elsif Atomic = Exchanged then
                  if Proposal.Signal_Required then
                     State.Stage := Signal_Pending; Finish_Signal;
                  else
                     State.Stage := Ready; State.Current := No_Lease; Result := Released;
                  end if;
                  return;
               else Word := Seen;
               end if;
            end;
         end loop;
         Result := Contended;
      end Run;
   begin
      if State.Working then Result := Reentered;
      elsif Caller = No_Principal or else Caller /= State.Authorized then Result := Unauthorized;
      elsif not Matches (State, Caller, Token) then Result := Stale_Lease;
      elsif State.Stage not in Held | Signal_Pending then Result := Not_Ready;
      else State.Working := True; Run; State.Working := False;
      end if;
   end Release;
end ACPI_Global_Lock_Owner;
