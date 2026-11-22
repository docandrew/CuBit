-- Metadata only. Actual output leases, grants and storage stay with the caller
-- until Storage_Ready. Evidence must describe these exact resources.
generic
   Maximum_Targets : Positive;
package Compositor_Output_Retirement with SPARK_Mode, Pure is
   subtype Target_Index is Positive range 1 .. Maximum_Targets;
   type Grant_Set is array (Target_Index) of Boolean;
   type Phase is (Idle, Renderer_Pending, Lease_Pending, Grants_Pending,
                  Storage_Ready, Released, Quarantined);
   type Renderer_Result is (Retired, Busy, Uncertain);
   type Grant_Phase is (Absent, Revoke_Required, Confirmation_Pending, Retired);
   type State is private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Status (S : State) return Phase;
   function Lease_Held (S : State) return Boolean;
   -- A renderer becoming quiescent does not retire Display's submitted frame.
   function Can_Release_Lease (S : State; Presentation_Retired : Boolean) return Boolean is
     (Status (S) = Lease_Pending and then Presentation_Retired);
   function Grant_Status (S : State; I : Target_Index) return Grant_Phase;
   function Grants_Held (S : State) return Boolean;
   function Same_Grants (After, Before : State) return Boolean with Ghost;
   function Other_Grants_Unchanged (After, Before : State; Changed : Target_Index)
     return Boolean with Ghost;
   function Start (Leased : Boolean; Grants : Grant_Set) return State
     with Post => Valid (Start'Result) and Status (Start'Result) = Renderer_Pending
       and Lease_Held (Start'Result) = Leased and
       (for all I in Target_Index => Grant_Status (Start'Result, I) =
         (if Grants (I) then Revoke_Required else Absent));
   procedure Observe_Renderer (S : in out State; Result : Renderer_Result)
     with Pre => Valid (S) and Status (S) = Renderer_Pending,
       Post => Valid (S) and Same_Grants (S, S'Old) and
         Lease_Held (S) = Lease_Held (S'Old) and
         Status (S) = (case Result is
           when Busy => Renderer_Pending, when Uncertain => Quarantined,
           when Retired => (if Lease_Held (S) then Lease_Pending
                           elsif Grants_Held (S) then Grants_Pending else Storage_Ready))
         and (if Result = Busy then S = S'Old);
   -- Current Display wire protocol has no explicit Busy release response.
   -- Failed/malformed lease release is uncertainty, not a retryable completion.
   procedure Observe_Lease (S : in out State; Confirmed : Boolean)
     with Pre => Valid (S) and Status (S) = Lease_Pending,
       Post => Valid (S) and Same_Grants (S, S'Old) and
         Lease_Held (S) = not Confirmed and
         Status (S) = (if not Confirmed then Quarantined
                      elsif Grants_Held (S) then Grants_Pending else Storage_Ready);
   procedure Observe_Revoke (S : in out State; I : Target_Index; Accepted : Boolean)
     with Pre => Valid (S) and Status (S) = Grants_Pending and
         Grant_Status (S, I) = Revoke_Required,
       Post => Valid (S) and not Lease_Held (S) and
         Other_Grants_Unchanged (S, S'Old, I) and
         Grant_Status (S, I) = (if Accepted then Confirmation_Pending else Revoke_Required)
         and Status (S) = (if Accepted then Grants_Pending else Quarantined);
   -- False includes still pending or unavailable confirmation. Both retain
   -- the grant/storage; acceptance alone never grants permission to free.
   procedure Observe_Grant (S : in out State; I : Target_Index; Confirmed : Boolean)
     with Pre => Valid (S) and Status (S) = Grants_Pending and
         Grant_Status (S, I) = Confirmation_Pending,
       Post => Valid (S) and not Lease_Held (S) and
         Other_Grants_Unchanged (S, S'Old, I) and
         Grant_Status (S, I) = (if Confirmed then Retired else Confirmation_Pending) and
         Status (S) = (if Grants_Held (S) then Grants_Pending else Storage_Ready) and
         (if not Confirmed then S = S'Old);
   procedure Observe_Storage (S : in out State; Confirmed : Boolean)
     with Pre => Valid (S) and Status (S) = Storage_Ready,
       Post => Valid (S) and not Lease_Held (S) and Same_Grants (S, S'Old) and
         Status (S) = (if Confirmed then Released else Quarantined);
private
   type Grant_States is array (Target_Index) of Grant_Phase;
   type State is record
      Stage : Phase := Idle;
      Lease : Boolean := False;
      Items : Grant_States := (others => Absent);
   end record;
   function Status (S : State) return Phase is (S.Stage);
   function Lease_Held (S : State) return Boolean is (S.Lease);
   function Grant_Status (S : State; I : Target_Index) return Grant_Phase is (S.Items (I));
   function Grants_Held (S : State) return Boolean is
     (for some G of S.Items => G in Revoke_Required | Confirmation_Pending);
   function Same_Grants (After, Before : State) return Boolean is (After.Items = Before.Items);
   function Other_Grants_Unchanged (After, Before : State; Changed : Target_Index)
     return Boolean is
       (for all I in Target_Index => (if I /= Changed then After.Items (I) = Before.Items (I)));
   function Valid (S : State) return Boolean is
     (case S.Stage is
       when Idle | Storage_Ready | Released => not S.Lease and not Grants_Held (S),
       when Renderer_Pending => (for all G of S.Items => G in Absent | Revoke_Required),
       when Lease_Pending => S.Lease and (for all G of S.Items => G in Absent | Revoke_Required),
       when Grants_Pending => not S.Lease and Grants_Held (S),
       when Quarantined => True);
end Compositor_Output_Retirement;
