package body Compositor_Output_Retirement with SPARK_Mode is
   function Start (Leased : Boolean; Grants : Grant_Set) return State is
      S : State;
   begin
      S.Stage := Renderer_Pending;
      S.Lease := Leased;
      for I in Target_Index loop
         S.Items (I) := (if Grants (I) then Revoke_Required else Absent);
         pragma Loop_Invariant
           (for all J in Target_Index => (if J <= I then
              S.Items (J) = (if Grants (J) then Revoke_Required else Absent)));
         pragma Loop_Invariant (Valid (S));
      end loop;
      return S;
   end Start;
   procedure Observe_Renderer (S : in out State; Result : Renderer_Result) is
   begin
      case Result is
         when Busy => null;
         when Uncertain => S.Stage := Quarantined;
         when Retired =>
            S.Stage := (if S.Lease then Lease_Pending
                        elsif Grants_Held (S) then Grants_Pending else Storage_Ready);
      end case;
   end Observe_Renderer;
   procedure Observe_Lease (S : in out State; Confirmed : Boolean) is
   begin
      if Confirmed then
         S.Lease := False;
         S.Stage := (if Grants_Held (S) then Grants_Pending else Storage_Ready);
      else
         S.Stage := Quarantined;
      end if;
   end Observe_Lease;
   procedure Observe_Revoke (S : in out State; I : Target_Index; Accepted : Boolean) is
   begin
      if Accepted then S.Items (I) := Confirmation_Pending;
      else S.Stage := Quarantined; end if;
   end Observe_Revoke;
   procedure Observe_Grant (S : in out State; I : Target_Index; Confirmed : Boolean) is
   begin
      if Confirmed then
         S.Items (I) := Retired;
         if not Grants_Held (S) then S.Stage := Storage_Ready; end if;
      end if;
   end Observe_Grant;
   procedure Observe_Storage (S : in out State; Confirmed : Boolean) is
   begin
      S.Stage := (if Confirmed then Released else Quarantined);
   end Observe_Storage;
end Compositor_Output_Retirement;
