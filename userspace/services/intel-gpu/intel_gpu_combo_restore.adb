package body Intel_GPU_Combo_Restore is
   use Intel_GPU_Combo_PHY;
   Attempted : Boolean := False;
   function Diagnostic (Value : Report) return String is
     ("PHY=" & (if Value.Port = A then "A" else "B") & " result=" &
      (case Value.Status is
         when Rejected => "rejected", when Read_Failed => "read-failed",
         when Invalid_State => "invalid-state", when Changed => "changed",
         when Power_Lost => "power-lost", when Write_Failed => "write-failed",
         when Visibility_Failed => "visibility-failed",
         when Verification_Failed => "verification-failed", when Ready => "ready") &
      " writes=" & Natural'Image (Value.Writes_Attempted));
   procedure Execute (Authorized : Boolean; Result : out Report) is
      Baseline : array (PHY) of Snapshot;
      Plans : array (PHY) of Plan;
      Current : Snapshot;
      OK : Boolean;

      procedure Run is
      begin
         -- Preflight BOTH PHYs before changing A. An invalid B must not cause
         -- an avoidable partial restore of the master PHY.
         for Port in PHY loop
            Result.Port := Port;
            if not Held then Result.Status := Power_Lost; return; end if;
            Read_State (Port, Baseline (Port), OK);
            if not OK then Result.Status := Read_Failed; return; end if;
            Plans (Port) := Prepare (Port, Baseline (Port));
            if Plans (Port).Status in Invalid_Read | Unknown_Process then
               Result.Status := Invalid_State; return;
            end if;
         end loop;
         for Port in PHY loop
            Result.Port := Port;
            if not Held then Result.Status := Power_Lost; return; end if;
            Read_State (Port, Current, OK);
            if not OK then Result.Status := Read_Failed; return; end if;
            if not Same_Configuration (Current, Baseline (Port)) then
               Result.Status := Changed; return;
            end if;
            -- Replan from the latest copied words, preserving unowned fields
            -- rather than replaying their older preflight observations.
            Plans (Port) := Prepare (Port, Current);
            for I in 1 .. Plans (Port).Count loop
               if not Held then Result.Status := Power_Lost; return; end if;
               Result.Writes_Attempted := Result.Writes_Attempted + 1;
               Write_Register
                 (Write_Offset (Port, Plans (Port).Writes (I).Register),
                  Plans (Port).Writes (I).Value, OK);
               if not OK then Result.Status := Write_Failed; return; end if;
            end loop;
            if Plans (Port).Count > 0 then
               if not Held then Result.Status := Power_Lost; return; end if;
               Finish_Writes (OK);
               if not OK then Result.Status := Visibility_Failed; return; end if;
            end if;
            if not Held then Result.Status := Power_Lost; return; end if;
            Read_State (Port, Current, OK);
            if not OK then Result.Status := Read_Failed; return; end if;
            if Prepare (Port, Current).Status /= Already_Ready then
               Result.Status := Verification_Failed; return;
            end if;
         end loop;
         if not Held then Result.Status := Power_Lost; return; end if;
         Result.Status := Ready;
      end Run;
   begin
      Result := (others => <>);
      if not Authorized or Attempted then return; end if;
      Attempted := True;
      if not Begin_Scope then return; end if;
      Run;
      End_Scope;
   end Execute;
end Intel_GPU_Combo_Restore;
