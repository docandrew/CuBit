pragma Ada_2022;
package body ACPI_GPE_Lifecycle with SPARK_Mode is
   package body Driver with SPARK_Mode is
      function Observe (Run : Session) return Report is (Run.Info);
      function Valid (Run : Session) return Boolean is
        (case Run.Info.Stage is
           when Idle => Run.Info.Source = Unknown_State
              and then Run.Info.Charged = 0
              and then Run.Info.Reason = No_Failure and then not Run.Info.Deferred
              and then Run.Info.Handler_Budget = 0,
           when Clearing_Edge | Ready =>
              Run.Info.Source = Known_Masked and then Run.Info.Charged = 0
              and then Run.Info.Reason = No_Failure and then not Run.Info.Deferred,
           when Executing => Run.Info.Source = Known_Masked
              and then Run.Info.Charged = 0 and then Run.Info.Reason = No_Failure
              and then Run.Info.Handler_Budget > 0,
           when Handler_Done | Clearing_Level =>
              Run.Info.Source = Known_Masked
              and then Run.Info.Handler_Status = Handler_Succeeded
              and then Run.Info.Charged = Run.Info.Reported_Charge
              and then Run.Info.Charged <= Run.Info.Handler_Budget and then not Run.Info.Deferred
              and then Run.Info.Reason = No_Failure,
           when Finished => Run.Info.Source = Known_Enabled
              and then Run.Info.Handler_Status = Handler_Succeeded
              and then Run.Info.Charged = Run.Info.Reported_Charge
              and then Run.Info.Charged <= Run.Info.Handler_Budget and then not Run.Info.Deferred
              and then Run.Info.Reason = No_Failure,
           when others => True);
      -- The callback may report any outcome. Its physical effects and
      -- termination remain backend obligations; this contract tracks only
      -- the controller's report and preserves the handler accounting.
      procedure Perform (Run : in out Session; Kind : Operation; Success : out Boolean)
        with Always_Terminates, Pre =>
          (case Kind is
             when Mask_Source => Run.Info.Stage = Masking
               and then Run.Info.Source = Unknown_State,
             when Clear_Status => Run.Info.Stage in Clearing_Edge | Clearing_Level
               and then Run.Info.Source = Known_Masked,
             when Enable_Source => Run.Info.Stage = Enabling
               and then Run.Info.Source = Known_Masked,
             when others => False),
        Post => Run.ID = Run.ID'Old
          and then Run.Info.Trigger = Run.Info.Trigger'Old
          and then Run.Info.Handler_Status = Run.Info.Handler_Status'Old
          and then Run.Info.Charged = Run.Info.Charged'Old
          and then Run.Info.Reported_Charge = Run.Info.Reported_Charge'Old
          and then Run.Info.Handler_Budget = Run.Info.Handler_Budget'Old
          and then Run.Info.Deferred = Run.Info.Deferred'Old
          and then Run.Info.Last_Operation = Kind
          and then
            (if Success then Run.Info.Stage = Run.Info.Stage'Old
               and then Run.Info.Reason = Run.Info.Reason'Old
               and then Run.Info.IO_Status = Completed
               and then Run.Info.Source =
                 (case Kind is
                    when Mask_Source => Known_Masked,
                    when Enable_Source => Known_Enabled,
                    when others => Run.Info.Source'Old)
             else Run.Info.Stage = Failed
               and then Run.Info.Reason /= No_Failure
               and then Run.Info.Source /= Known_Enabled)
      is
         Outcome : IO_Outcome;
      begin
         Success := False; Run.Info.Last_Operation := Kind;
         if Kind = Enable_Source then Run.Info.Source := Unknown_State; end if;
         case Kind is
            when Mask_Source => Mask (Run.ID, Outcome);
            when Clear_Status => Clear (Run.ID, Outcome);
            when Enable_Source => Enable (Run.ID, Outcome);
            when others => Run.Info.Stage := Failed; Run.Info.Reason := Provider_Contract_Error; return;
         end case;
         Run.Info.IO_Status := Outcome;
         case Outcome is
            when Completed =>
               if Kind = Mask_Source then Run.Info.Source := Known_Masked;
               elsif Kind = Enable_Source then Run.Info.Source := Known_Enabled; end if;
               Success := True; return;
            when Refused =>
               if Kind = Enable_Source then Run.Info.Source := Known_Masked; end if;
               Run.Info.Reason := Operation_Refused;
            when Completion_Unknown => Run.Info.Reason := Uncertain_Completion;
         end case;
         Run.Info.Stage := Failed;
      exception
         when others =>
            Run.Info.Stage := Failed; Run.Info.Reason := Callback_Exception;
            Run.Info.IO_Status := Completion_Unknown;
      end Perform;
      procedure Start
        (Run : in out Session; Source : Source_ID; Trigger : Trigger_Kind;
         Result : out Action_Result) is
         Success : Boolean;
      begin
         Result := Wrong_Phase; if Run.Info.Stage /= Idle then return; end if;
         Result := Invalid_Source; if Source = No_Source then return; end if;
         Result := Accepted; Run.ID := Source; Run.Info.Trigger := Trigger;
         Run.Info.Stage := Masking;
         Perform (Run, Mask_Source, Success); if not Success then return; end if;
         if Trigger = Edge_Triggered then
            Run.Info.Stage := Clearing_Edge;
            Perform (Run, Clear_Status, Success); if not Success then return; end if;
         end if;
         Run.Info.Stage := Ready;
      end Start;
      procedure Record_Completion
        (Run : in out Session; Outcome : Handler_Outcome; Charge : Natural)
        with Always_Terminates, Pre => Valid (Run) and then Run.Info.Stage = Executing,
          Post => Valid (Run) and then Run.Info.Source = Known_Masked
            and then not Run.Info.Deferred
            and then Run.Info.Stage in Handler_Done | Failed
            and then Run.Info.Charged <= Run.Info.Handler_Budget
            and then Run.Info.Handler_Budget = Run.Info.Handler_Budget'Old;
      procedure Record_Completion
        (Run : in out Session; Outcome : Handler_Outcome; Charge : Natural) is
      begin
         Run.Info.Deferred := False;
         Run.Info.Handler_Status := Outcome; Run.Info.Reported_Charge := Charge;
         if Charge > Run.Info.Handler_Budget then
            Run.Info.Reason := Provider_Contract_Error; Run.Info.Stage := Failed; return;
         end if;
         Run.Info.Charged := Charge;
         case Outcome is
            when Handler_Succeeded => Run.Info.Stage := Handler_Done; return;
            when Handler_Budget_Exhausted => Run.Info.Reason := Handler_Budget_Error;
            when Handler_Completion_Unknown => Run.Info.Reason := Uncertain_Completion;
            when others => Run.Info.Reason := Handler_Error;
         end case;
         Run.Info.Stage := Failed;
      end Record_Completion;
      procedure Service
        (Run : in out Session; Budget : Natural; Result : out Action_Result) is
         Outcome : Handler_Outcome;
         Charge : Natural;
      begin
         Result := Wrong_Phase; if Run.Info.Stage /= Ready then return; end if;
         Result := No_Budget; if Budget = 0 then return; end if;
         Result := Accepted; Run.Info.Stage := Executing;
         Run.Info.Last_Operation := Invoke_Handler;
         Run.Info.Handler_Budget := Budget; Run.Info.Deferred := False;
         Invoke (Run.ID, Budget, Outcome, Charge);
         Record_Completion (Run, Outcome, Charge);
      exception
         when others =>
            Run.Info.Stage := Failed; Run.Info.Reason := Callback_Exception;
            Run.Info.Handler_Status := Handler_Completion_Unknown; Run.Info.Deferred := False;
      end Service;
      procedure Begin_Deferred
        (Run : in out Session; Budget : Natural; Result : out Action_Result) is
      begin
         Result := Wrong_Phase; if Run.Info.Stage /= Ready then return; end if;
         Result := No_Budget; if Budget = 0 then return; end if;
         Result := Accepted; Run.Info.Stage := Executing;
         Run.Info.Last_Operation := Invoke_Handler;
         Run.Info.Handler_Budget := Budget; Run.Info.Deferred := True;
      end Begin_Deferred;
      procedure Complete_Deferred
        (Run : in out Session; Outcome : Handler_Outcome; Charged : Natural;
         Result : out Action_Result) is
      begin
         Result := Wrong_Phase;
         if Run.Info.Stage /= Executing or else not Run.Info.Deferred then return; end if;
         Result := Accepted;
         Record_Completion (Run, Outcome, Charged);
      end Complete_Deferred;
      procedure Finish (Run : in out Session; Result : out Action_Result) is
         Success : Boolean;
      begin
         Result := Wrong_Phase; if Run.Info.Stage /= Handler_Done then return; end if;
         Result := Accepted;
         if Run.Info.Trigger = Level_Triggered then
            Run.Info.Stage := Clearing_Level;
            Perform (Run, Clear_Status, Success); if not Success then return; end if;
         end if;
         Run.Info.Stage := Enabling;
         Perform (Run, Enable_Source, Success);
         if Success then Run.Info.Stage := Finished; end if;
      end Finish;
   end Driver;
end ACPI_GPE_Lifecycle;
