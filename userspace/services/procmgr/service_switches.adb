pragma Ada_2022;

package body Service_Switches with SPARK_Mode is

   procedure Begin_Switch
     (S : in out Switch; Old : Process_Word; Graceful : Boolean; Grace : SR.Stop_Grace_Ms;
      Health : SR.Health_Window_Ms; Now : Time; Next : out Action) is
   begin
      S := (State => Idle, Graceful => Graceful, Old_Process => Old, New_Process => No_Process,
            Deadline => Now, Grace => Grace, Health => Health, Must_Register => False,
            Old_Unclean => False, Result => SR.Switched, Failure => SR.Healthy);
      if Old = No_Process then
         S.State := Starting;
         Next := Start_Program;
      elsif Grace = 0 then
         S.State := Ending_Old;
         S.Deadline := Now + SR.End_Deadline_Ms;
         Next := End_Old;
      else
         S.State := Stopping_Old;
         S.Deadline := Now + Grace;
         Next := Ask_Stop;
      end if;
   end Begin_Switch;

   procedure Exited (S : in out Switch; Process : Process_Word; Clean : Boolean; Next : out Action) is
   begin
      Next := Nothing;
      case S.State is
         when Stopping_Old | Ending_Old =>
            if Process = S.Old_Process then
               if S.Graceful and then not Clean then
                  --  Not quiesced: never start the new one after it.
                  S.Old_Unclean := True;
                  S.State := Restoring;
                  Next := Start_Previous;
               else
                  S.State := Starting;
                  Next := Start_Program;
               end if;
            end if;
         when Checking =>
            if Process = S.New_Process then
               S.Failure := SR.Exited_Early;
               S.State := Restoring;
               Next := Start_Previous;
            end if;
         when Stopping_New | Ending_New =>
            if Process = S.New_Process then
               S.State := Restoring;
               Next := Start_Previous;
            end if;
         when Idle | Starting | Restoring | Finished =>
            null;
      end case;
   end Exited;

   procedure Started
     (S : in out Switch; Process : Process_Word; Must_Register : Boolean; Now : Time;
      Next : out Action) is
   begin
      if S.State = Starting then
         if Process /= No_Process then
            S.New_Process := Process;
            S.Must_Register := Must_Register;
            S.Deadline := Now + S.Health;
            S.State := Checking;
            Next := Nothing;
         else
            S.Failure := SR.Start_Refused;
            S.State := Restoring;
            Next := Start_Previous;
         end if;
      else
         S.Result := (if Process = No_Process then SR.Rollback_Failed
                      elsif S.Old_Unclean then SR.Stop_Failed
                      else SR.Rolled_Back);
         S.State := Finished;
         Next := Answer;
      end if;
   end Started;

   procedure Tick (S : in out Switch; Now : Time; Registered : Boolean; Next : out Action) is
   begin
      Next := Nothing;
      if not Waiting (S.State) or else Now < S.Deadline then
         return;
      end if;
      case S.State is
         when Stopping_Old =>
            if S.Graceful then
               --  It did not quiesce in time: it keeps running; nothing starts.
               S.Result := SR.Stop_Failed;
               S.State := Finished;
               Next := Answer;
            else
               S.State := Ending_Old;
               S.Deadline := Now + SR.End_Deadline_Ms;
               Next := End_Old;
            end if;
         when Ending_Old =>
            S.Result := SR.Stop_Failed;
            S.State := Finished;
            Next := Answer;
         when Checking =>
            if Registered or else not S.Must_Register then
               S.Result := SR.Switched;
               S.State := Finished;
               Next := Answer;
            else
               S.Failure := SR.Not_Registered;
               if S.Graceful and then S.Grace > 0 then
                  S.State := Stopping_New;
                  S.Deadline := Now + S.Grace;
                  Next := Ask_Stop_New;
               else
                  S.State := Ending_New;
                  S.Deadline := Now + SR.End_Deadline_Ms;
                  Next := End_New;
               end if;
            end if;
         when Stopping_New =>
            S.State := Ending_New;
            S.Deadline := Now + SR.End_Deadline_Ms;
            Next := End_New;
         when Ending_New =>
            S.Result := SR.Rollback_Failed;
            S.State := Finished;
            Next := Answer;
         when Idle | Starting | Restoring | Finished =>
            null;
      end case;
   end Tick;

   procedure Answered (S : in out Switch) is
   begin
      S := (others => <>);
   end Answered;
end Service_Switches;
