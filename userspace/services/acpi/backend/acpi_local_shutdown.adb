pragma Ada_2022;
package body ACPI_Local_Shutdown with SPARK_Mode is
   use type Word;
   function Observe (State : Context) return Report is (State.Info);
   function May_Retire (State : Context) return Boolean is
     (State.Info.Stage = Quiescent and then not State.Info.Working
      and then State.Info.Consumers_Closed and then State.Info.Local_Closed
      and then State.Info.Hardware_Closing and then State.Info.Fields_Cleaned
      and then State.Info.Hardware_Idle and then State.Info.Others_Idle);
   procedure Request_Stop (State : in out Context; Accepted : out Boolean) is
      Good : Boolean;
   begin
      Accepted := False;
      if State.Info.Working then return; end if;
      if State.Info.Stage /= Open then
         Accepted := State.Info.Stage in Draining | Waiting | Retained | Quiescent;
         return;
      end if;
      State.Info.Working := True; State.Info.Stage := Closing;
      Close_Consumers (Good);
      if not Good then State.Info.Stage := Refused; State.Info.Working := False; return; end if;
      State.Info.Consumers_Closed := True;
      Close_Local; State.Info.Local_Closed := True;
      Begin_Hardware_Stop; State.Info.Hardware_Closing := True;
      State.Info.Stage := Draining; State.Info.Working := False; Accepted := True;
   end Request_Stop;
   procedure Advance (State : in out Context) is
      Hardware_Deadline, Other_Deadline : Word := No_Deadline;
      Hardware_Continues, Cleaned, Other_Idle : Boolean := False;
   begin
      if State.Info.Working or else State.Info.Stage not in Draining | Waiting | Retained then return; end if;
      State.Info.Working := True; State.Info.Stage := Retained;
      State.Info.Wake_Deadline_Milliseconds := No_Deadline;
      Maintain_Hardware (Hardware_Deadline,Hardware_Continues);
      State.Info.Hardware_Continues := Hardware_Continues;
      if not State.Info.Fields_Cleaned then
         Cleanup_Fields (Cleaned); State.Info.Fields_Cleaned := Cleaned;
      end if;
      State.Info.Hardware_Idle := Hardware_May_Exit;
      Other_Owners_View (Other_Idle,Other_Deadline); State.Info.Others_Idle := Other_Idle;
      if State.Info.Fields_Cleaned and then State.Info.Hardware_Idle and then Other_Idle then
         State.Info.Stage := Quiescent;
      else
         if not State.Info.Hardware_Idle then State.Info.Wake_Deadline_Milliseconds := Hardware_Deadline; end if;
         if not Other_Idle then
            State.Info.Wake_Deadline_Milliseconds := Word'Min
              (State.Info.Wake_Deadline_Milliseconds,Other_Deadline);
         end if;
         if State.Info.Wake_Deadline_Milliseconds /= No_Deadline then State.Info.Stage := Waiting; end if;
      end if;
      State.Info.Working := False;
   end Advance;
end ACPI_Local_Shutdown;
