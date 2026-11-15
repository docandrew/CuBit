pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Local_Shutdown;
procedure Shutdown_Tests is
   Mode : constant String := Ada.Command_Line.Argument (1);
   Checks, Consumer_Calls, Close_Calls, Stop_Calls, Maintain_Calls,
      Cleanup_Calls, View_Calls : Natural := 0;
   Consumers_Accept, Fields_Done, Hardware_Idle, Other_Idle : Boolean := True;
   Continues : Boolean := True;
   Hardware_Deadline : Unsigned_64 := 100;
   Other_Deadline : Unsigned_64 := 90;
   procedure Check (Good : Boolean) is
   begin Checks := Checks+1; if not Good then raise Program_Error with Mode & Checks'Image; end if; end Check;
   procedure Try_Reentry;
   procedure Close_Consumers (Accepted : out Boolean) is
   begin
      Consumer_Calls := Consumer_Calls+1;
      Check (Close_Calls = 0 and Stop_Calls = 0);
      if Mode = "reentry" then Try_Reentry; end if;
      Accepted := Consumers_Accept;
   end Close_Consumers;
   procedure Close_Local is
   begin
      Close_Calls := Close_Calls+1; Check (Consumer_Calls = 1 and Stop_Calls = 0);
      if Mode = "close-exception" then raise Program_Error with "modeled close"; end if;
   end Close_Local;
   procedure Begin_Stop is
   begin Stop_Calls := Stop_Calls+1; Check (Close_Calls = 1); end Begin_Stop;
   procedure Maintain (Deadline : out Unsigned_64; May_Continue : out Boolean) is
   begin
      Maintain_Calls := Maintain_Calls+1; Check (Stop_Calls = 1);
      if Mode = "maintenance-exception" then raise Program_Error with "modeled maintenance"; end if;
      Deadline := Hardware_Deadline; May_Continue := Continues;
   end Maintain;
   procedure Cleanup (Completed : out Boolean) is
   begin Cleanup_Calls := Cleanup_Calls+1; Check (Maintain_Calls > 0); Completed := Fields_Done; end Cleanup;
   function May_Exit return Boolean is (Hardware_Idle);
   procedure View (Quiescent : out Boolean; Deadline : out Unsigned_64) is
   begin View_Calls := View_Calls+1; Quiescent := Other_Idle; Deadline := Other_Deadline; end View;
   package Control is new ACPI_Local_Shutdown
     (Close_Consumers,Close_Local,Begin_Stop,Maintain,Cleanup,May_Exit,View);
   State : Control.Context;
   use type Control.Phase;
   procedure Try_Reentry is
      Accepted : Boolean;
   begin
      Control.Request_Stop (State,Accepted); Check (not Accepted);
      Control.Advance (State); Check (Maintain_Calls = 0 and not Control.May_Retire (State));
   end Try_Reentry;
   Accepted, Raised : Boolean := False;
begin
   Control.Advance (State);
   Check (not Control.May_Retire (State) and Consumer_Calls = 0 and Maintain_Calls = 0);
   if Mode = "consumers-refused" then Consumers_Accept := False;
   elsif Mode in "hardware-wait" | "earliest" | "retained" | "continue-false" | "cached-cleanup" then
      Hardware_Idle := False;
   end if;
   if Mode in "other-wait" | "earliest" | "retained" then Other_Idle := False; end if;
   if Mode = "retained" then Hardware_Deadline := Control.No_Deadline; Other_Deadline := Control.No_Deadline; end if;
   if Mode = "continue-false" then Continues := False; end if;
   if Mode = "fields-wait" then Fields_Done := False; end if;
   begin Control.Request_Stop (State,Accepted);
   exception when Program_Error => if Mode /= "close-exception" then raise; end if; Raised := True; end;
   if Mode = "close-exception" then
      Check (Raised and Control.Observe (State).Working and not Control.May_Retire (State));
      Control.Advance (State); Control.Request_Stop (State,Accepted);
      Check (not Accepted and Consumer_Calls = 1 and Close_Calls = 1 and Stop_Calls = 0 and Maintain_Calls = 0);
   elsif Mode = "consumers-refused" then
      Check (not Accepted and Control.Observe (State).Stage = Control.Refused);
      Control.Advance (State); Control.Request_Stop (State,Accepted);
      Check (not Accepted and Consumer_Calls = 1 and Close_Calls = 0 and Stop_Calls = 0);
      Check (not Control.May_Retire (State));
   else
      Check (Accepted and not Control.May_Retire (State));
      Check (Control.Observe (State).Consumers_Closed and Control.Observe (State).Local_Closed
        and Control.Observe (State).Hardware_Closing);
      begin Control.Advance (State);
      exception when Program_Error => if Mode /= "maintenance-exception" then raise; end if; Raised := True; end;
      if Mode = "maintenance-exception" then
         Check (Raised and Control.Observe (State).Working and not Control.May_Retire (State));
         Control.Advance (State); Check (Maintain_Calls = 1 and Cleanup_Calls = 0 and View_Calls = 0);
      else
         if Mode in "hardware-wait" | "continue-false" | "cached-cleanup" then
            Check (Control.Observe (State).Hardware_Continues = Continues);
            Check (not Control.May_Retire (State) and Control.Observe (State).Stage = Control.Waiting);
            Check (Control.Observe (State).Wake_Deadline_Milliseconds = Hardware_Deadline);
            Control.Advance (State); Check (Cleanup_Calls = 1);
            Hardware_Idle := True;
         elsif Mode = "other-wait" then
            Check (not Control.May_Retire (State) and Control.Observe (State).Wake_Deadline_Milliseconds = Other_Deadline);
            Other_Idle := True;
         elsif Mode = "earliest" then
            Check (not Control.May_Retire (State) and Control.Observe (State).Wake_Deadline_Milliseconds = Other_Deadline);
            Control.Advance (State); Check (Control.Observe (State).Wake_Deadline_Milliseconds = Other_Deadline);
            Other_Idle := True; Control.Advance (State);
            Check (Control.Observe (State).Wake_Deadline_Milliseconds = Hardware_Deadline);
            Hardware_Idle := True;
         elsif Mode = "retained" then
            Check (not Control.May_Retire (State) and Control.Observe (State).Stage = Control.Retained);
            Hardware_Idle := True; Control.Advance (State);
            Check (not Control.May_Retire (State) and Control.Observe (State).Stage = Control.Retained);
            Other_Idle := True;
         elsif Mode = "fields-wait" then
            Check (not Control.May_Retire (State) and Control.Observe (State).Stage = Control.Retained);
            Check (Control.Observe (State).Wake_Deadline_Milliseconds = Control.No_Deadline);
            Fields_Done := True;
         end if;
         Control.Advance (State);
         Check (Control.May_Retire (State) and Control.Observe (State).Stage = Control.Quiescent);
         Check (Cleanup_Calls = (if Mode = "fields-wait" then 2 else 1));
         declare Before : constant Natural := Maintain_Calls; begin
            Control.Advance (State); Control.Request_Stop (State,Accepted);
            Check (Accepted and Control.May_Retire (State) and Maintain_Calls = Before);
         end;
      end if;
      Check (Consumer_Calls = 1 and Close_Calls = 1 and Stop_Calls = 1);
   end if;
   Ada.Text_IO.Put_Line ("LOCAL SHUTDOWN mode=" & Mode & " checks" & Checks'Image);
end Shutdown_Tests;
