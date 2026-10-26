with XHCI_Registers; use XHCI_Registers;

package body XHCI_Port_Machine with SPARK_Mode => On is

   function Observe (Status : Unsigned_32) return Observation is
     ((Connected       => Connected (Status),
       Enabled         => Enabled (Status),
       Resetting       => In_Reset (Status),
       Super_Speed     => Is_Super_Speed (Status),
       Connect_Changed => (Status and PORTSC_CSC) /= 0,
       Enable_Changed  => (Status and PORTSC_PEC) /= 0));

   procedure Start_Debounce (S : out Port_State; Now : Milliseconds)
   with Post => S.Current = Debouncing and then S.Stable_From = Now
     and then S.Connected_At = Now
   is
   begin
      S := (Current => Debouncing, Connected_At => Now, Stable_From => Now,
            Deadline => Plus (Now, Debounce_Stable_Ms));
   end Start_Debounce;

   procedure Step
     (S : in out Port_State; O : Observation; Now : Milliseconds; A : out Action)
   is
   begin
      A := No_Action;
      if not O.Connected then
         case S.Current is
            when Enumerating | Attached | Failed =>
               S.Current := Releasing;
               S.Deadline := No_Deadline;
               A := Release_Slots;
            when Releasing =>
               null;
            when others =>
               S := (Current => Empty, others => <>);
         end case;
         return;
      end if;

      case S.Current is
         when Empty =>
            Start_Debounce (S, Now);

         when Debouncing =>
            if O.Connect_Changed then
               --  Bounced: the stable interval starts again.
               S.Stable_From := Now;
               S.Deadline := Plus (Now, Debounce_Stable_Ms);
               if Since (Now, S.Connected_At) >= Debounce_Limit_Ms then
                  S.Current := Failed;
                  S.Deadline := No_Deadline;
                  A := Give_Up;
               end if;
            elsif Since (Now, S.Stable_From) >= Debounce_Stable_Ms then
               if O.Enabled and then not O.Resetting then
                  --  Link training enabled it (USB3): no reset needed.
                  S.Current := Recovering;
                  S.Deadline := Plus (Now, Reset_Recovery_Ms);
               elsif O.Resetting then
                  --  A reset is already underway: wait for it, never restart.
                  S.Current := Resetting;
                  S.Deadline := Plus (Now, Reset_Limit_Ms);
               else
                  S.Current := Resetting;
                  S.Deadline := Plus (Now, Reset_Limit_Ms);
                  A := Start_Reset;
               end if;
            end if;

         when Resetting =>
            if O.Enabled and then not O.Resetting then
               S.Current := Recovering;
               S.Deadline := Plus (Now, Reset_Recovery_Ms);
            elsif Now >= S.Deadline then
               S.Current := Failed;
               S.Deadline := No_Deadline;
               A := Give_Up;
            end if;

         when Recovering =>
            if Now >= S.Deadline then
               S.Current := Ready;
               S.Deadline := No_Deadline;
            end if;

         when Ready =>
            null;

         when Enumerating | Attached =>
            --  A new connection, or the controller disabled the port
            --  (an error, 4.19.1): this device is gone; start over.
            if O.Connect_Changed or else (O.Enable_Changed and then not O.Enabled) then
               S.Current := Releasing;
               S.Deadline := No_Deadline;
               A := Release_Slots;
            end if;

         when Failed =>
            if O.Connect_Changed then
               S.Current := Releasing;
               S.Deadline := No_Deadline;
               A := Release_Slots;
            end if;

         when Releasing =>
            null;
      end case;
   end Step;

   procedure Claim (S : in out Port_State) is
   begin
      S.Current := Enumerating;
   end Claim;

   procedure Finished (S : in out Port_State; Success : Boolean) is
   begin
      S.Current := (if Success then Attached else Failed);
      S.Deadline := No_Deadline;
   end Finished;

   procedure Recover (S : in out Port_State) is
   begin
      S.Current := Releasing;
      S.Deadline := No_Deadline;
   end Recover;

   procedure Released (S : in out Port_State; Connected : Boolean; Now : Milliseconds) is
   begin
      if Connected then
         Start_Debounce (S, Now);
      else
         S := (Current => Empty, others => <>);
      end if;
   end Released;

   function Booted (Connected, Has_Slots : Boolean; Now : Milliseconds) return Port_State is
     (if Has_Slots then (Current => Attached, others => <>)
      elsif Connected then
        (Current => Debouncing, Connected_At => Now, Stable_From => Now,
         Deadline => Plus (Now, Debounce_Stable_Ms))
      else (Current => Empty, others => <>));

end XHCI_Port_Machine;
