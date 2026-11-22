pragma Ada_2022;
package body GPIO_IRQ_Sweep with SPARK_Mode is
   package C renames R.C;
   package Channels renames R.Channels;
   package L renames Channels.Leases;
   use type C.Pin_ID, C.Engine.Phase, R.Channel_ID, R.Phase;
   use type Channels.Binding_Phase, Channels.Lifecycle, L.Phase;
   use type R.I.Tick;
   function Current (S : State) return Phase is (S.At_Phase);
   procedure Start (S : in out State; Deadline : R.I.Tick; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.At_Phase not in Idle | Source_Clear then return; end if;
      -- A coalesced kernel notice requests another sweep before parent ACK.
      -- It cannot release the inbox or renew the original sweep deadline.
      if S.At_Phase = Source_Clear and then
         (not Requests_Held or else Deadline /= S.Deadline) then return; end if;
      Hold_Requests (True);
      if not Requests_Held then S.At_Phase := Uncertain; return; end if;
      S.Deadline := Deadline; S.At_Phase := Await_Controls; Accepted := True;
   end Start;
   procedure Advance (S : in out State) is
      Waiting : Boolean := False;
   begin
      if S.At_Phase in Idle | Uncertain then return; end if;
      if not Requests_Held or else not R.Observe.Operational or else C.Observe.Faulted or else
         R.Observe.Layout_Changed or else R.Observe.Unresolved /= 0
      then S.At_Phase := Uncertain; return; end if;
      for ID in R.Channel_ID range 1 .. R.Observe.Registered loop
         declare Item : constant R.Binding_Report := R.Observe_Channel (ID); begin
            if Item.At_Phase in R.Close_Failed | R.Unresolved or else
               Item.Channel.Binding = Channels.Failed or else Item.Channel.Life = Channels.Unresolved or else
               Item.Channel.Answer_Lost or else Item.Channel.Pin.At_Phase = L.Faulted
            then S.At_Phase := Uncertain; return; end if;
            if Item.At_Phase in R.Registering | R.Binding | R.Retiring then Waiting := True;
            elsif Item.At_Phase = R.Live then
               if Item.Channel.Binding /= Channels.Ready or else Item.Channel.Life /= Channels.Running or else
                  Item.Channel.Control_Pending or else Item.Channel.Completion_Ready or else
                  Item.Channel.Pin.At_Phase not in L.Parked | L.Armed | L.Event_Retained | L.Event_Delivered
               then Waiting := True; end if;
            end if;
         end;
      end loop;
      if C.Observe.Active /= C.No_Pin or else Waiting then return; end if;
      if S.At_Phase = Await_Controls then
         if not R.Observe.Stopping then R.Poll_All (S.Deadline); end if;
         S.At_Phase := Await_Pins;
         return;
      end if;
      -- Inspect every registered hardware slot, including a released cached
      -- pin. Lease status alone cannot establish mask/readback completion.
      for Pin in C.Pin_ID range 1 .. C.Pin_ID (C.Observe.Registered) loop
         declare Item : constant C.Pin_Observation := C.Observe_Pin (Pin); begin
            if not Item.Present then S.At_Phase := Uncertain; return; end if;
            case Item.Report.At_Phase is
               when C.Engine.Listening =>
                  if R.Observe.Stopping then return; end if;
               when C.Engine.Held_Masked | C.Engine.Event_Ready =>
                  if Item.Report.Pin_May_Be_Enabled then S.At_Phase := Uncertain; return; end if;
               when C.Engine.Faulted => S.At_Phase := Uncertain; return;
               when others => return;
            end case;
         end;
      end loop;
      S.At_Phase := Source_Clear;
   end Advance;
   procedure Finish (S : in out State; Parent_ACK_Accepted : Boolean) is
   begin
      if S.At_Phase /= Source_Clear or else not Parent_ACK_Accepted then S.At_Phase := Uncertain; return; end if;
      S.At_Phase := Idle; Hold_Requests (False);
   end Finish;
end GPIO_IRQ_Sweep;
