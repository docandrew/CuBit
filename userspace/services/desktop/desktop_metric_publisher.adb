with CuBit.Metrics;
with Compositor_Requests;
with Compositor_Release_Metrics;
with Compositor_Stage_Metrics;
with Compositor_Metric_Batch_Policy;
with Compositor_Metric_Completion;
package body Desktop_Metric_Publisher with SPARK_Mode => Off is
   use Interfaces;
   package SDK renames CuBit.Metrics;
   package CR renames Compositor_Requests;
   package RM renames Compositor_Release_Metrics;
   package SM renames Compositor_Stage_Metrics;
   package BP renames Compositor_Metric_Batch_Policy;
   package MC renames Compositor_Metric_Completion;
   use type BP.Append_Kind;
   Item : SDK.Publisher (Capability);
   Batch : BP.State;
   Flights : array (1 .. 2) of CR.State;
   Sent : array (Flights'Range) of SDK.Records.Record_Count := [others => 0];
   Off : Boolean := False;
   Local_Drops, Bad : Unsigned_64 := 0;
   function Add (A, B : Unsigned_64) return Unsigned_64 is
     (if B > Unsigned_64'Last - A then Unsigned_64'Last else A + B);
   function Disabled return Boolean is (Off or SDK.Disabled (Item));
   function Dropped return Unsigned_64 is (Add (Local_Drops, SDK.Dropped (Item)));
   function Invalid return Unsigned_64 is (Bad);
   function Rejected return Unsigned_64 is (SDK.Rejected (Item));
   function Pending return Boolean is (not Disabled and then BP.Samples (Batch) > 0);
   function Delay_Us (Now : Unsigned_64) return Unsigned_64 is
     (if Disabled then Unsigned_64'Last else BP.Delay_Us (Batch, Now));

   procedure Quarantine is
   begin
      Off := True;
      for Flight of Flights loop
         if CR.Busy (Flight) then CR.Quarantine (Flight); end if;
      end loop;
      -- No revocation/reuse while a grant's reader state is uncertain.
   end Quarantine;

   procedure Append (Value : SDK.Records.Metric_Record; Now : Unsigned_64) is
      Accepted : Boolean;
      procedure Write (Record_Value : SDK.Records.Metric_Record) is
      begin
         SDK.Put (Item, Record_Value, Accepted);
         if Accepted then BP.Accepted (Batch, Now);
         else Quarantine; end if;
      end Write;
   begin
      if Disabled then Local_Drops := Add (Local_Drops, 1); return; end if;
      if not SDK.Has_Room (Item) then
         SDK.Put (Item, Value, Accepted);
         if Accepted then Quarantine; end if;
         return;
      end if;
      -- At most six metadata records; no allocation, retry, or IPC here.
      for Kind in BP.Description loop
         if BP.Next (Batch) = Kind then
            case Kind is
               when BP.Describe_Output_0 => Write (RM.Declaration (0));
               when BP.Describe_Output_1 => Write (RM.Declaration (1));
               when BP.Describe_Input => Write (SM.Declaration (SM.Input_Dispatch));
               when BP.Describe_Request => Write (SM.Declaration (SM.Request_Dispatch));
               when BP.Describe_Draw => Write (SM.Declaration (SM.Scene_Draw));
               when BP.Describe_Submit => Write (SM.Declaration (SM.Submit_Call));
            end case;
            if Disabled then Local_Drops := Add (Local_Drops, 1); return; end if;
         end if;
      end loop;
      if BP.Next (Batch) /= BP.Measurement then
         Local_Drops := Add (Local_Drops, 1); Quarantine; return;
      end if;
      Write (Value);
   end Append;

   procedure Record_Completion (Frame : Compositor_Frame_Trace.Record_Value) is
      Value : constant RM.Sample := RM.Prepare (Frame);
   begin
      if not Value.Valid then Bad := Add (Bad, 1); return; end if;
      Append (Value.Value, Frame.Completed);
   end Record_Completion;

   procedure Record_Stage (Stage : SM.Stage; First, Last : Unsigned_64) is
      Value : constant SM.Sample := SM.Prepare (Stage, First, Last);
   begin
      if not Value.Valid then Bad := Add (Bad, 1); return; end if;
      Append (Value.Value, Last);
   end Record_Stage;

   procedure Pump (Sequence : in out Unsigned_64; Now : Unsigned_64) is
      Token : Unsigned_64;
      Begun, Submitted : Boolean;
   begin
      if Disabled or else not BP.Due (Batch, Now) then return; end if;
      for I in Flights'Range loop
         if CR.Available (Flights (I)) then
            CR.Allocate (Sequence, Token);
            if Token = 0 then Quarantine; return; end if;
            CR.Begin_Request (Flights (I), Token, Begun);
            if not Begun then Quarantine; return; end if;
            Sent (I) := BP.Used (Batch);
            SDK.Flush (Item, Token, Submitted);
            if Submitted then BP.Submitted (Batch);
            else Quarantine; end if;
            return;
         end if;
      end loop;
   end Pump;

   function Matches (Token : Unsigned_64) return Boolean is
     (Token /= 0 and then (for some Flight of Flights => CR.Token (Flight) = Token));

   procedure Collect (Completion : CuBit.Messages.CompletionEntry) is
      Handled : Boolean;
      Reply : constant MC.Reply :=
        (Completion.valid, Completion.status, Completion.msg.tag.label,
         Completion.msg.tag.length, Completion.msg.tag.flags, Completion.msg.tag.reserved,
         [for I in MC.Payload'Range => Completion.msg.words (I)]);
   begin
      for I in Flights'Range loop
         if CR.Token (Flights (I)) /= 0 and then CR.Token (Flights (I)) = Completion.token then
            if not CR.Busy (Flights (I)) then return; end if;
            if not MC.Definitive (Reply, Sent (I)) then
               Bad := Add (Bad, 1); Quarantine; return;
            end if;
            -- Invalid envelopes never reach the SDK's weaker completion gate.
            SDK.Complete (Item, Completion, Handled);
            if not Handled then Bad := Add (Bad, 1); Quarantine; return; end if;
            CR.Complete (Flights (I), Completion.token, True);
            if SDK.Disabled (Item) then Quarantine; end if;
            return;
         end if;
      end loop;
   end Collect;
end Desktop_Metric_Publisher;
