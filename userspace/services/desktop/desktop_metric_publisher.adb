with CuBit.Metrics;
with Compositor_Requests;
with Compositor_Release_Metrics;
with Compositor_Stage_Metrics;
with Compositor_Work_Metrics;
with Compositor_Metric_Batch_Policy;
with Compositor_Metric_Completion;
with Compositor_Trace_Publication;
with Compositor_Trace_Metrics;
package body Desktop_Metric_Publisher with SPARK_Mode => Off is
   use Interfaces;
   package SDK renames CuBit.Metrics;
   package CR renames Compositor_Requests;
   package RM renames Compositor_Release_Metrics;
   package SM renames Compositor_Stage_Metrics;
   package WM renames Compositor_Work_Metrics;
   package BP renames Compositor_Metric_Batch_Policy;
   package MC renames Compositor_Metric_Completion;
   package TP is new Compositor_Trace_Publication;
   package TM renames Compositor_Trace_Metrics;
   package TW renames Compositor_Trace_Wire;
   Trace : TP.State;
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
   function Invalid return Unsigned_64 is (Add (Bad, TP.Invalid (Trace)));
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

   procedure Write_Record (Value : SDK.Records.Metric_Record; Now : Unsigned_64) is
      Accepted : Boolean;
   begin
      SDK.Put (Item, Value, Accepted);
      if Accepted then BP.Accepted (Batch, Now);
      else Quarantine; end if;
   end Write_Record;

   procedure Descriptions (Now : Unsigned_64) is
   begin
      for Kind in BP.Description loop
         if BP.Next (Batch) = Kind then
            case Kind is
               when BP.Describe_Output_0 => Write_Record (RM.Declaration (0), Now);
               when BP.Describe_Output_1 => Write_Record (RM.Declaration (1), Now);
               when BP.Describe_Input => Write_Record (SM.Declaration (SM.Input_Dispatch), Now);
               when BP.Describe_Request => Write_Record (SM.Declaration (SM.Request_Dispatch), Now);
               when BP.Describe_Draw => Write_Record (SM.Declaration (SM.Scene_Draw), Now);
               when BP.Describe_Submit => Write_Record (SM.Declaration (SM.Submit_Call), Now);
               when BP.Describe_Scene_Pixels => Write_Record (WM.Declaration (WM.Scene_Pixels), Now);
               when BP.Describe_Repair_Pixels => Write_Record (WM.Declaration (WM.Repair_Pixels), Now);
               when BP.Describe_GPU_Readback_Bytes => Write_Record (WM.Declaration (WM.GPU_Readback_Bytes), Now);
               when BP.Describe_CPU_Copy_Bytes => Write_Record (WM.Declaration (WM.CPU_Copy_Bytes), Now);
               when BP.Describe_Completion => Write_Record (SM.Declaration (SM.Completion_Dispatch), Now);
               when BP.Describe_Diagnostic => Write_Record (SM.Declaration (SM.Diagnostic_Output), Now);
            end case;
            if Disabled then return; end if;
         end if;
      end loop;
   end Descriptions;

   procedure Append (Value : SDK.Records.Metric_Record; Now : Unsigned_64) is
      Accepted : Boolean;
   begin
      if Disabled then Local_Drops := Add (Local_Drops, 1); return; end if;
      if not SDK.Has_Room (Item) then
         SDK.Put (Item, Value, Accepted);
         if Accepted then Quarantine; end if;
         return;
      end if;
      Descriptions (Now);
      if Disabled then Local_Drops := Add (Local_Drops, 1); return; end if;
      if BP.Next (Batch) /= BP.Measurement then
         Local_Drops := Add (Local_Drops, 1); Quarantine; return;
      end if;
      Write_Record (Value, Now);
   end Append;

   procedure Record_Trace (Value : TW.Event) is
      Prepared : TP.Prepared;
      Accepted, Room : Boolean;
      Now : Unsigned_64;
   begin
      TP.Prepare (Trace, Value, Prepared);
      if not Prepared.Ready then
         if TP.Valid_Content (Value) then Local_Drops := Add (Local_Drops, 4); end if;
         return;
      end if;
      if Disabled then
         TP.Note_Refusal (Trace); Local_Drops := Add (Local_Drops, 4); return;
      end if;
      Now := (case Prepared.Value.Kind is
         when TW.Input_Event => Prepared.Value.Input.Dequeued,
         when TW.Source_Event => Prepared.Value.Source.Accepted,
         when TW.Render_Event => Prepared.Value.Render.Observed,
         when TW.Frame_Event => Prepared.Value.Frame.Completed);
      if SDK.Has_Room (Item) then Descriptions (Now); end if;
      if Disabled then
         TP.Note_Refusal (Trace); Local_Drops := Add (Local_Drops, 4); return;
      end if;
      Room := SDK.Has_Group_Room (Item);
      if Room and then not BP.Group_Room (Batch) then
         TP.Note_Refusal (Trace); Local_Drops := Add (Local_Drops, 4); Quarantine; return;
      end if;
      SDK.Put_Group (Item, TM.Fragment (Prepared.Value), Accepted);
      if Accepted /= Room then
         TP.Note_Refusal (Trace); Quarantine; return;
      elsif Accepted then
         BP.Accepted_Group (Batch);
      else
         TP.Note_Refusal (Trace);
      end if;
      if BP.Samples (Batch) > 0 and then not BP.Group_Room (Batch) then
         BP.Request_Flush (Batch);
      end if;
   end Record_Trace;

   procedure Record_Unsupported_Trace is
   begin
      TP.Note_Unsupported (Trace);
   end Record_Unsupported_Trace;

   procedure Record_Trace_Status (Now : Unsigned_64) is
      package R renames SDK.Records;
      Value : Unsigned_64;
   begin
      if Now = Unsigned_64'Last then Bad := Add (Bad, 1); return; end if;
      for Key in R.Metric_Key range 13 .. 15 loop
         Value := (case Key is when 13 => TP.Invalid (Trace),
                   when 14 => TP.Refused (Trace), when others => TP.Unsupported (Trace));
         Append ((R.Describe, Key, R.Gauge, R.Count,
           R.To_Name ((case Key is when 13 => "desktop.trace.invalid",
                       when 14 => "desktop.trace.refused",
                       when others => "desktop.trace.unsupported"))), Now);
         Append ((R.Gauge, Key, Now, Value, 0), Now);
      end loop;
   end Record_Trace_Status;

   procedure Record_Work (Kind : WM.Work_Kind; Pixels, Now : Unsigned_64) is
      Value : constant WM.Sample := WM.Prepare (Kind, Pixels, Now);
   begin
      if not Value.Valid then Bad := Add (Bad, 1); return; end if;
      Append (Value.Value, Now);
   end Record_Work;

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
