with Ada.Text_IO;
with Trace_Publication_Instances;
with Compositor_Trace_Wire;
with Compositor_Metric_Batch_Policy;
procedure Trace_Publication_Check is
   package P renames Trace_Publication_Instances.Short_Run;
   package F renames Trace_Publication_Instances.Production;
   package Z renames Trace_Publication_Instances.Empty_Run;
   package W renames Compositor_Trace_Wire;
   package B renames Compositor_Metric_Batch_Policy;
   use type W.Word, W.Event;
   S : P.State;
   Full : F.State;
   Empty : Z.State;
   Ready : P.Prepared;
   Long_Result : F.Prepared;
   Empty_Result : Z.Prepared;
   Input : constant W.Event := (W.Input_Event, W.Word'Last, (W.Word'Last, 1, 1, 0));
   Bad : constant W.Event := (W.Input_Event, 9, (0, 0, 0, W.Word'Last));
   Checks : Natural := 0;
   procedure Check (V : Boolean) is
   begin
      Checks := Checks + 1;
      if not V then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   P.Prepare (S, Bad, Ready);
   Check (not Ready.Ready and P.Invalid (S) = 1 and P.Following (S) = 1);
   for I in 1 .. 7 loop
      P.Prepare (S, Input, Ready);
      Check (Ready.Ready and then Ready.Value = P.Identified (Input, W.Word (I)));
      P.Note_Refusal (S);
      Check (P.Refused (S) = W.Word (I) and P.Following (S) = W.Word (I + 1));
   end loop;
   for I in 1 .. 1000 loop
      P.Prepare (S, Input, Ready);
      Check (not Ready.Ready and P.Following (S) = 8 and P.Refused (S) = W.Word (I + 7));
      P.Note_Unsupported (S); Check (P.Unsupported (S) = W.Word (I));
   end loop;
   Z.Prepare (Empty, Input, Empty_Result);
   Check (not Empty_Result.Ready and Z.Following (Empty) = 1 and Z.Refused (Empty) = 1);
   Check (P.Increment (W.Word'Last) = W.Word'Last);
   for I in 1 .. 10000 loop
      F.Prepare (Full, Input, Long_Result);
      Check (Long_Result.Ready and then Long_Result.Value.Event_ID = W.Word (I));
   end loop;
   for Used in 0 .. B.Capacity loop
      declare
         Batch : B.State;
      begin
         for I in 1 .. Used loop B.Accepted (Batch, 100); end loop;
         Check (B.Group_Room (Batch) = (Used in B.Declaration_Count .. B.Capacity - 4));
         if B.Group_Room (Batch) then
            B.Accepted_Group (Batch);
            Check (B.Used (Batch) = Used + 4 and B.First (Batch) = 100);
         end if;
         B.Request_Flush (Batch);
         Check (B.Due (Batch, 100) = (B.Samples (Batch) > 0));
         if B.Samples (Batch) > 0 then Check (B.Delay_Us (Batch, 100) = 0); end if;
         B.Submitted (Batch);
         Check (not B.Due (Batch, 100) and B.Delay_Us (Batch, 100) = W.Word'Last);
         for I in 1 .. B.Declaration_Count + 1 loop B.Accepted (Batch, 200); end loop;
         Check (not B.Due (Batch, 200) and B.Delay_Us (Batch, 200) = B.Flush_Interval_Us);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS trace publication checks" & Checks'Image);
end Trace_Publication_Check;
