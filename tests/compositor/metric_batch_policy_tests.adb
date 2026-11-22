with Ada.Text_IO;
with Compositor_Metric_Batch_Policy;
with Compositor_Elapsed;
procedure Metric_Batch_Policy_Tests is
   package B renames Compositor_Metric_Batch_Policy;
   use type B.Append_Kind, B.Tick;
   S : B.State;
begin
   for Batch in 1 .. 1000 loop
      pragma Assert (B.Next (S) = B.Describe_Output_0 and not B.Due (S, B.Tick'Last));
      B.Accepted (S, 500);
      pragma Assert (B.Next (S) = B.Describe_Output_1 and not B.Due (S, 200_000));
      B.Accepted (S, 501);
      for Kind in B.Describe_Input .. B.Description'Last loop
         pragma Assert (B.Next (S) = Kind and B.Samples (S) = 0);
         B.Accepted (S, 501);
      end loop;
      pragma Assert (B.Next (S) = B.Measurement and not B.Due (S, 200_000));
      for I in B.Declaration_Count + 1 .. B.Capacity loop
         pragma Assert (B.Next (S) = B.Measurement);
         B.Accepted (S, 502);
         pragma Assert (B.Used (S) = I and B.Samples (S) = I - B.Declaration_Count and B.First (S) = 500);
         pragma Assert (B.Due (S, 100_500));
         pragma Assert (B.Due (S, 499));
         pragma Assert (B.Due (S, Compositor_Elapsed.Unavailable));
         pragma Assert (B.Due (S, 100_499) = (I = B.Capacity));
      end loop;
      pragma Assert (B.Next (S) = B.Full and B.Samples (S) = B.Capacity - B.Declaration_Count);
      B.Submitted (S);
   end loop;
   for I in 1 .. B.Declaration_Count loop B.Accepted (S, B.Tick'Last - 2); end loop;
   B.Accepted (S, B.Tick'Last - 1);
   pragma Assert (not B.Due (S, B.Tick'Last - 1));
   pragma Assert (B.Due (S, B.Tick'Last));
   pragma Assert (B.Delay_Us (S, B.Tick'Last) = 0);
   B.Submitted (S);
   pragma Assert (B.Delay_Us (S, 0) = B.Tick'Last);
   for I in 1 .. B.Declaration_Count + 1 loop B.Accepted (S, 1000); end loop;
   pragma Assert (B.Delay_Us (S, 1000) = 100_000);
   pragma Assert (B.Delay_Us (S, 100_999) = 1);
   pragma Assert (B.Delay_Us (S, 101_000) = 0);
   pragma Assert (B.Delay_Us (S, 999) = 0);
   for Pause_Us in B.Tick range 0 .. B.Flush_Interval_Us loop
      pragma Assert (B.Wake_At_Ms (100, Pause_Us) = 100 + (Pause_Us + 999) / 1000);
      pragma Assert (B.Wake_At_Ms (B.Tick'Last - 1, Pause_Us) = B.Tick'Last - 1);
   end loop;
   Ada.Text_IO.Put_Line ("METRIC-BATCH-POLICY: PASS 1000 self-describing pages, exact capacity, clock faults and flush boundary");
end Metric_Batch_Policy_Tests;
