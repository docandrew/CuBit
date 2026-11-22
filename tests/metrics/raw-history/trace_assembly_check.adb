with Ada.Text_IO;
with Compositor_Trace_Metrics; use Compositor_Trace_Metrics;
procedure Trace_Assembly_Check is
   use type W.Word, W.Event;
   Rows, Bad : Group;
   Parts : R.Trace_Group;
   Event : W.Event;
   Answer : W.Decoded;
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for Iteration in 1 .. 1000 loop
      Event := (W.Render_Event, W.Word'Last - W.Word (Iteration),
                (W.RT.Draw, 1, 3, W.Word'Last, W.Word'Last - 1,
                 W.Word'Last - 2, W.Word'Last - 3, W.Word'Last - 4, 0, 0, 0));
      Parts := Fragment (Event);
      for I in R.Trace_Part loop
         Rows (I) := (W.Word (Iteration + I), 99, P.Publisher_Tag (1),
                      W.Word (Iteration), Parts (I));
      end loop;
      Answer := Assemble (Rows);
      Check (Answer.Success and then Answer.Value = Event);
      for I in R.Trace_Part loop
         Bad := Rows; Bad (I).Pid := 100;
         Check (not Assemble (Bad).Success);
         Bad := Rows; Bad (I).Publisher := P.Publisher_Tag (2);
         Check (not Assemble (Bad).Success);
         Bad := Rows; Bad (I).Batch := Bad (I).Batch + 1;
         Check (not Assemble (Bad).Success);
         Bad := Rows; Bad (I).Sequence := Bad (I).Sequence + 1;
         Check (not Assemble (Bad).Success);
         Bad := Rows; Bad (I).Value.Trace_ID := Bad (I).Value.Trace_ID - 1;
         Check (not Assemble (Bad).Success);
         Bad := Rows; Bad (I).Value.Part := (I + 1) mod 4;
         Check (not Assemble (Bad).Success);
         Bad := Rows; Bad (I).Value.Key := 2;
         Check (not Assemble (Bad).Success);
      end loop;
   end loop;
   Bad := Rows;
   for I in R.Trace_Part loop Bad (I).Value.Trace_ID := 1; end loop;
   Check (not Assemble (Bad).Success); -- envelope ID differs from packet ID
   Bad := Rows; Bad (0).Value.Data (0) := 0;
   Check (not Assemble (Bad).Success); -- missing schema/version
   Bad := Rows; Bad (0).Sequence := W.Word'Last;
   Check (not Assemble (Bad).Success); -- no wrap acceptance
   Bad := Rows;
   for I in R.Trace_Part loop
      Bad (I).Sequence := W.Word'Last - 3 + W.Word (I);
   end loop;
   Check (Assemble (Bad).Success);
   Bad := Rows;
   for I in R.Trace_Part loop Bad (I).Publisher := P.Observer_Tag (1); end loop;
   Check (not Assemble (Bad).Success);
   Bad := Rows;
   for I in R.Trace_Part loop Bad (I).Pid := 0; end loop;
   Check (not Assemble (Bad).Success);
   Bad := Rows;
   for I in R.Trace_Part loop Bad (I).Batch := 0; end loop;
   Check (not Assemble (Bad).Success);
   Ada.Text_IO.Put_Line ("PASS trace assembly checks" & Checks'Image);
end Trace_Assembly_Check;
