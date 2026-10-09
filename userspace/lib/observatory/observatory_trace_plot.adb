package body Observatory_Trace_Plot with SPARK_Mode is
   package W renames A.W;
   use type W.RT.Phase;
   function Describe (Value : A.S.Capture) return Interval is
   begin
      case Value.Value.Kind is
         when W.Input_Event => return (0, Value.Value.Input.Dequeued, Value.Value.Input.Dequeued);
         when W.Source_Event => return (1, Value.Value.Source.Accepted, Value.Value.Source.Accepted);
         when W.Render_Event => return ((if Value.Value.Render.Kind = W.RT.Draw then 2 else 3),
                                         Value.Value.Render.Observed, Value.Value.Render.Observed);
         when W.Frame_Event => return (4, Value.Value.Frame.Submitted, Value.Value.Frame.Completed);
      end case;
   end Describe;
   function Extent (View : V.State) return Bounds is
      Result : Bounds;
      Item : Interval;
   begin
      for I in 1 .. V.Length (View) loop
         Item := Describe (V.Value_At (View, I));
         if I = 1 then Result := (Item.First, Item.Last);
         else Result := (Word'Min (Result.First, Item.First), Word'Max (Result.Last, Item.Last)); end if;
         pragma Loop_Invariant (Result.First <= Result.Last);
         pragma Loop_Invariant (for all J in 1 .. I =>
           Result.First <= Describe (V.Value_At (View, J)).First and
           Describe (V.Value_At (View, J)).Last <= Result.Last);
      end loop;
      return Result;
   end Extent;
   function Project (Value : Interval; Window : Bounds; Size : Width) return Bar is
      Left : constant Natural := Natural'Min (Size - 1,
        Observatory_History.Scale (Value.First - Window.First, Window.Last - Window.First, Size));
      Right : constant Natural := Natural'Max (Left + 1,
        Observatory_History.Scale (Value.Last - Window.First, Window.Last - Window.First, Size));
   begin return (Left, Right - Left); end Project;
end Observatory_Trace_Plot;
