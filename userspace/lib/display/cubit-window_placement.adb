pragma Ada_2022;
package body CuBit.Window_Placement with SPARK_Mode is
   use type L.Named_Display_ID;

   function Fit (Area : Work_Area; Window : Local_Bounds)
      return G.Logical_Rectangle
   is
      X : constant G.Logical_Coordinate := Area.X +
        G.Logical_Coordinate'Max
          (0, G.Logical_Coordinate'Min (Window.X, Area.Width - Window.Width));
      Y : constant G.Logical_Coordinate := Area.Y +
        G.Logical_Coordinate'Max
          (0, G.Logical_Coordinate'Min
             (Window.Y, Area.Height - Window.Height));
   begin
      return (X, Y, X + Window.Width, Y + Window.Height);
   end Fit;

   function Safe_Plan
     (Desired : Preference; Areas : Ready_Areas; Result : Plan_Result)
      return Boolean is
     (Result.Target.Available =
         (Result.Reason in At_Home | Temporary_Fallback) and then
      (if Result.Target.Available then
         Result.Target.Index <= Areas.Count and then
         Result.Target.Display = Areas.Items (Result.Target.Index).Display
         and then Contained
           (Areas.Items (Result.Target.Index), Result.Target.Bounds) and then
         Result.Target.Bounds.Right - Result.Target.Bounds.Left =
           Desired.Bounds.Width and then
         Result.Target.Bounds.Bottom - Result.Target.Bounds.Top =
           Desired.Bounds.Height and then
         (if Result.Reason = At_Home then
            Result.Target.Display = Desired.Home
          else Result.Target.Display /= Desired.Home)));

   function Plan
     (Desired : Preference; Areas : Ready_Areas;
      Previous : Display_Choice := (Known => False);
      Policy : Fallback_Policy := Await_Preferred_Output;
      Interaction : Interaction_State := Idle) return Plan_Result
   is
      Home, Best, Prior : L.Viewport_Count := 0;
      Chosen : L.Viewport_Count;
   begin
      if Interaction /= Idle then
         return (Deferred_Interaction, (Available => False));
      end if;
      --  An ambiguous name is not a license to choose either output.
      for I in 1 .. Areas.Count loop
         for J in 1 .. I - 1 loop
            if Areas.Items (I).Display = Areas.Items (J).Display then
               return (Ambiguous_Display, (Available => False));
            end if;
         end loop;
      end loop;
      for I in 1 .. Areas.Count loop
         if Areas.Items (I).Display = Desired.Home then
            Home := I;
         end if;
         if Areas.Items (I).Display /= Desired.Home and then
           Suitable (Areas.Items (I), Desired.Bounds)
         then
            if Best = 0 or else
              Areas.Items (I).Display < Areas.Items (Best).Display
            then
               Best := I;
            end if;
            if Previous.Known and then
              Areas.Items (I).Display = Previous.Display
            then
               Prior := I;
            end if;
         end if;
         pragma Loop_Invariant
           (Home <= I and then
            (if Home > 0 then Areas.Items (Home).Display = Desired.Home));
         pragma Loop_Invariant
           (Best <= I and then
            (if Best > 0 then Suitable (Areas.Items (Best), Desired.Bounds)
             and then Areas.Items (Best).Display /= Desired.Home));
         pragma Loop_Invariant
           (Prior <= I and then
            (if Prior > 0 then Suitable (Areas.Items (Prior), Desired.Bounds)
             and then Areas.Items (Prior).Display /= Desired.Home));
      end loop;
      if Home > 0 and then Suitable (Areas.Items (Home), Desired.Bounds) then
         return (At_Home,
           (True, Home, Desired.Home,
            Fit (Areas.Items (Home), Desired.Bounds)));
      elsif Home = 0 and then Policy = Await_Preferred_Output then
         return (Waiting_For_Home, (Available => False));
      elsif Areas.Count = 0 then
         return (No_Ready_Output, (Available => False));
      end if;
      Chosen := (if Prior > 0 then Prior else Best);
      if Chosen = 0 then
         return (No_Suitable_Work_Area, (Available => False));
      end if;
      return (Temporary_Fallback,
        (True, Chosen, Areas.Items (Chosen).Display,
         Fit (Areas.Items (Chosen), Desired.Bounds)));
   end Plan;

   function Active (Episode : Recovery_Episode) return Boolean is
     (Episode.Started);
   function Deadline (Episode : Recovery_Episode) return Instant is
     (Episode.Due);
   function Expired (Episode : Recovery_Episode; Now : Instant)
      return Boolean is (Episode.Started and then Now >= Episode.Due);

   procedure Begin_Recovery
     (Episode : in out Recovery_Episode; Now, Grace : Instant) is
   begin
      if not Episode.Started then
         Episode := (Started => True,
           Due => (if Grace > Instant'Last - Now then Instant'Last
                   else Now + Grace));
      end if;
   end Begin_Recovery;

   procedure Complete_Recovery (Episode : out Recovery_Episode) is
   begin
      Episode := (Started => False, Due => 0);
   end Complete_Recovery;
end CuBit.Window_Placement;
