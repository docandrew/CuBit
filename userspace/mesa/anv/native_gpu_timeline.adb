pragma Ada_2022;
package body Native_GPU_Timeline with SPARK_Mode is

   procedure Initialize (T : out Timeline; Initial : Sync_Value) is
   begin
      T := (Value => Initial, Count => 0, Points => [others => (others => <>)]);
   end Initialize;

   --  Drop the first K points (K <= Count), keeping the rest in order.
   procedure Drop_First (T : in out Timeline; K : Point_Count)
     with Pre  => K <= T.Count,
          Post => T.Value = T'Old.Value and then T.Count = T'Old.Count - K and then
                  (for all I in 1 .. T.Count => T.Points (I) = T'Old.Points (I + K));
   procedure Drop_First (T : in out Timeline; K : Point_Count) is
      Old : constant Timeline := T;
   begin
      if K = 0 then
         return;
      end if;
      for I in 1 .. Old.Count - K loop
         T.Points (I) := Old.Points (I + K);
         pragma Loop_Invariant (T.Value = Old.Value and then T.Count = Old.Count);
         pragma Loop_Invariant (for all J in 1 .. I => T.Points (J) = Old.Points (J + K));
      end loop;
      T.Count := Old.Count - K;
   end Drop_First;

   procedure Signal (T : in out Timeline; V : Sync_Value; OK : out Boolean) is
      Covered : Point_Count := 0;
   begin
      OK := V >= T.Value;
      if not OK then
         return;
      end if;
      --  Points are increasing: the covered ones are a prefix.
      while Covered < T.Count and then T.Points (Covered + 1).Value <= V loop
         pragma Loop_Invariant (Covered < T.Count);
         pragma Loop_Invariant (for all I in 1 .. Covered + 1 => T.Points (I).Value <= V);
         pragma Loop_Variant (Increases => Covered);
         Covered := Covered + 1;
      end loop;
      pragma Assert (Covered = T.Count or else T.Points (Covered + 1).Value > V);
      pragma Assert (for all I in Covered + 1 .. T.Count => T.Points (I).Value > V);
      Drop_First (T, Covered);
      T.Value := V;
      pragma Assert (for all I in 1 .. T.Count => T.Points (I).Value > V);
   end Signal;

   procedure Add_Point
     (T : in out Timeline; V : Sync_Value; Context : GQ.Context_Index; GPU : GPU_Value;
      Result : out Add_Result) is
   begin
      if V <= Last_Value (T) then
         Result := Not_Increasing;
      elsif T.Count = Max_Points then
         Result := Full;
      else
         T.Count := T.Count + 1;
         T.Points (T.Count) := (Value => V, GPU => GPU, Context => Context);
         Result := Added;
      end if;
   end Add_Point;

   procedure Resolve (T : in out Timeline; Completed : Completed_Values) is
      Old : constant Timeline := T;
      Highest : Point_Count := 0;   --  the last reached point, 0 for none
   begin
      for I in reverse 1 .. T.Count loop
         if Point_Reached (T.Points (I), Completed) then
            Highest := I;
            exit;
         end if;
         pragma Loop_Invariant
           (for all J in I .. T.Count => not Point_Reached (T.Points (J), Completed));
      end loop;
      pragma Assert (for all J in Highest + 1 .. T.Count =>
                       not Point_Reached (T.Points (J), Completed));
      if Highest = 0 then
         return;
      end if;
      T.Value := Old.Points (Highest).Value;
      Drop_First (T, Highest);
      pragma Assert (for all I in 1 .. T.Count => T.Points (I) = Old.Points (I + Highest));
   end Resolve;

   procedure Find (T : Timeline; W : Sync_Value; Kind : out Wait_Kind; P : out Point) is
   begin
      P := (others => <>);
      if W <= T.Value then
         Kind := Reached;
         return;
      elsif W > Last_Value (T) then
         Kind := Unsubmitted;
         return;
      end if;
      for I in 1 .. T.Count loop
         if T.Points (I).Value >= W then
            P := T.Points (I);
            Kind := On_GPU;
            return;
         end if;
         pragma Loop_Invariant (for all J in 1 .. I => T.Points (J).Value < W);
      end loop;
      --  Unreachable: the last point is at or above W.
      pragma Assert (False);
      Kind := Unsubmitted;
   end Find;

   procedure Move
     (Target, Source : in out Timeline; Source_Next : Sync_Value;
      Target_Next, New_Source_Next : out Sync_Value; OK : out Boolean)
   is
      Last : constant Sync_Value := Last_Value (Source);
   begin
      Target_Next := 0;
      New_Source_Next := 0;
      OK := Source_Next <= Last and then Last < Sync_Value'Last;
      if not OK then
         return;
      end if;
      Target := Source;
      Target_Next := Source_Next;
      Source.Count := 0;
      New_Source_Next := Last + 1;
   end Move;

   procedure Clear (S : out Wait_Set) is
   begin
      S := [others => GQ.No_Wait];
   end Clear;

   procedure Merge (S : in out Wait_Set; Job : GQ.Context_Index; P : Point) is
   begin
      if P.Context /= Job and then P.GPU > S (P.Context) then
         S (P.Context) := P.GPU;
      end if;
   end Merge;

   procedure Select_Waits (S : Wait_Set; First, Second : out Wait; Fits : out Boolean) is
      Found : Natural := 0;
   begin
      First := (others => <>);
      Second := (others => <>);
      Fits := True;
      for C in GQ.Context_Index loop
         if S (C) /= GQ.No_Wait then
            if Found = 0 then
               First := (Context => C, Target => S (C));
               Found := 1;
            elsif Found = 1 then
               Second := (Context => C, Target => S (C));
               Found := 2;
            else
               Fits := False;
            end if;
         end if;
         pragma Loop_Invariant (Found <= 2);
         pragma Loop_Invariant (if Found = 0 then First.Target = GQ.No_Wait);
         pragma Loop_Invariant (if Found <= 1 then Second.Target = GQ.No_Wait);
         pragma Loop_Invariant (First.Target = GQ.No_Wait or else
                                (First.Context <= C and then S (First.Context) = First.Target));
         pragma Loop_Invariant (Second.Target = GQ.No_Wait or else
                                (Second.Context <= C and then S (Second.Context) = Second.Target));
         pragma Loop_Invariant
           (if Fits then
              (for all D in GQ.Context_Index'First .. C =>
                 (if S (D) /= GQ.No_Wait then
                    (First.Context = D and then First.Target = S (D)) or else
                    (Second.Context = D and then Second.Target = S (D)))));
      end loop;
   end Select_Waits;

   ------------------------------------------------------------------------
   --  C interface
   ------------------------------------------------------------------------
   procedure C_Initialize (T : out Timeline; Initial : Sync_Value) is
   begin
      Initialize (T, Initial);
   end C_Initialize;

   procedure C_Signal (T : in out Timeline; V : Sync_Value; OK : out Unsigned_32) is
      Done : Boolean;
   begin
      Signal (T, V, Done);
      OK := (if Done then 1 else 0);
   end C_Signal;

   procedure C_Add
     (T : in out Timeline; V : Sync_Value; Context : Unsigned_32; GPU : GPU_Value;
      Result : out Add_Result) is
   begin
      if Context > Unsigned_32 (GQ.Context_Index'Last) then
         Result := Bad_Context;
         return;
      end if;
      Add_Point (T, V, GQ.Context_Index (Context), GPU, Result);
   end C_Add;

   procedure C_Resolve (T : in out Timeline; Completed : Completed_Values) is
   begin
      Resolve (T, Completed);
   end C_Resolve;

   procedure C_Value (T : Timeline; Value, Last : out Sync_Value) is
   begin
      Value := T.Value;
      Last := Last_Value (T);
   end C_Value;

   procedure C_Find
     (T : Timeline; W : Sync_Value; Kind : out Wait_Kind; Context : out Unsigned_32;
      GPU : out GPU_Value)
   is
      P : Point;
   begin
      Find (T, W, Kind, P);
      Context := Unsigned_32 (P.Context);
      GPU := P.GPU;
   end C_Find;

   procedure C_Move
     (Target, Source : in out Timeline; Source_Next : Sync_Value;
      Target_Next, New_Source_Next : out Sync_Value; OK : out Unsigned_32)
   is
      Done : Boolean;
   begin
      Move (Target, Source, Source_Next, Target_Next, New_Source_Next, Done);
      OK := (if Done then 1 else 0);
   end C_Move;

   procedure C_Clear (S : out Wait_Set) is
   begin
      Clear (S);
   end C_Clear;

   procedure C_Merge
     (S : in out Wait_Set; Job, Context : Unsigned_32; GPU : GPU_Value; OK : out Unsigned_32) is
   begin
      if Job > Unsigned_32 (GQ.Context_Index'Last) or else
        Context > Unsigned_32 (GQ.Context_Index'Last)
      then
         OK := 0;
         return;
      end if;
      Merge (S, GQ.Context_Index (Job),
             (Value => 0, GPU => GPU, Context => GQ.Context_Index (Context)));
      OK := 1;
   end C_Merge;

   procedure C_Select
     (S : Wait_Set; First_Context : out Unsigned_32; First_Target : out GPU_Value;
      Second_Context : out Unsigned_32; Second_Target : out GPU_Value; Fits : out Unsigned_32)
   is
      First, Second : Wait;
      Done : Boolean;
   begin
      Select_Waits (S, First, Second, Done);
      First_Context := Unsigned_32 (First.Context);
      First_Target := First.Target;
      Second_Context := Unsigned_32 (Second.Context);
      Second_Target := Second.Target;
      Fits := (if Done then 1 else 0);
   end C_Select;

end Native_GPU_Timeline;
