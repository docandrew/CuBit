pragma Ada_2022;

package body CuBit.Display_Planes with SPARK_Mode is
   function Place (R : Request_State; O : Output_State) return Placement is
   begin
      if not Touches (R, O) then
         return (Visible => False);
      end if;
      return (Visible => True,
              X => Local_Coordinate (Left (R) - Integer (O.X)),
              Y => Local_Coordinate (Top (R) - Integer (O.Y)));
   end Place;

   function Free (Outputs : Output_Table; A : Plan; R : Request_State;
                  O : Output_Id; P : Plane_Number) return Boolean is
     (P <= Outputs (O).Count and then A.Holders (O, P) = No_Request and then
      Compatible (R, Outputs (O).Planes (P)));

   --  Lowest-numbered free plane of O that can carry R, or No_Plane.
   function First_Free
     (Outputs : Output_Table; A : Plan; R : Request_State; O : Output_Id)
      return Plane_Count
   with Global => null,
        Post => (if First_Free'Result = No_Plane then
                   (for all P in Plane_Number =>
                      not Free (Outputs, A, R, O, P))
                 else Free (Outputs, A, R, O, First_Free'Result))
   is
   begin
      for P in Plane_Number loop
         if Free (Outputs, A, R, O, P) then
            return P;
         end if;
         pragma Loop_Invariant
           (for all Q in Plane_Number =>
              (if Q <= P then not Free (Outputs, A, R, O, Q)));
      end loop;
      return No_Plane;
   end First_Free;

   --  Can R take a plane on every output it touches?
   function Admissible
     (Requests : Request_Table; Outputs : Output_Table; A : Plan;
      R : Request_Id) return Boolean is
     ((for some O in Output_Id => Touches (Requests (R), Outputs (O)))
      and then
      (for all O in Output_Id =>
         (if Touches (Requests (R), Outputs (O)) then
            (for some P in Plane_Number =>
               Free (Outputs, A, Requests (R), O, P)))));

   function Visited_Before
     (Requests : Request_Table; Prio : Request_Priority; D, R : Request_Id)
      return Boolean is
     (Requests (D).Priority > Prio or else
      (Requests (D).Priority = Prio and then D < R));

   function Visited
     (Requests : Request_Table; Prio : Request_Priority; D, R : Request_Id)
      return Boolean is
     (Requests (D).Priority > Prio or else
      (Requests (D).Priority = Prio and then D <= R));

   --  A composited request already offered is blocked for good.
   function Blocked
     (Requests : Request_Table; Outputs : Output_Table; A : Plan;
      D : Request_Id) return Boolean is
     (A.Backing (D) /= Composited or else
      (for all O in Output_Id => not Touches (Requests (D), Outputs (O)))
      or else
      (for some O in Output_Id =>
         Touches (Requests (D), Outputs (O)) and then
         Taken_Before (Requests, Outputs, A, D, O)));

   function Invariants
     (Requests : Request_Table; Outputs : Output_Table; A : Plan)
      return Boolean is
     (Covered (Requests, A) and then Indexed (A) and then
      Within_Outputs (Outputs, A) and then Complete (Requests, Outputs, A));

   procedure Admit
     (Requests : Request_Table; Outputs : Output_Table; R : Request_Id;
      A : in out Plan)
   with Global => null,
        Pre => A.Backing (R) = Composited and then
               Admissible (Requests, Outputs, A, R) and then
               Invariants (Requests, Outputs, A),
        Post => A.Backing (R) = Hardware and then
                Invariants (Requests, Outputs, A) and then
                (for all D in Request_Id =>
                   (if D /= R then A.Backing (D) = A.Backing'Old (D))) and then
                (for all O in Output_Id =>
                   (for all P in Plane_Number =>
                      (if A.Holders'Old (O, P) /= No_Request then
                         A.Holders (O, P) = A.Holders'Old (O, P))))
   is
      Chosen : Plane_Count;
   begin
      for O in Output_Id loop
         if Touches (Requests (R), Outputs (O)) then
            Chosen := First_Free (Outputs, A, Requests (R), O);
            A.Planes (R, O) := Chosen;
            A.Holders (O, Chosen) := R;
         end if;
         pragma Loop_Invariant (A.Backing = A.Backing'Loop_Entry);
         pragma Loop_Invariant
           (for all D in Request_Id =>
              (if D /= R then
                 (for all Q in Output_Id =>
                    A.Planes (D, Q) = A.Planes'Loop_Entry (D, Q))));
         pragma Loop_Invariant
           (for all Q in Output_Id =>
              (if Q <= O and then Touches (Requests (R), Outputs (Q)) then
                 A.Planes (R, Q) /= No_Plane and then
                 A.Planes (R, Q) <= Outputs (Q).Count and then
                 A.Holders'Loop_Entry (Q, A.Planes (R, Q)) = No_Request and then
                 A.Holders (Q, A.Planes (R, Q)) = R and then
                 Compatible (Requests (R),
                   Outputs (Q).Planes (A.Planes (R, Q))) and then
                 (for all P in Plane_Number =>
                    (if P /= A.Planes (R, Q) then
                       A.Holders (Q, P) = A.Holders'Loop_Entry (Q, P)))
               else
                 A.Planes (R, Q) = No_Plane and then
                 (for all P in Plane_Number =>
                    A.Holders (Q, P) = A.Holders'Loop_Entry (Q, P))));
         pragma Loop_Invariant
           (for all Q in Output_Id =>
              (if Q > O and then Touches (Requests (R), Outputs (Q)) then
                 (for some P in Plane_Number =>
                    Free (Outputs, A, Requests (R), Q, P))));
      end loop;
      A.Backing (R) := Hardware;
   end Admit;

   --  Admitting another request only fills free planes, so a blocking
   --  output stays blocking.
   procedure Lemma_Stable
     (Requests : Request_Table; Outputs : Output_Table; Old_A, A : Plan;
      D : Request_Id)
   with Ghost, Global => null,
        Pre => Blocked (Requests, Outputs, Old_A, D) and then
               A.Backing (D) = Old_A.Backing (D) and then
               (for all O in Output_Id =>
                  (for all P in Plane_Number =>
                     (if Old_A.Holders (O, P) /= No_Request then
                        A.Holders (O, P) = Old_A.Holders (O, P)))),
        Post => Blocked (Requests, Outputs, A, D)
   is
   begin
      if A.Backing (D) /= Composited or else
        (for all O in Output_Id => not Touches (Requests (D), Outputs (O)))
      then
         return;
      end if;
      for O in Output_Id loop
         if Touches (Requests (D), Outputs (O)) and then
           Taken_Before (Requests, Outputs, Old_A, D, O)
         then
            pragma Assert (Taken_Before (Requests, Outputs, A, D, O));
            return;
         end if;
         pragma Loop_Invariant
           (for all Q in Output_Id =>
              (if Q <= O then
                 not (Touches (Requests (D), Outputs (Q)) and then
                      Taken_Before (Requests, Outputs, Old_A, D, Q))));
      end loop;
   end Lemma_Stable;

   --  Offer R its planes; everything offered before stays blocked.
   procedure Offer
     (Requests : Request_Table; Outputs : Output_Table;
      Prio : Request_Priority; R : Request_Id; Result : in out Plan)
   with Global => null,
        Pre => Invariants (Requests, Outputs, Result) and then
               (for all E in Request_Id =>
                  (if Result.Backing (E) = Hardware then
                     Visited_Before (Requests, Prio, E, R))) and then
               (for all D in Request_Id =>
                  (if Visited_Before (Requests, Prio, D, R) then
                     Blocked (Requests, Outputs, Result, D))),
        Post => Invariants (Requests, Outputs, Result) and then
                (for all E in Request_Id =>
                   (if Result.Backing (E) = Hardware then
                      Visited (Requests, Prio, E, R))) and then
                (for all D in Request_Id =>
                   (if Visited (Requests, Prio, D, R) then
                      Blocked (Requests, Outputs, Result, D)))
   is
   begin
      if Result.Backing (R) = Composited and then
        Requests (R).Priority = Prio and then
        Admissible (Requests, Outputs, Result, R)
      then
         declare
            Old_Result : constant Plan := Result with Ghost;
         begin
            Admit (Requests, Outputs, R, Result);
            for D in Request_Id loop
               if Visited_Before (Requests, Prio, D, R) then
                  Lemma_Stable (Requests, Outputs, Old_Result, Result, D);
               end if;
               pragma Loop_Invariant
                 (for all X in Request_Id =>
                    (if X <= D and then Visited_Before (Requests, Prio, X, R)
                     then Blocked (Requests, Outputs, Result, X)));
            end loop;
         end;
      elsif Result.Backing (R) = Composited and then
        Requests (R).Priority = Prio
      then
         --  Not admissible: name the blocking output explicitly.
         for O in Output_Id loop
            if Touches (Requests (R), Outputs (O)) and then
              not (for some P in Plane_Number =>
                     Free (Outputs, Result, Requests (R), O, P))
            then
               for P in Plane_Number loop
                  pragma Assert
                    (not Free (Outputs, Result, Requests (R), O, P));
                  pragma Loop_Invariant
                    (for all Q in Plane_Number =>
                       (if Q <= P and then Q <= Outputs (O).Count and then
                           Compatible (Requests (R), Outputs (O).Planes (Q))
                        then Result.Holders (O, Q) /= No_Request));
               end loop;
               pragma Assert
                 (for all P in Plane_Number =>
                    (if P <= Outputs (O).Count and then
                        Compatible (Requests (R), Outputs (O).Planes (P))
                     then Result.Holders (O, P) /= No_Request));
               pragma Assert
                 (for all P in Plane_Number =>
                    (if Result.Holders (O, P) /= No_Request then
                       Result.Backing (Result.Holders (O, P)) = Hardware and then
                       Visited_Before (Requests, Prio, Result.Holders (O, P), R)));
               pragma Assert (Taken_Before (Requests, Outputs, Result, R, O));
               exit;
            end if;
            pragma Loop_Invariant
              (for all Q in Output_Id =>
                 (if Q <= O and then Touches (Requests (R), Outputs (Q)) then
                    (for some P in Plane_Number =>
                       Free (Outputs, Result, Requests (R), Q, P))));
         end loop;
      end if;
   end Offer;

   --  Indexed holders make every plane number name a single request.
   procedure Lemma_Exclusive (A : Plan)
   with Ghost, Global => null,
        Pre => Indexed (A),
        Post => Exclusive (A)
   is
   begin
      null;
   end Lemma_Exclusive;

   function Plan_Planes (Requests : Request_Table; Outputs : Output_Table)
      return Plan
   is
      Result : Plan;
   begin
      for R in Request_Id loop
         if Shown (Requests (R)) then
            Result.Backing (R) := Composited;
         end if;
         pragma Loop_Invariant
           (for all D in Request_Id =>
              Result.Backing (D) =
                (if D <= R and then Shown (Requests (D)) then Composited
                 else Hidden));
         pragma Loop_Invariant
           (Result.Planes = Result.Planes'Loop_Entry and then
            Result.Holders = Result.Holders'Loop_Entry);
      end loop;
      --  Priority descending, identity ascending: a total, input-only order.
      for Prio in reverse Request_Priority loop
         for R in Request_Id loop
            Offer (Requests, Outputs, Prio, R, Result);
            pragma Loop_Invariant (Invariants (Requests, Outputs, Result));
            pragma Loop_Invariant
              (for all E in Request_Id =>
                 (if Result.Backing (E) = Hardware then
                    Visited (Requests, Prio, E, R)));
            pragma Loop_Invariant
              (for all D in Request_Id =>
                 (if Visited (Requests, Prio, D, R) then
                    Blocked (Requests, Outputs, Result, D)));
         end loop;
         pragma Loop_Invariant (Invariants (Requests, Outputs, Result));
         pragma Loop_Invariant
           (for all E in Request_Id =>
              (if Result.Backing (E) = Hardware then
                 Requests (E).Priority >= Prio));
         pragma Loop_Invariant
           (for all D in Request_Id =>
              (if Requests (D).Priority >= Prio then
                 Blocked (Requests, Outputs, Result, D)));
      end loop;
      Lemma_Exclusive (Result);
      return Result;
   end Plan_Planes;
end CuBit.Display_Planes;
