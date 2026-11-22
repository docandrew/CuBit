with Interfaces; use Interfaces;
package body Compositor_Damage with SPARK_Mode is
   procedure Clear (S : out State) is
   begin
      S := (others => <>);
   end Clear;
   procedure Capture (Pending, Frame : in out State; Accepted : out Boolean) is
   begin
      Accepted := Pending.Used > 0 and Frame.Used = 0;
      if not Accepted then return; end if;
      Frame := Pending;
      Clear (Pending);
   end Capture;
   --  Each of the frame's boxes goes back as itself: restoring a frame of
   --  an animating window and a caret must not merge them into their
   --  envelope (LAT-002: one damage rect of ~950k px per typing frame).
   --  Past Capacity, Add joins a box to its nearest neighbour (LAT-003).
   procedure Restore (Pending, Frame : in out State) is
      Old_Pending : constant State := Pending with Ghost;
   begin
      for I in 1 .. Frame.Used loop
         Add (Pending, Frame.Region (I));
         pragma Loop_Invariant (Valid (Pending) and Valid (Frame) and Frame = Frame'Loop_Entry);
         pragma Loop_Invariant
           (for all J in 1 .. Count (Old_Pending) => Covers (Pending, Item (Old_Pending, J)));
         pragma Loop_Invariant
           (for all J in 1 .. I => Covers (Pending, Frame.Region (J)));
      end loop;
      Clear (Frame);
   end Restore;
   --  LAT-003: R joins only the boxes it overlaps (and, transitively, what
   --  their union then overlaps); distant boxes stay apart. Past Capacity,
   --  the new box joins the existing box whose envelope with it is
   --  smallest, never the envelope of everything: a clock in one corner and
   --  a log line in the other used to become half the screen.
   function Area (B : Box) return Unsigned_64 is
     (Unsigned_64 (B.Right - B.Left) * Unsigned_64 (B.Bottom - B.Top))
     with Pre => Valid (B);

   --  Every box of Old is in S or inside Cur.
   function Kept_Or_Absorbed (Old, S : State; Cur : Box) return Boolean is
     (for all K in 1 .. Old.Used =>
        Contains (Cur, Old.Region (K)) or else Covers (S, Old.Region (K)))
     with Ghost;

   --  Drop every box Cur overlaps, growing Cur to their envelope, until no
   --  box left overlaps Cur.
   procedure Absorb (S : in out State; Cur : in out Box; Limit : Box; Old : State)
     with Pre => S.Used <= Capacity and Valid (Cur) and Contains (Limit, Cur) and
         (for all I in 1 .. S.Used => Valid (S.Region (I)) and Contains (Limit, S.Region (I))) and
         (for all I in 1 .. S.Used =>
            (for all J in 1 .. S.Used =>
               (if I /= J then not Overlaps (S.Region (I), S.Region (J))))) and
         Kept_Or_Absorbed (Old, S, Cur),
       Post => Valid (Cur) and Contains (Limit, Cur) and Contains (Cur, Cur'Old) and
         S.Used <= S.Used'Old and
         (for all I in 1 .. S.Used => Valid (S.Region (I)) and Contains (Limit, S.Region (I)) and
            not Overlaps (S.Region (I), Cur)) and
         (for all I in 1 .. S.Used =>
            (for all J in 1 .. S.Used =>
               (if I /= J then not Overlaps (S.Region (I), S.Region (J))))) and
         Kept_Or_Absorbed (Old, S, Cur);

   procedure Absorb (S : in out State; Cur : in out Box; Limit : Box; Old : State) is
      type Origins is array (Index) of Index;
      Changed : Boolean := True;
      Kept : Boxes;
      From : Origins with Ghost;
      Used : Length;
      Cur_Entry : constant Box := Cur with Ghost;
   begin
      loop
         pragma Loop_Invariant
           (S.Used <= Capacity and Valid (Cur) and Contains (Limit, Cur) and Contains (Cur, Cur_Entry) and
            S.Used <= S.Used'Loop_Entry and
            (for all I in 1 .. S.Used => Valid (S.Region (I)) and Contains (Limit, S.Region (I))) and
            (for all I in 1 .. S.Used =>
               (for all J in 1 .. S.Used =>
                  (if I /= J then not Overlaps (S.Region (I), S.Region (J))))) and
            Kept_Or_Absorbed (Old, S, Cur) and
            (if not Changed then (for all I in 1 .. S.Used => not Overlaps (S.Region (I), Cur))));
         pragma Loop_Variant (Decreases => 2 * S.Used + Boolean'Pos (Changed));
         exit when not Changed;
         Changed := False;
         Used := 0;
         Kept := (others => (others => 0));
         From := (others => 1);
         declare
            Prior : constant State := S;
            Before : constant Box := Cur with Ghost;
         begin
            for I in 1 .. S.Used loop
               if Overlaps (S.Region (I), Cur) then
                  Cur := Envelope (Cur, S.Region (I));
                  Changed := True;
               else
                  Used := Used + 1;
                  Kept (Used) := S.Region (I);
                  From (Used) := I;
               end if;
               pragma Loop_Invariant (Used <= I and (if Changed then Used < I else Used = I));
               pragma Loop_Invariant
                 (Valid (Cur) and Contains (Limit, Cur) and Contains (Cur, Before));
               pragma Loop_Invariant
                 (for all K in 1 .. Used => From (K) <= I and Kept (K) = S.Region (From (K)));
               pragma Loop_Invariant
                 (for all K in 1 .. Used =>
                    (for all L in 1 .. Used => (if K < L then From (K) < From (L))));
               pragma Loop_Invariant
                 (for all J in 1 .. I =>
                    Contains (Cur, S.Region (J)) or else
                    (for some K in 1 .. Used => From (K) = J));
               pragma Loop_Invariant
                 (if not Changed then
                    (for all K in 1 .. Used => From (K) = K and not Overlaps (Kept (K), Cur)));
            end loop;
            S.Region := Kept;
            S.Used := Used;
            pragma Assert
              (for all K in 1 .. S.Used =>
                 (for all L in 1 .. S.Used =>
                    (if K /= L then not Overlaps (S.Region (K), S.Region (L)))));
            pragma Assert
              (for all K in 1 .. Old.Used =>
                 Contains (Cur, Old.Region (K)) or else
                 (for some J in 1 .. Prior.Used => Contains (Prior.Region (J), Old.Region (K))));
         end;
      end loop;
   end Absorb;

   procedure Add (S : in out State; R : Box) is
      --  Not ghost: Absorb takes it (eight boxes).
      Old : constant State := S;
      Limit : constant Box := (if S.Used = 0 then R else Envelope (S.Extent, R));
      Cur : Box := R;
      Best : Index;
      Best_Cost, Cost : Unsigned_64;
   begin
      for I in 1 .. S.Used loop
         if Contains (S.Region (I), R) then return; end if;
         pragma Loop_Invariant (for all J in 1 .. I => not Contains (S.Region (J), R));
      end loop;
      Absorb (S, Cur, Limit, Old);
      while S.Used = Capacity loop
         pragma Loop_Invariant
           (S.Used <= Capacity and Valid (Cur) and Contains (Limit, Cur) and Contains (Cur, R) and
            (for all I in 1 .. S.Used => Valid (S.Region (I)) and Contains (Limit, S.Region (I)) and
               not Overlaps (S.Region (I), Cur)) and
            (for all I in 1 .. S.Used =>
               (for all J in 1 .. S.Used =>
                  (if I /= J then not Overlaps (S.Region (I), S.Region (J))))) and
            Kept_Or_Absorbed (Old, S, Cur));
         pragma Loop_Variant (Decreases => S.Used);
         --  Full: join the box that grows least with Cur, then absorb what
         --  that union overlaps.
         Best := 1;
         Best_Cost := Unsigned_64'Last;
         for I in 1 .. S.Used loop
            Cost := Area (Envelope (Cur, S.Region (I))) - Area (S.Region (I));
            if Cost < Best_Cost then Best := I; Best_Cost := Cost; end if;
            pragma Loop_Invariant (Best <= S.Used);
         end loop;
         declare
            Prior : constant State := S with Ghost;
            Prior_Cur : constant Box := Cur with Ghost;
         begin
            Cur := Envelope (Cur, S.Region (Best));
            S.Region (Best) := S.Region (S.Used);
            S.Used := S.Used - 1;
            pragma Assert (Contains (Cur, Prior.Region (Best)) and Contains (Cur, Prior_Cur));
            pragma Assert
              (for all J in 1 .. Prior.Used =>
                 Contains (Cur, Prior.Region (J)) or else
                 (for some I in 1 .. S.Used => S.Region (I) = Prior.Region (J)));
            pragma Assert (Kept_Or_Absorbed (Old, S, Cur));
         end;
         Absorb (S, Cur, Limit, Old);
      end loop;
      pragma Assert (for all I in 1 .. S.Used => not Overlaps (Cur, S.Region (I)));
      declare
         Before : constant State := S with Ghost;
      begin
         pragma Assert
           (for all I in 1 .. Before.Used =>
              (for all J in 1 .. Before.Used =>
                 (if I /= J then not Overlaps (Before.Region (I), Before.Region (J)))));
         S.Used := S.Used + 1;
         S.Region (S.Used) := Cur;
         S.Extent := Limit;
         pragma Assert (for all I in 1 .. Before.Used => S.Region (I) = Before.Region (I));
         pragma Assert
           (for all K in 1 .. Old.Used =>
              (if Contains (Cur, Old.Region (K)) then Contains (S.Region (S.Used), Old.Region (K))
               else Covers (Before, Old.Region (K))));
         pragma Assert (for all K in 1 .. Old.Used => Covers (S, Old.Region (K)));
         pragma Assert
           (for all I in 1 .. S.Used =>
              (for all J in 1 .. S.Used =>
                 (if I /= J then
                    (if I = S.Used then not Overlaps (Cur, Before.Region (J))
                     elsif J = S.Used then not Overlaps (Before.Region (I), Cur)
                     else not Overlaps (Before.Region (I), Before.Region (J))))));
         pragma Assert (for all I in 1 .. S.Used =>
                          S.Region (I) = (if I = S.Used then Cur else Before.Region (I)));
         pragma Assert
           (for all I in 1 .. S.Used =>
              (for all J in 1 .. S.Used =>
                 (if I /= J then not Overlaps (S.Region (I), S.Region (J)))));
      end;
   end Add;
end Compositor_Damage;
