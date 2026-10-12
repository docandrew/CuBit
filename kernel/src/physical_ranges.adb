pragma Ada_2022;

package body Physical_Ranges with SPARK_Mode is

   procedure Add (M : in out Map; First, Last : Unsigned_64; Class : Range_Class) is
   begin
      if First > Last or else First >= Address_Limit or else not M.Complete then
         return;
      elsif M.Count = Max_Spans then
         M.Complete := False;
         return;
      end if;
      M.Count := M.Count + 1;
      M.Spans (M.Count) :=
        (First => First,
         Last  => Unsigned_64'Min (Last, Address_Limit - 1),
         Class => Class);
   end Add;

   procedure Invalidate (M : in out Map) is
   begin
      M.Complete := False;
   end Invalidate;

   procedure Seal (M : in out Map) is
   begin
      M.Sealed := True;
   end Seal;

   function Class_Of (M : Map; P : Page_Number) return Range_Class is
      Result : Range_Class := Unreported;
   begin
      for I in 1 .. M.Count loop
         if Touches (M.Spans (I), P) and then M.Spans (I).Class < Result then
            Result := M.Spans (I).Class;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              (if Touches (M.Spans (J), P) then Result <= M.Spans (J).Class));
         pragma Loop_Invariant
           (Result = Unreported
            or else (for some J in 1 .. I =>
                       Touches (M.Spans (J), P)
                       and then M.Spans (J).Class = Result));
      end loop;
      return Result;
   end Class_Of;

   procedure Lemma_Device_Range_Excludes_RAM (M : Map; First, Last : Address) is
   begin
      for P in Page_Of (First) .. Page_Of (Last) loop
         -- Any span that touches P touches the requested pages, so the
         -- Blocker_Free clause for it can only hold by its class.
         pragma Assert
           (for all I in 1 .. M.Count =>
              (if Touches (M.Spans (I), P)
               then Device_Mappable (M.Spans (I).Class)));
         pragma Assert (Device_Mappable (Class_Of (M, P)));
         pragma Loop_Invariant
           (for all Q in Page_Of (First) .. P =>
              Device_Mappable (Class_Of (M, Q)));
      end loop;
   end Lemma_Device_Range_Excludes_RAM;

   procedure First_Blocked_Page
     (M : Map; First, Last : Address; Found : out Boolean; Page : out Page_Number)
   is
      Low  : constant Page_Number := Page_Of (First);
      High : constant Page_Number := Page_Of (Last);
      Hit  : Span_Count := 0;
   begin
      Found := False;
      Page := Low;
      for I in 1 .. M.Count loop
         if not Device_Mappable (M.Spans (I).Class)
           and then Page_Of (M.Spans (I).Last) >= Low
           and then Page_Of (M.Spans (I).First) <= High
           and then (not Found
                     or else Page_Number'Max (Low, Page_Of (M.Spans (I).First)) < Page)
         then
            Found := True;
            Hit := I;
            Page := Page_Number'Max (Low, Page_Of (M.Spans (I).First));
         end if;
         pragma Loop_Invariant
           (if Found then
              Hit in 1 .. I and then Page in Low .. High
              and then Touches (M.Spans (Hit), Page)
              and then not Device_Mappable (M.Spans (Hit).Class)
            else
              (for all J in 1 .. I =>
                 Device_Mappable (M.Spans (J).Class)
                 or else Page_Of (M.Spans (J).Last) < Low
                 or else Page_Of (M.Spans (J).First) > High));
      end loop;
      if Found then
         -- Class_Of is at most the class of the span that touches Page.
         pragma Assert (Class_Of (M, Page) <= M.Spans (Hit).Class);
      end if;
   end First_Blocked_Page;

   -- A non-mappable span touching page P, if any.
   function Blocker_At (M : Map; P : Page_Number) return Span_Count
     with Pre  => Well_Formed (M),
          Post => (if Blocker_At'Result /= 0 then
                     Blocker_At'Result <= M.Count
                     and then Touches (M.Spans (Blocker_At'Result), P)
                     and then not Device_Mappable
                                (M.Spans (Blocker_At'Result).Class))
   is
   begin
      for I in 1 .. M.Count loop
         if Touches (M.Spans (I), P)
           and then not Device_Mappable (M.Spans (I).Class)
         then
            return I;
         end if;
      end loop;
      return 0;
   end Blocker_At;

   procedure Next_Device_Run
     (M : Map; From : Page_Number;
      Found : out Boolean; First, Last : out Address)
   is
      Cursor  : Page_Number := From;
      End_Page : Page_Number;
      Hit     : Span_Count;
      Settled : Boolean := False;
   begin
      Found := False;
      First := 0;
      Last := 0;
      -- Each blocker can move the cursor past itself at most once, so
      -- Count + 1 rounds settle on a free page or run off the end.
      for Round in 0 .. M.Count loop
         Hit := Blocker_At (M, Cursor);
         if Hit = 0 then
            Settled := True;
            exit;
         end if;
         if Page_Of (M.Spans (Hit).Last) = Last_Page then
            return;
         end if;
         Cursor := Page_Of (M.Spans (Hit).Last) + 1;
         pragma Loop_Invariant (Cursor >= From);
      end loop;
      if not Settled then
         return;
      end if;

      -- The run ends just before the nearest blocker that starts above it.
      End_Page := Last_Page;
      for I in 1 .. M.Count loop
         if not Device_Mappable (M.Spans (I).Class)
           and then Page_Of (M.Spans (I).First) > Cursor
           and then Page_Of (M.Spans (I).First) - 1 < End_Page
         then
            End_Page := Page_Of (M.Spans (I).First) - 1;
         end if;
         pragma Loop_Invariant (End_Page >= Cursor);
      end loop;

      First := Cursor * Page_Bytes;
      Last := End_Page * Page_Bytes + (Page_Bytes - 1);
      -- The search is not trusted: only a verified run is reported.
      Found := Device_Range (M, First, Last);
   end Next_Device_Run;

   procedure Largest_Device_Runs
     (M : Map; Runs : out Run_Array; Count : out Root_Count)
   is
      From  : Page_Number := 0;
      Found : Boolean;
      First, Last : Address;
      Slot  : Root_Index;
      Held  : Run;

      function Size (R : Run) return Unsigned_64 is
        (if R.Last >= R.First then R.Last - R.First else 0);
   begin
      Runs := [others => (First => 0, Last => 0)];
      Count := 0;
      -- Runs are disjoint and each ends at least one page before the next
      -- begins, so Count + 1 searches enumerate them all.
      for Round in 0 .. M.Count loop
         Next_Device_Run (M, From, Found, First, Last);
         exit when not Found;
         if Count < Max_Roots then
            Count := Count + 1;
            Runs (Count) := (First => First, Last => Last);
         else
            -- Replace the smallest kept run if this one is larger.
            Slot := 1;
            for I in Root_Index loop
               if Size (Runs (I)) < Size (Runs (Slot)) then
                  Slot := I;
               end if;
            end loop;
            if Size (Runs (Slot)) < Last - First then
               Runs (Slot) := (First => First, Last => Last);
            end if;
         end if;
         pragma Loop_Invariant
           (for all I in 1 .. Count =>
              Device_Range (M, Runs (I).First, Runs (I).Last));
         exit when Page_Of (Last) = Last_Page;
         From := Page_Of (Last) + 1;
      end loop;
      -- Largest first (insertion sort over at most Max_Roots runs).
      for I in 2 .. Count loop
         for J in reverse 2 .. I loop
            exit when Size (Runs (J - 1)) >= Size (Runs (J));
            Held := Runs (J);
            Runs (J) := Runs (J - 1);
            Runs (J - 1) := Held;
            pragma Loop_Invariant
              (for all K in 1 .. Count =>
                 Device_Range (M, Runs (K).First, Runs (K).Last));
         end loop;
         pragma Loop_Invariant
           (for all K in 1 .. Count =>
              Device_Range (M, Runs (K).First, Runs (K).Last));
      end loop;
   end Largest_Device_Runs;

end Physical_Ranges;
