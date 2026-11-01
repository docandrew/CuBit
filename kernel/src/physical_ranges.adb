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

   procedure Set_Control (M : in out Map; Physical : Unsigned_64) is
   begin
      if Physical < Address_Limit then
         M.Control_Known := True;
         M.Control_Page := Physical / Page_Bytes;
      else
         M.Control_Known := False;
         M.Control_Page := 0;
      end if;
   end Set_Control;

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

   procedure Lemma_Firmware_Range_Excludes_RAM
     (M : Map; First, Last : Address; Kind : Firmware_Kind) is
   begin
      for P in Page_Of (First) .. Page_Of (Last) loop
         pragma Assert (Firmware_Page (M, P, Kind));
         -- Every span touching P has a control class, and one of them
         -- (wholly containing P) touches it, so Class_Of is that class or
         -- another touching span's.
         case Kind is
            when NVS_Memory =>
               pragma Assert (NVS_Page (M, P));
            when Control_Memory =>
               pragma Assert (Control_Page_Admitted (M, P));
         end case;
         pragma Assert
           (for all I in 1 .. M.Count =>
              (if Touches (M.Spans (I), P) then Control_Class (M.Spans (I).Class)));
         pragma Assert
           (for some I in 1 .. M.Count =>
              Control_Class (M.Spans (I).Class) and then Within (M.Spans (I), P)
              and then Touches (M.Spans (I), P));
         pragma Assert (Class_Of (M, P) /= Unreported);
         pragma Assert (Control_Class (Class_Of (M, P)));
         pragma Loop_Invariant
           (for all Q in Page_Of (First) .. P => Control_Class (Class_Of (M, Q)));
      end loop;
   end Lemma_Firmware_Range_Excludes_RAM;

   -- The whole pages inside span I when it is ACPI NVS: [Low, High], or
   -- Usable False.
   procedure NVS_Candidate
     (M : Map; I : Span_Index; Usable : out Boolean; Low, High : out Page_Number)
     with Pre => Well_Formed (M) and then I <= M.Count
   is
      S : constant Span := M.Spans (I);
      -- One past the last whole page: Last + 1 is at most Address_Limit.
      Ends : constant Unsigned_64 := (S.Last + 1) / Page_Bytes;
   begin
      Usable := False;
      Low := 0;
      High := 0;
      if S.Class /= ACPI_NVS or else S.First > Address_Limit - Page_Bytes then
         return;
      end if;
      Low := (S.First + (Page_Bytes - 1)) / Page_Bytes;
      if Low >= Ends then
         return;
      end if;
      High := Ends - 1;
      Usable := True;
   end NVS_Candidate;

   -- Page P is wholly inside some NVS span and no span of another class
   -- touches it (NVS_Page, computed span by span).
   function Clean_NVS (M : Map; P : Page_Number) return Boolean
     with Pre => Well_Formed (M)
   is
      Usable : Boolean;
      Low, High : Page_Number;
      Held : Boolean := False;
   begin
      for I in 1 .. M.Count loop
         if M.Spans (I).Class /= ACPI_NVS and then Touches (M.Spans (I), P) then
            return False;
         end if;
         NVS_Candidate (M, I, Usable, Low, High);
         if Usable and then P in Low .. High then
            Held := True;
         end if;
      end loop;
      return Held;
   end Clean_NVS;

   -- The lowest run of NVS pages at or after page From: it starts at the
   -- lowest clean NVS page (a candidate's first page, or the page after a
   -- span of another class), runs through adjacent and overlapping NVS
   -- candidates, and stops before the next span of another class. Not
   -- trusted: Firmware_Runs re-checks what it finds.
   procedure Next_NVS_Run
     (M : Map; From : Page_Number; Found : out Boolean; Low, High : out Page_Number)
     with Pre  => Well_Formed (M),
          Post => (if Found then From <= Low and then Low <= High)
   is
      Usable : Boolean;
      C_Low, C_High : Page_Number;
      Stop : Page_Number := Last_Page;
      Grew : Boolean;

      procedure Consider (X : Page_Number)
        with Pre  => Well_Formed (M),
             Post => (if Found then (Found'Old and then Low = Low'Old) or else From <= Low)
      is
      begin
         if X >= From and then (not Found or else X < Low) and then Clean_NVS (M, X) then
            Found := True;
            Low := X;
         end if;
      end Consider;
   begin
      Found := False;
      Low := 0;
      High := 0;
      for I in 1 .. M.Count loop
         NVS_Candidate (M, I, Usable, C_Low, C_High);
         if Usable and then C_High >= From then
            Consider (Page_Number'Max (C_Low, From));
         elsif M.Spans (I).Class /= ACPI_NVS and then Page_Of (M.Spans (I).Last) < Last_Page then
            Consider (Page_Of (M.Spans (I).Last) + 1);
         end if;
         pragma Loop_Invariant (if Found then From <= Low);
      end loop;
      if not Found then
         return;
      end if;
      -- Through the candidates holding Low and those adjacent to or
      -- overlapping the run so far; each round grows past another or
      -- settles.
      High := Low;
      for Round in 0 .. M.Count loop
         Grew := False;
         for I in 1 .. M.Count loop
            NVS_Candidate (M, I, Usable, C_Low, C_High);
            if Usable and then C_High > High
              and then C_Low <= High + 1
            then
               High := C_High;
               Grew := True;
            end if;
            pragma Loop_Invariant (Found and then From <= Low and then Low <= High);
         end loop;
         pragma Loop_Invariant (Found and then From <= Low and then Low <= High);
         exit when not Grew;
      end loop;
      -- Stop before the first span of another class above Low (Low is
      -- clean, so every such span touching the run starts above it).
      for I in 1 .. M.Count loop
         if M.Spans (I).Class /= ACPI_NVS
           and then Page_Of (M.Spans (I).First) > Low
           and then Page_Of (M.Spans (I).First) - 1 < Stop
         then
            Stop := Page_Of (M.Spans (I).First) - 1;
         end if;
         pragma Loop_Invariant (Low <= Stop);
      end loop;
      High := Page_Number'Min (High, Stop);
   end Next_NVS_Run;

   procedure Firmware_Runs
     (M : Map; Runs : out Firmware_Run_Array; Count : out Firmware_Root_Count;
      Left_Out : out Natural)
   is
      From : Page_Number := 0;
      Found : Boolean;
      Low, High : Page_Number;
      Control_Pending : Boolean;

      -- Records pages [First_Page, Last_Page_Of] as the next run if it is
      -- verified firmware memory above every recorded run, short enough,
      -- and there is room; otherwise counts it left out.
      procedure Emit (First_Page, Last_Page_Of : Page_Number; Kind : Firmware_Kind)
        with Pre  => First_Page <= Last_Page_Of
                     and then (for all I in 1 .. Count =>
                                 Firmware_Range (M, Runs (I).First, Runs (I).Last, Runs (I).Kind))
                     and then (for all I in 2 .. Count => Runs (I - 1).Last < Runs (I).First)
                     and then (for all I in Count + 1 .. Max_Firmware_Roots =>
                                 Runs (I) = No_Firmware_Run),
             Post => (for all I in 1 .. Count =>
                        Firmware_Range (M, Runs (I).First, Runs (I).Last, Runs (I).Kind))
                     and then (for all I in 2 .. Count => Runs (I - 1).Last < Runs (I).First)
                     and then (for all I in Count + 1 .. Max_Firmware_Roots =>
                                 Runs (I) = No_Firmware_Run)
      is
         First : constant Address := First_Page * Page_Bytes;
         Last  : constant Address := Last_Page_Of * Page_Bytes + (Page_Bytes - 1);
      begin
         if Count < Max_Firmware_Roots
           and then Last_Page_Of - First_Page < Max_Firmware_Run_Pages
           and then (Count = 0 or else Runs (Count).Last < First)
           and then Firmware_Range (M, First, Last, Kind)
         then
            Count := Count + 1;
            Runs (Count) := (First => First, Last => Last, Kind => Kind);
         elsif Left_Out < Natural'Last then
            Left_Out := Left_Out + 1;
         end if;
      end Emit;
   begin
      Runs := [others => No_Firmware_Run];
      Count := 0;
      Left_Out := 0;
      if not (M.Sealed and then M.Complete) then
         return;
      end if;
      Control_Pending := Control_Page_Admitted (M, M.Control_Page);
      -- Each round records the control page or consumes at least one NVS
      -- candidate, so Count + 2 rounds enumerate them all.
      for Round in 0 .. M.Count + 1 loop
         Next_NVS_Run (M, From, Found, Low, High);
         if Control_Pending and then M.Control_Page >= From
           and then (not Found or else M.Control_Page < Low)
         then
            Control_Pending := False;
            Emit (M.Control_Page, M.Control_Page, Control_Memory);
            if M.Control_Page < Last_Page then
               From := M.Control_Page + 1;
            else
               exit;
            end if;
         elsif Found then
            declare
               Before : constant Firmware_Root_Count := Count;
            begin
               Emit (Low, High, NVS_Memory);
               if Control_Pending and then M.Control_Page in Low .. High then
                  Control_Pending := False;
                  -- A run left out still leaves the control page its own.
                  if Count = Before then
                     Emit (M.Control_Page, M.Control_Page, Control_Memory);
                  end if;
               end if;
            end;
            exit when High = Last_Page;
            From := High + 1;
         else
            exit;
         end if;
         pragma Loop_Invariant
           (for all I in 1 .. Count =>
              Firmware_Range (M, Runs (I).First, Runs (I).Last, Runs (I).Kind));
         pragma Loop_Invariant (for all I in 2 .. Count => Runs (I - 1).Last < Runs (I).First);
         pragma Loop_Invariant
           (for all I in Count + 1 .. Max_Firmware_Roots => Runs (I) = No_Firmware_Run);
      end loop;
   end Firmware_Runs;

end Physical_Ranges;
