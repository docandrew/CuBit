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

   procedure Add_Table (M : in out Map; First, Last : Unsigned_64) is
   begin
      if First > Last or else First >= Address_Limit or else not M.Tables_Complete then
         return;
      elsif M.Tables_Used = Max_Table_Extents then
         M.Tables_Complete := False;
         return;
      end if;
      M.Tables_Used := M.Tables_Used + 1;
      M.Tables (M.Tables_Used) :=
        (First => First, Last => Unsigned_64'Min (Last, Address_Limit - 1));
   end Add_Table;

   procedure Tables_Unknown (M : in out Map) is
   begin
      M.Tables_Complete := False;
   end Tables_Unknown;

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

   procedure Add_PCI_Config (M : in out Map; Item : MC.Allocation) is
      First, Last_Exclusive : Unsigned_64;
   begin
      if not M.Complete then return; end if;
      if not PW.Valid (Item, Address_Limit) or else M.PCI_Used = Max_PCI_Allocations then
         M.Complete := False; return;
      end if;
      First := Item.Base + Unsigned_64 (Item.First_Bus) * PCI_Bus_Bytes;
      Last_Exclusive := Item.Base + PW.Upper_Bytes (Item);
      for I in 1 .. M.PCI_Used loop
         if (Item.Segment = M.PCI (I).Segment
             and then Item.First_Bus <= M.PCI (I).Last_Bus
             and then M.PCI (I).First_Bus <= Item.Last_Bus)
           or else (First < M.PCI (I).Base + PW.Upper_Bytes (M.PCI (I))
             and then M.PCI (I).Base + Unsigned_64 (M.PCI (I).First_Bus) * PCI_Bus_Bytes < Last_Exclusive)
         then M.Complete := False; return; end if;
      end loop;
      M.PCI_Used := M.PCI_Used + 1;
      M.PCI (M.PCI_Used) := Item;
   end Add_PCI_Config;

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
         case Kind is
            when NVS_Memory | Control_Memory | Data_Memory =>
               -- Every span touching P has a control class, and one of them
               -- (wholly containing P) touches it, so Class_Of is that class
               -- or another touching span's.
               if Kind = NVS_Memory then
                  pragma Assert (NVS_Page (M, P));
               elsif Kind = Control_Memory then
                  pragma Assert (Control_Page_Admitted (M, P));
               else
                  pragma Assert (Data_Page_Admitted (M, P));
               end if;
               pragma Assert
                 (for all I in 1 .. M.Count =>
                    (if Touches (M.Spans (I), P) then Control_Class (M.Spans (I).Class)));
               pragma Assert
                 (for some I in 1 .. M.Count =>
                    Control_Class (M.Spans (I).Class) and then Within (M.Spans (I), P)
                    and then Touches (M.Spans (I), P));
               pragma Assert (Class_Of (M, P) /= Unreported);
               pragma Assert (Control_Class (Class_Of (M, P)));
            when PCI_Config_Memory =>
               -- Every span touching P is device-mappable, so Class_Of is
               -- one of their classes or Unreported (bus space).
               pragma Assert (PCI_Config_Page_Admitted (M, P));
               pragma Assert
                 (for all I in 1 .. M.Count =>
                    (if Touches (M.Spans (I), P) then Device_Mappable (M.Spans (I).Class)));
               pragma Assert (Device_Mappable (Class_Of (M, P)));
         end case;
         pragma Assert (Firmware_Class (Kind, Class_Of (M, P)));
         pragma Loop_Invariant
           (for all Q in Page_Of (First) .. P => Firmware_Class (Kind, Class_Of (M, Q)));
      end loop;
   end Lemma_Firmware_Range_Excludes_RAM;

   procedure Lemma_Data_Page_Alone (M : Map; P : Page_Number) is null;

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

   function First_Firmware_Cursor (M : Map) return Firmware_Cursor is
     (if M.Sealed and then M.Complete then
        (Phase => Admitted_Runs, From => 0,
         Control_Pending => Control_Page_Admitted (M, M.Control_Page), PCI_From => 0)
      else (Phase => Finished, others => <>));

   ---------------------------------------------------------------------------
   -- Uniform stretches (firmware data)
   ---------------------------------------------------------------------------

   -- Within, in pages: P is wholly inside S exactly when it is at or after
   -- S's first whole page and before its first page past the last whole one.
   procedure Lemma_Within_Pages (S : Span; P : Page_Number)
     with Ghost, Global => null,
          Post => Within (S, P) = (Ceil_Page (S.First) <= P and then P < Whole_End (S.Last))
   is
   begin
      pragma Assert ((S.First <= P * Page_Bytes) = (Ceil_Page (S.First) <= P));
      pragma Assert ((P * Page_Bytes + (Page_Bytes - 1) <= S.Last) = (P < Whole_End (S.Last)));
   end Lemma_Within_Pages;

   -- A span steady over [A, B] touches, and holds whole, every page of it
   -- alike.
   procedure Lemma_Span_Steady (S : Span; A, B, P : Page_Number)
     with Ghost, Global => null,
          Pre  => A <= P and then P <= B and then Span_Steady (S, A, B),
          Post => Touches (S, P) = Touches (S, A) and then Within (S, P) = Within (S, A)
   is
   begin
      Lemma_Within_Pages (S, P);
      Lemma_Within_Pages (S, A);
   end Lemma_Span_Steady;

   procedure Lemma_Uniform_Data (M : Map; A, B, P : Page_Number) is
   begin
      for I in 1 .. M.Count loop
         Lemma_Span_Steady (M.Spans (I), A, B, P);
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              Touches (M.Spans (J), P) = Touches (M.Spans (J), A)
              and then Within (M.Spans (J), P) = Within (M.Spans (J), A));
      end loop;
      pragma Assert (Data_Page_Admitted (M, P) = Data_Page_Admitted (M, A));
      pragma Assert (NVS_Page (M, P) = NVS_Page (M, A));
      pragma Assert (Table_Page (M, P) = Table_Page (M, A));
      pragma Assert (In_PCI_Config (M, P) = In_PCI_Config (M, A));
      pragma Assert ((P = M.Control_Page) = (A = M.Control_Page));
   end Lemma_Uniform_Data;

   -- The last page of the uniform stretch starting at A: the page before
   -- the nearest change above A (Uniform's boundaries), or the last page.
   function Stretch_End (M : Map; A : Page_Number) return Page_Number
     with Pre  => Well_Formed (M),
          Post => Stretch_End'Result >= A and then Uniform (M, A, Stretch_End'Result)
   is
      B : Page_Number := Last_Page;

      -- Move B below C when C changes something above A.
      procedure Cut (C : Unsigned_64)
        with Pre  => B >= A,
             Post => B <= B'Old and then B >= A and then Steady (C, A, B)
      is
      begin
         if C > A and then C - 1 < B then
            B := C - 1;
         end if;
      end Cut;
   begin
      for I in 1 .. M.Count loop
         Cut (Page_Of (M.Spans (I).First));
         Cut (Page_Of (M.Spans (I).Last) + 1);
         Cut (Ceil_Page (M.Spans (I).First));
         Cut (Whole_End (M.Spans (I).Last));
         pragma Loop_Invariant (B >= A);
         pragma Loop_Invariant (for all J in 1 .. I => Span_Steady (M.Spans (J), A, B));
      end loop;
      for I in 1 .. M.Tables_Used loop
         Cut (Page_Of (M.Tables (I).First));
         Cut (Page_Of (M.Tables (I).Last) + 1);
         pragma Loop_Invariant (B >= A);
         pragma Loop_Invariant (for all J in 1 .. M.Count => Span_Steady (M.Spans (J), A, B));
         pragma Loop_Invariant (for all J in 1 .. I => Extent_Steady (M.Tables (J), A, B));
      end loop;
      Cut (M.Control_Page);
      Cut (M.Control_Page + 1);
      for I in 1 .. M.PCI_Used loop
         if PW.Valid (M.PCI (I), Address_Limit) then
            Cut (PCI_First_Page (M.PCI (I)));
            Cut (PCI_End_Page (M.PCI (I)));
         end if;
         pragma Loop_Invariant (B >= A);
         pragma Loop_Invariant (for all J in 1 .. M.Count => Span_Steady (M.Spans (J), A, B));
         pragma Loop_Invariant (for all J in 1 .. M.Tables_Used => Extent_Steady (M.Tables (J), A, B));
         pragma Loop_Invariant (Steady (M.Control_Page, A, B) and then Steady (M.Control_Page + 1, A, B));
         pragma Loop_Invariant (for all J in 1 .. I =>
           PW.Valid (M.PCI (J), Address_Limit)
           and then Steady (PCI_First_Page (M.PCI (J)), A, B)
           and then Steady (PCI_End_Page (M.PCI (J)), A, B));
      end loop;
      return B;
   end Stretch_End;

   -- The lowest data page at or after From: From or the first page of a
   -- stretch (Uniform's boundaries), the first of them that is a data page.
   -- Not trusted beyond what it checks: Low is a data page at or after
   -- From.
   procedure Next_Data_Start
     (M : Map; From : Page_Number; Found : out Boolean; Low : out Page_Number)
     with Pre  => Well_Formed (M),
          Post => (if Found then Low >= From and then Data_Page (M, Low))
   is
      procedure Consider (C : Unsigned_64)
        with Pre  => (if Found then Low >= From and then Data_Page (M, Low)),
             Post => (if Found then Low >= From and then Data_Page (M, Low))
                     and then (if Found'Old then Found)
      is
      begin
         if C >= From and then C <= Last_Page and then (not Found or else C < Low)
           and then Data_Page (M, C)
         then
            Found := True;
            Low := C;
         end if;
      end Consider;
   begin
      Found := False;
      Low := From;
      Consider (From);
      for I in 1 .. M.Count loop
         Consider (Page_Of (M.Spans (I).First));
         Consider (Page_Of (M.Spans (I).Last) + 1);
         Consider (Ceil_Page (M.Spans (I).First));
         Consider (Whole_End (M.Spans (I).Last));
         pragma Loop_Invariant (if Found then Low >= From and then Data_Page (M, Low));
      end loop;
      for I in 1 .. M.Tables_Used loop
         Consider (Page_Of (M.Tables (I).First));
         Consider (Page_Of (M.Tables (I).Last) + 1);
         pragma Loop_Invariant (if Found then Low >= From and then Data_Page (M, Low));
      end loop;
      Consider (M.Control_Page);
      Consider (M.Control_Page + 1);
      for I in 1 .. M.PCI_Used loop
         if PW.Valid (M.PCI (I), Address_Limit) then
            Consider (PCI_First_Page (M.PCI (I)));
            Consider (PCI_End_Page (M.PCI (I)));
         end if;
         pragma Loop_Invariant (if Found then Low >= From and then Data_Page (M, Low));
      end loop;
   end Next_Data_Start;

   procedure Next_PCI_Window
     (M : Map; From : Unsigned_64; Found : out Boolean; First, Last : out Page_Number)
     with Pre => Well_Formed (M),
       Post => (if Found then First >= From and then First <= Last_PCI_Bus_Page
         and then Last = First + PCI_Bus_Pages - 1)
   is
      Bus, Relative, Base_Page, Candidate : Unsigned_64;
   begin
      Found := False; First := 0; Last := 0;
      if From > Last_Page then return; end if;
      for I in 1 .. M.PCI_Used loop
         Base_Page := M.PCI (I).Base / Page_Bytes;
         if From <= Base_Page then Bus := Unsigned_64 (M.PCI (I).First_Bus);
         else
            Relative := From - Base_Page;
            Bus := Relative / PCI_Bus_Pages + (if Relative mod PCI_Bus_Pages = 0 then 0 else 1);
            Bus := Unsigned_64'Max (Bus, Unsigned_64 (M.PCI (I).First_Bus));
         end if;
         if Bus <= Unsigned_64 (M.PCI (I).Last_Bus) then
            Candidate := Base_Page + Bus * PCI_Bus_Pages;
            if not Found or else Candidate < First then
               First := Candidate; Last := Candidate + PCI_Bus_Pages - 1; Found := True;
            end if;
         end if;
         pragma Loop_Invariant (if Found then First >= From and then First <= Last_PCI_Bus_Page
           and then Last = First + PCI_Bus_Pages - 1);
      end loop;
   end Next_PCI_Window;

   procedure Next_Firmware_Run
     (M : Map; Cursor : in out Firmware_Cursor;
      Found : out Boolean; Run : out Firmware_Run; Left_Out : in out Natural)
   is
      Start : constant Firmware_Cursor := Cursor with Ghost;
      NVS_Found : Boolean;
      Low, High : Page_Number;
      -- The control page, or the next PCI bus window, precedes the NVS run.
      Control_Next, PCI_Next, PCI_Found : Boolean;
      PCI_First, PCI_Last : Page_Number;

      -- Pages [First_Page, Last_Page_Of] are the run if they are verified
      -- firmware memory of Kind and short enough; otherwise they are counted
      -- left out.
      procedure Try (First_Page, Last_Page_Of : Page_Number; Kind : Firmware_Kind)
        with Pre  => First_Page <= Last_Page_Of and then not Found,
             Post => (if Found then
                        Run = (First => First_Page * Page_Bytes,
                               Last  => Last_Page_Of * Page_Bytes + (Page_Bytes - 1),
                               Kind  => Kind)
                        and then Firmware_Range (M, Run.First, Run.Last, Run.Kind))
      is
         First : constant Address := First_Page * Page_Bytes;
         Last  : constant Address := Last_Page_Of * Page_Bytes + (Page_Bytes - 1);
      begin
         if Last_Page_Of - First_Page < Max_Firmware_Run_Pages
           and then Firmware_Range (M, First, Last, Kind)
         then
            Found := True;
            Run := (First => First, Last => Last, Kind => Kind);
         elsif Left_Out < Natural'Last then
            Left_Out := Left_Out + 1;
         end if;
      end Try;

      -- The admitted phase is over; firmware data comes next. Rejected PCI
      -- bus candidates were counted by their lane, never again at this transition.
      procedure End_Admitted
        with Pre  => Cursor.Phase = Admitted_Runs,
             Post => Cursor.Phase = Data_Runs and then Cursor.From = 0
      is
      begin
         Cursor := (Phase => Data_Runs, From => 0, Control_Pending => False,
                    PCI_From => 0);
      end End_Admitted;
   begin
      Found := False;
      Run := No_Firmware_Run;
      if not (M.Sealed and then M.Complete) then
         Cursor.Phase := Finished;
         return;
      end if;

      -- NVS runs, the control page and individual PCI buses, ascending.
      -- Each round advances a PCI candidate, the control page, or an NVS
      -- boundary. The bound includes every bus in every allocation and
      -- both boundaries of every span; it is not just the span count.
      -- PCI_From advances even for a rejected bus, while From still allows
      -- an NVS/control root inside it. Left_Out counts that bus once.
      if Cursor.Phase = Admitted_Runs then
         for Round in 0 .. 2 * M.Count + (Natural (MC.Bus_Number'Last) + 1) * M.PCI_Used + 2 loop
            pragma Loop_Invariant (not Found and then Cursor.Phase = Admitted_Runs
                                   and then Cursor.From >= Start.From);
            Next_NVS_Run (M, Cursor.From, NVS_Found, Low, High);
            Control_Next := Cursor.Control_Pending and then M.Control_Page >= Cursor.From
              and then (not NVS_Found or else M.Control_Page < Low);
            Next_PCI_Window (M, Cursor.PCI_From, PCI_Found, PCI_First, PCI_Last);
            PCI_Next := PCI_Found and then
              (PCI_First < Cursor.From or else
               ((not NVS_Found or else PCI_First < Low)
                and then (not Control_Next or else PCI_First < M.Control_Page)));
            if PCI_Next then
               -- Advance this lane even when another class blocks the bus.
               -- Do not skip an NVS/control root within a rejected PCI window.
               Cursor.PCI_From := PCI_Last + 1;
               if PCI_First >= Cursor.From then
                  Try (PCI_First, PCI_Last, PCI_Config_Memory);
               elsif Left_Out < Natural'Last then Left_Out := Left_Out + 1;
               end if;
               if Found then
                  if PCI_Last = Last_Page then End_Admitted;
                  else Cursor.From := PCI_Last + 1; end if;
                  return;
               end if;
            elsif Control_Next then
               Cursor.Control_Pending := False;
               Try (M.Control_Page, M.Control_Page, Control_Memory);
               if M.Control_Page < Last_Page then
                  Cursor.From := M.Control_Page + 1;
               else
                  End_Admitted;
               end if;
               if Found or else Cursor.Phase /= Admitted_Runs then
                  exit;
               end if;
            elsif NVS_Found then
               Try (Low, High, NVS_Memory);
               if Cursor.Control_Pending and then M.Control_Page in Low .. High then
                  Cursor.Control_Pending := False;
                  -- A run left out still leaves the control page its own.
                  if not Found then
                     Try (M.Control_Page, M.Control_Page, Control_Memory);
                  end if;
               end if;
               if High = Last_Page then
                  End_Admitted;
               else
                  Cursor.From := High + 1;
               end if;
               if Found or else Cursor.Phase /= Admitted_Runs then
                  exit;
               end if;
            else
               End_Admitted;
               exit;
            end if;
         end loop;
         if Found then
            return;
         end if;
         if Cursor.Phase = Admitted_Runs then
            End_Admitted;
         end if;
      end if;

      -- Firmware data, ascending, a uniform stretch at a time: Low is a data
      -- page, so its whole stretch is (Lemma_Uniform_Data); the run grows
      -- while the next stretch starts with a data page, up to
      -- Max_Firmware_Run_Pages. Each stretch ends at a boundary, so the
      -- boundaries bound the growth.
      if Cursor.Phase = Data_Runs then
         Next_Data_Start (M, Cursor.From, Found, Low);
         if not Found then
            Cursor.Phase := Finished;
            return;
         end if;
         High := Stretch_End (M, Low);
         for P in Low .. High loop
            Lemma_Uniform_Data (M, Low, High, P);
            pragma Loop_Invariant (for all Q in Low .. P => Data_Page (M, Q));
         end loop;
         for Round in 0 .. 4 * M.Count + 2 * M.Tables_Used + 2 * M.PCI_Used + 4 loop
            pragma Loop_Invariant (Low <= High and then (for all Q in Low .. High => Data_Page (M, Q)));
            exit when High = Last_Page or else High - Low + 1 >= Max_Firmware_Run_Pages
              or else not Data_Page (M, High + 1);
            declare
               Next_End : constant Page_Number := Stretch_End (M, High + 1);
            begin
               for P in High + 1 .. Next_End loop
                  Lemma_Uniform_Data (M, High + 1, Next_End, P);
                  pragma Loop_Invariant (for all Q in High + 1 .. P => Data_Page (M, Q));
               end loop;
               High := Next_End;
            end;
         end loop;
         if High - Low >= Max_Firmware_Run_Pages then
            High := Low + (Max_Firmware_Run_Pages - 1);
         end if;
         Run := (First => Low * Page_Bytes, Last => High * Page_Bytes + (Page_Bytes - 1),
                 Kind => Data_Memory);
         pragma Assert (for all Q in Page_Of (Run.First) .. Page_Of (Run.Last) =>
                          Firmware_Page (M, Q, Data_Memory));
         if High = Last_Page then
            Cursor.Phase := Finished;
         else
            Cursor.From := High + 1;
         end if;
         return;
      end if;

      Cursor.Phase := Finished;
   end Next_Firmware_Run;

end Physical_Ranges;
