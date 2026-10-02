with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metric_Protocol;
with CuBit.Metric_Records;
with Observatory_Metric_Observer;
with Observatory_Metric_Queries;
with CCL_Manifest_Bindings;
with Observatory_CCL;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Sessions;
with CCL.Language;
procedure Main is
   package P renames CuBit.Metric_Protocol;
   package R renames CuBit.Metric_Records;
   package O is new Observatory_Metric_Observer
     (CCL_Manifest_Bindings.Slot_metrics_observer);
   package View renames Observatory_CCL;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Language.Interpretation_Status;
   View_State : View.Context;
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants, Missing : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   Outcome : CCL.Language.Interpretation_Result;
   Catalog_Error : CCL.Catalog.Catalog_Error;
   Resolved : CCL.Catalog.Resolved_Operation;
   Grant : CCL.Catalog.Grant_Result;
   procedure Submit is new CCL.Sessions.Submit_With_Values
     (View.Context, View.Invoke);
   Rows : P.Summary_Page;
   Written : P.Row_Count;
   Next : Observatory_Metric_Queries.Cursor;
   Sequence, First_Count, Queries, Pulses, Ignore : Unsigned_64 := 0;
   Submitted, Success, Found : Boolean;
   C : aliased CompletionEntry;
   Stages_Seen : array (3 .. 6) of Boolean := [others => False];
   Expected : constant String := "desktop.out0.submit_release";
   function Now return Unsigned_64 is
      Millis : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   begin
      return (if Millis > Unsigned_64'Last / 1000 then Unsigned_64'Last else Millis * 1000);
   end Now;
   procedure Fail (Reason : String) is
   begin
      debugPrint ("TEST: FAIL observatory-async " & Reason & ASCII.LF);
      O.Close;
      Ignore := syscall (SYSCALL_EXIT, 1);
      loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
   end Fail;
   function Decimal (N : Unsigned_64) return String is
      S : constant String := N'Image;
   begin
      return S (S'First + 1 .. S'Last);
   end Decimal;
   procedure Evaluate (Source, Expected_Image : String) is
   begin
      Submit (Session, Source, 4096, Grants, View_State, Outcome);
      if Outcome.Status /= CCL.Language.Succeeded or else
        CCL.Sessions.Result_Image (Outcome) /= Expected_Image
      then Fail ("CCL evaluation: " & Source); end if;
   end Evaluate;
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);
   CCL.Catalog.Initialize (Missing);
   View.Publish (Catalog, Catalog_Error);
   if Catalog_Error /= CCL.Catalog.Catalog_Valid then Fail ("CCL catalog"); end if;
   CCL.Sessions.Initialize (Session, Catalog);
   Submit (Session, "(metrics.count)", 4096, Missing, View_State, Outcome);
   if Outcome.Status /= CCL.Language.Host_Authority_Denied then
      Fail ("CCL missing authority admitted");
   end if;
   for Op in View.Operation loop
      CCL.Catalog.Resolve (Catalog, "metrics." & View.Name (Op), Resolved, Found);
      if not Found then Fail ("CCL resolve"); end if;
      CCL.Catalog.Install (Grants, Resolved, View.Binding (Op), Grant);
      if Grant /= CCL.Catalog.Grant_Added then Fail ("CCL grant"); end if;
   end loop;
   Evaluate ("(metrics.ready)", "Boolean: false");
   for Attempt in 1 .. 400 loop
      O.Begin_Query (0, Sequence, Now, Submitted);
      if not Submitted then Fail ("submit"); end if;
      Found := False;
      for Turn in 1 .. 300 loop
         -- Stand-in for other event-loop work; adapter calls never wait.
         Pulses := Pulses + 1;
         for Drain in 1 .. 16 loop
            exit when Poll_Completion (C'Address) = 0;
            if not O.Matches (C.token) then Fail ("unrouted token"); end if;
            O.Collect (C);
         end loop;
         O.Tick (Now);
         if O.Disabled then Fail ("query disabled or timed out"); end if;
         if O.Ready then
            O.Take (Rows, Written, Next, Success);
            if not Success then Fail ("take"); end if;
            Found := True;
            exit;
         end if;
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      if not Found then Fail ("bounded event loop exhausted"); end if;
      -- The adapter has confirmed retirement before this private cache copy.
      View.Replace (View_State, Rows, Written, Success);
      if not Success then Fail ("CCL rejected retired page"); end if;
      Evaluate ("(metrics.ready)", "Boolean: true");
      Evaluate ("(metrics.count)", "Integer: " & Decimal (Unsigned_64 (Written)));
      Queries := Queries + 1;
      if Next /= 512 then Fail ("unexpected continuation"); end if;
      -- Check live stage rows independently of release-row ordering. These
      -- values traversed real publication, collector, retired grant and CCL.
      for I in 0 .. Written - 1 loop
         if Rows (I) (P.Row_Source) = getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP) and then
           Rows (I) (P.Row_Key) in 3 .. 6
         then
            declare
               Key : constant Natural := Natural (Rows (I) (P.Row_Key));
               Name : constant String := (case Key is
                 when 3 => "desktop.input_dispatch", when 4 => "desktop.request_dispatch",
                 when 5 => "desktop.scene_draw", when others => "desktop.submit_call");
               Index : constant String := Decimal (Unsigned_64 (I));
            begin
               if Rows (I) (P.Row_Kind) /= R.Record_Kind'Enum_Rep (R.Latency) or else
                 Rows (I) (P.Row_Unit) /= R.Unit'Enum_Rep (R.Microseconds)
               then Fail ("stage schema"); end if;
               Evaluate ("(metrics.name " & Index & ")", "String: " & Name);
               Evaluate ("(metrics.samples " & Index & ")",
                         "String: " & Decimal (Rows (I) (P.Row_Count_Word)));
               Evaluate ("(metrics.lossy " & Index & ")", "Boolean: false");
               if Rows (I) (P.Row_Count_Word) > 0 and then not Stages_Seen (Key) then
                  Stages_Seen (Key) := True;
                  debugPrint ("TEST: stage metric " & Name & " samples=" &
                    Rows (I) (P.Row_Count_Word)'Image & ASCII.LF);
               end if;
            end;
         end if;
      end loop;
      for I in 0 .. Written - 1 loop
         if Rows (I) (P.Row_Source) = getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP) and then
           Rows (I) (P.Row_Key) = 1
         then
            if Rows (I) (P.Row_Kind) /= R.Record_Kind'Enum_Rep (R.Span) or else
              Rows (I) (P.Row_Unit) /= R.Unit'Enum_Rep (R.Microseconds) or else
              Rows (I) (P.Row_Source_Rejected) /= 0 or else
              Rows (I) (P.Row_Source_Producer_Dropped) /= 0 or else
              Rows (I) (P.Row_Source_Batch_Gaps) /= 0
            then Fail ("schema or loss"); end if;
            for J in Expected'Range loop
               if (Shift_Right (Rows (I) (P.Row_First_Name + (J - 1) / 8),
                                ((J - 1) mod 8) * 8) and 255) /= Character'Pos (Expected (J))
               then Fail ("metric name"); end if;
            end loop;
            declare
               Index : constant String := Decimal (Unsigned_64 (I));
               Expression : constant String :=
                 "(concat (metrics.name " & Index & ") (concat "": p99 <= "" " &
                 "(concat (metrics.p99-upper " & Index & ") (concat "" "" " &
                 "(metrics.unit " & Index & ")))))";
               Expected_Text : constant String := Expected & ": p99 <= " &
                 (if Rows (I) (P.Row_Count_Word) = 0 then "unavailable"
                  else Decimal (Rows (I) (P.Row_P99))) & " us";
            begin
               Evaluate ("(metrics.samples " & Index & ")",
                         "String: " & Decimal (Rows (I) (P.Row_Count_Word)));
               Evaluate ("(metrics.lossy " & Index & ")", "Boolean: false");
               Evaluate (Expression, "String: " & Expected_Text);
               if Queries = 1 then
                  debugPrint ("TEST: observatory-ccl " &
                    CCL.Sessions.Result_Image (Outcome) & ASCII.LF);
               end if;
            end;
            if First_Count = 0 then First_Count := Rows (I) (P.Row_Count_Word);
            elsif Queries >= 3 and then Rows (I) (P.Row_Count_Word) > First_Count and then
              Rows (I) (P.Row_Count_Word) >= 3 and then (for all Seen of Stages_Seen => Seen)
            then
               View.Clear (View_State);
               Evaluate ("(metrics.ready)", "Boolean: false");
               Evaluate ("(metrics.count)", "Integer: 0");
               debugPrint ("TEST: PASS desktop stage metrics all four live" & ASCII.LF);
               debugPrint ("TEST: PASS observatory-ccl live summary expressions" & ASCII.LF);
               O.Close;
               debugPrint ("TEST: PASS observatory-async queries=" & Queries'Image &
                 " event-turns=" & Pulses'Image & " grant-retirement=confirmed" & ASCII.LF);
               debugPrint ("TEST: PASS desktop-metrics frames=" &
                 Rows (I) (P.Row_Count_Word)'Image & " batches=" &
                 Rows (I) (P.Row_Source_Batches)'Image & ASCII.LF);
               Ignore := syscall (SYSCALL_EXIT, 0);
               return;
            end if;
         end if;
      end loop;
      Ignore := syscall (SYSCALL_SLEEP, 50);
   end loop;
   Fail ("no growing Desktop release series");
end Main;
