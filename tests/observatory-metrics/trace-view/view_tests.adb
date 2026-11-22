with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Observatory_Trace_CCL;
with CCL.Catalog;
with CCL.Sessions;
with CCL.Language;
procedure View_Tests is
   package O renames Observatory_Trace_CCL;
   package V renames O.V;
   package F renames V.F;
   package A renames V.A;
   package W renames A.W;
   package IO renames Ada.Streams.Stream_IO;
   use type CCL.Catalog.Catalog_Error, CCL.Catalog.Grant_Result;
   use type CCL.Language.Interpretation_Status, A.S.Capture;
   View : O.Context;
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants, Missing : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   Outcome : CCL.Language.Interpretation_Result;
   Error : CCL.Catalog.Catalog_Error;
   Resolved : CCL.Catalog.Resolved_Operation;
   Grant : CCL.Catalog.Grant_Result;
   Found : Boolean;
   procedure Submit is new CCL.Sessions.Submit_With_Values (O.Context, O.Invoke);
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Checks'Image; end if; end Check;
   procedure Run (Source, Expected : String) is
   begin
      Submit (Session, Source, 4096, Grants, View, Outcome);
      Check (Outcome.Status = CCL.Language.Succeeded);
      if CCL.Sessions.Result_Image (Outcome) /= Expected then
         Put_Line (Source & " => " & CCL.Sessions.Result_Image (Outcome));
      end if;
      Check (CCL.Sessions.Result_Image (Outcome) = Expected);
   end Run;
   procedure Load (Page : V.Page_Number; Omit_Footer : Boolean := False; Tail : Natural := 0) is
      File : IO.File_Type;
      Bytes : Stream_Element_Array (1 .. 256);
      Last : Stream_Element_Offset;
      Data : A.Chunk;
      First : Boolean := True;
      Sequence : Natural := 0;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      while not IO.End_Of_File (File) loop
         IO.Read (File, Bytes, Last); Check (Last = 256);
         for I in Data'Range loop
            Data (I) := 0;
            for J in 0 .. 7 loop
               Data (I) := Data (I) or Shift_Left (Unsigned_64 (Bytes (Stream_Element_Offset (I * 8 + J + 1))), J * 8);
            end loop;
         end loop;
         if First then V.Start (View, Data, Page); First := False;
         elsif Omit_Footer and IO.End_Of_File (File) then null;
         else V.Feed (View, Data); end if;
         Check (not V.Ready (View) and V.Length (View) = 0);
         Sequence := Sequence + 1;
      end loop;
      IO.Close (File); Check (Sequence = 80); V.Finish (View, Tail);
   end Load;
   Header, Data, Footer : A.Chunk;
   Archive : F.State;
   Accepted : Boolean;
   Capture : A.S.Capture :=
     (True, Unsigned_64'Last, Unsigned_64'Last, A.S.P.Publisher_Tag (A.S.P.Issuance'Last),
      Unsigned_64'Last, Unsigned_64'Last - 3, 0, 0,
      (W.Frame_Event, Unsigned_64'Last, (0, Unsigned_64'Last, Unsigned_64'Last, 10, 20)));
begin
   CCL.Catalog.Initialize (Catalog); CCL.Catalog.Initialize (Grants);
   O.Publish (Catalog, Error); Check (Error = CCL.Catalog.Catalog_Valid);
   CCL.Sessions.Initialize (Session, Catalog);
   for Op in O.Operation loop
      CCL.Catalog.Resolve (Catalog, O.Namespace (Op) & "." & O.Name (Op), Resolved, Found); Check (Found);
      CCL.Catalog.Install (Grants, Resolved, O.Binding (Op), Grant); Check (Grant = CCL.Catalog.Grant_Added);
   end loop;
   Run ("(trace.ready)", "Boolean: false"); Run ("(trace.count)", "Integer: 0");
   Load (0); Check (V.Ready (View) and V.Length (View) = 64 and V.Total (View) = 78);
   Run ("(trace.ready)", "Boolean: true"); Run ("(trace.count)", "Integer: 64");
   Run ("(trace.total)", "Integer: 78"); Run ("(trace.lossy)", "Boolean: true");
   Run ("(trace.stop-reason)", "String: requested-stop");
   Submit (Session, "(trace.kind -1)", 4096, Grants, View, Outcome); Check (Outcome.Status /= CCL.Language.Succeeded);
   Submit (Session, "(trace.kind 64)", 4096, Grants, View, Outcome); Check (Outcome.Status /= CCL.Language.Succeeded);
   Submit (Session, "(trace.kind true)", 4096, Grants, View, Outcome); Check (Outcome.Status /= CCL.Language.Succeeded);
   CCL.Catalog.Initialize (Missing);
   Submit (Session, "(trace.kind 0)", 4096, Missing, View, Outcome); Check (Outcome.Status /= CCL.Language.Succeeded);
   Load (1); Check (V.Ready (View) and V.Length (View) = 14 and V.Total (View) = 78);
   Run ("(trace.page)", "Integer: 1");
   Load (63); Check (V.Ready (View) and V.Length (View) = 0 and V.Total (View) = 78);
   Load (0, True); Check (not V.Ready (View)); Run ("(trace.count)", "Integer: 0");
   Load (0, False, 1); Check (not V.Ready (View));
   Header := F.Header ((1, Unsigned_64'Last, 0, 1)); F.Start (Archive, Header); V.Start (View, Header, 0);
   Data := F.Event_Chunk (Archive, Capture); F.Feed (Archive, Data, Accepted); V.Feed (View, Data);
   Check (not V.Ready (View));
   Footer := F.Footer (Archive, 20, F.Budget_Reached, (Emitted_Events => 1, others => 0));
   V.Feed (View, Footer); V.Finish (View, 0);
   Check (V.Value_At (View, 1) = Capture);
   Run ("(trace.pid 0)", "String: 18446744073709551615");
   Run ("(trace.observer 0)", "String: 18446744073709551615");
   Run ("(trace.event-id 0)", "String: 18446744073709551615");
   Run ("(trace-detail.session 0)", "String: 18446744073709551615");
   Run ("(trace-detail.frame 0)", "String: 18446744073709551615");
   Run ("(trace.kind 0)", "String: completion-collected");
   Run ("(trace.has-duration 0)", "Boolean: true");
   Run ("(trace.duration-us 0)", "String: 10");
   Run ("(trace.surface 0)", "String: unavailable");
   Run ("(trace.lossy)", "Boolean: false"); Run ("(trace.stop-reason)", "String: budget-reached");
   V.Feed (View, Data); Check (not V.Ready (View));
   for Phase in 1 .. 4 loop
      Capture.Value := (case Phase is
         when 1 => (W.Input_Event, 1, (Unsigned_64'Last, Unsigned_64'Last, 1, 30)),
         when 2 => (W.Source_Event, 2, (Unsigned_64'Last, Unsigned_64'Last, Unsigned_64'Last, 0, 30)),
         when 3 => (W.Render_Event, 3, (W.RT.Draw, 1, 3, Unsigned_64'Last, Unsigned_64'Last,
                      Unsigned_64'Last, Unsigned_64'Last, Unsigned_64'Last, 0, 0, 30)),
         when others => (W.Render_Event, 4, (W.RT.Submit, 1, 3, Unsigned_64'Last, Unsigned_64'Last,
                           0, 0, 0, Unsigned_64'Last, Unsigned_64'Last, 30)));
      F.Start (Archive, Header); V.Start (View, Header, 0);
      Data := F.Event_Chunk (Archive, Capture); F.Feed (Archive, Data, Accepted); V.Feed (View, Data);
      Footer := F.Footer (Archive, 30, F.Budget_Reached, (Emitted_Events => 1, others => 0));
      V.Feed (View, Footer); V.Finish (View, 0); Check (V.Value_At (View, 1) = Capture);
      Run ("(trace.has-duration 0)", "Boolean: false");
      Run ("(trace.duration-us 0)", "String: unavailable");
      Run ("(trace.time-us 0)", "String: 30");
      if Phase <= 3 then Run ("(trace.surface 0)", "String: 18446744073709551615"); end if;
      case Phase is
         when 1 => Run ("(trace-detail.input-serial 0)", "String: 18446744073709551615");
         when 2 => Run ("(trace-detail.input-watermark 0)", "String: 0");
         when 3 => Run ("(trace-detail.source-ticket 0)", "String: 18446744073709551615");
         when others => Run ("(trace-detail.writer-serial 0)", "String: 18446744073709551615");
      end case;
   end loop;
   Header := F.Header ((1, Unsigned_64'Last, 0, 4096));
   for Page in V.Page_Number loop
      if Page in 0 | 1 | 63 then
         F.Start (Archive, Header); V.Start (View, Header, Page);
         for I in 1 .. 4096 loop
            Capture.Value.Event_ID := Unsigned_64 (I);
            Data := F.Event_Chunk (Archive, Capture); F.Feed (Archive, Data, Accepted); V.Feed (View, Data);
            Check (V.Length (View) = 0 and not V.Ready (View));
         end loop;
         Footer := F.Footer (Archive, 30, F.Budget_Reached, (Emitted_Events => 4096, others => 0));
         V.Feed (View, Footer); V.Finish (View, 0);
         Check (V.Length (View) = 64 and V.Total (View) = 4096);
         for I in V.Index loop
            Check (V.Value_At (View, I).Value.Event_ID = Unsigned_64 (Page * 64 + I));
         end loop;
      end if;
   end loop;
   F.Start (Archive, Header); V.Start (View, Header, 0);
   Data := F.Event_Chunk (Archive, Capture); F.Feed (Archive, Data, Accepted); V.Feed (View, Data);
   Footer := F.Footer (Archive, 30, F.Observer_Failed, (Emitted_Events => 1, others => 0));
   V.Feed (View, Footer); V.Finish (View, 0);
   Check (V.Ready (View) and V.Observer_Failed (View));
   Run ("(trace.stop-reason)", "String: observer-failed");
   Data (31) := Data (31) xor 1;
   V.Start (View, Header, 0); V.Feed (View, Data); V.Feed (View, Footer); V.Finish (View, 0);
   Check (not V.Ready (View) and V.Length (View) = 0);
   Put_Line ("PASS checked trace view and CCL queries checks=" & Checks'Image);
end View_Tests;
