with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.UI;
with CuBit.UI.App;
with Observatory_Archive_Reader;
with Observatory_Trace_View;
with Observatory_Trace_CCL;
with Observatory_Trace_Plot;
with Compositor_Requests;
with CCL_Manifest_Bindings;
with CCL.Catalog;
with CCL.Sessions;
with CCL.Language;
procedure Main is
   package UI renames CuBit.UI;
   package A renames CuBit.UI.App;
   package V renames Observatory_Trace_View;
   package Q renames Observatory_Trace_CCL;
   package P renames Observatory_Trace_Plot;
   package R is new Observatory_Archive_Reader (CCL_Manifest_Bindings.Slot_filesystem);
   use type R.Result_Kind, CCL.Catalog.Catalog_Error, CCL.Catalog.Grant_Result;
   use type CCL.Language.Interpretation_Status;
   Win : A.Window;
   View : V.State;
   Page : V.Page_Number := 0;
   Selected : Natural range 0 .. 63 := 0;
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   Outcome : CCL.Language.Interpretation_Result;
   procedure Submit is new CCL.Sessions.Submit_With_Values (Q.Context, Q.Invoke);
   Sequence, Token, Now : Unsigned_64 := 0;
   OK, Found, Consumed, Healthy : Boolean;
   Dirty, Running : Boolean := True;
   Seen_Status : R.Result_Kind := R.Idle;
   Event : A.Input_Event;
   Receipt : aliased CompletionEntry;
   Repair : UI.Rect;
   Activity : Activity_Result;
   function Decimal (Value : Unsigned_64) return String is
      S : constant String := Value'Image;
   begin return S (S'First + 1 .. S'Last); end Decimal;
   function Add (Value, Delta_Time : Unsigned_64) return Unsigned_64 is
     (if Value > Unsigned_64'Last - Delta_Time then Unsigned_64'Last else Value + Delta_Time);
   function Micros (Value : Unsigned_64) return Unsigned_64 is
     (if Value > Unsigned_64'Last / 1000 then Unsigned_64'Last else Value * 1000);
   function Evaluate (Code : String) return String is
   begin
      Submit (Session, Code, 4096, Grants, View, Outcome);
      if Outcome.Status /= CCL.Language.Succeeded then return "unavailable"; end if;
      declare Text : constant String := CCL.Sessions.Result_Image (Outcome); begin
         if Text'Length >= 8 and then Text (Text'First .. Text'First + 7) = "String: " then
            return Text (Text'First + 8 .. Text'Last);
         end if;
      end;
      return "unavailable";
   end Evaluate;
   function Field (Name : String) return String is
     (Evaluate ("(" & Name & " " & Decimal (Unsigned_64 (Selected)) & ")"));
   procedure Reload is
   begin
      R.Start (Page, OK);
      if OK then V.Start (View, [others => 0], Page); Selected := 0; Dirty := True; end if;
   end Reload;
   procedure Handle is
   begin
      A.Begin_Input_Event (Win, Event);
      if Event.kind = A.INPUT_CONFIGURE then Dirty := True;
      elsif Event.kind = A.INPUT_KEY_DOWN then
         case Event.payload0 is
            when A.KEY_ESC | A.KEY_Q => Running := False;
            when A.KEY_R => Reload;
            when 16#31# =>
               if R.Status /= R.Loading and Page < V.Page_Number'Last and
                 (Natural (Page) + 1) * 64 < V.Total (View)
               then Page := Page + 1; Reload; end if;
            when 16#19# =>
               if R.Status /= R.Loading and Page > 0 then Page := Page - 1; Reload; end if;
            when 16#48# | 16#4B# =>
               if Selected > 0 then Selected := Selected - 1; Dirty := True; end if;
            when 16#50# | 16#4D# =>
               if Selected + 1 < V.Length (View) then Selected := Selected + 1; Dirty := True; end if;
            when others => null;
         end case;
      end if;
      A.Finish_Input_Event (Win, Event);
   end Handle;
   function Lane_Name (Lane : P.Lane) return String is
     (case Lane is when 0 => "Input dequeue", when 1 => "Surface accepted",
      when 2 => "Client draw", when 3 => "Submission", when 4 => "Completion");
   procedure Paint is
      C : constant UI.Canvas := A.Canvas (Win, Repair);
      Theme : constant UI.Theme := UI.Current_Theme;
      Size : constant P.Width := P.Width (Natural'Max (1, Natural'Min (512, A.Width (Win) - 200)));
      Bounds : P.Bounds;
      Item : P.Interval;
      Bar : P.Bar;
   begin
      UI.Fill_Rect (C, A.Full_Rect (Win), Theme.face);
      UI.Draw_UI_Text (C, 16, 14, "Desktop trace", Theme.text, Theme.face);
      UI.Draw_UI_Text (C, 16, 38, "Completion spans: submission to collection", Theme.muted, Theme.face);
      if V.Ready (View) then
         UI.Draw_UI_Text (C, 16, 64, "Page " & Decimal (Unsigned_64 (Page) + 1) &
           " | " & Decimal (Unsigned_64 (V.Length (View))) & " of " & Decimal (Unsigned_64 (V.Total (View))) &
           " events | " & Evaluate ("(trace.stop-reason)") &
           (if V.Lossy (View) then " | LOSS REPORTED" else ""),
           (if V.Lossy (View) or V.Observer_Failed (View) then Theme.danger else Theme.text), Theme.face);
         Bounds := P.Extent (View);
         for Lane in P.Lane loop
            UI.Draw_UI_Text (C, 16, 108 + Lane * 28, Lane_Name (Lane), Theme.muted, Theme.face);
            UI.Fill_Rect (C, (176, 104 + Lane * 28, Size, 22), Theme.field);
         end loop;
         for I in 1 .. V.Length (View) loop
            Item := P.Describe (V.Value_At (View, I)); Bar := P.Project (Item, Bounds, Size);
            UI.Fill_Rect (C, (176 + Bar.Left, 108 + Item.Row * 28, Bar.Pixels, 14),
              (if I = Selected + 1 then Theme.danger elsif Item.Row = 4 then Theme.good else Theme.accent));
         end loop;
         UI.Draw_UI_Text (C, 16, 250, "Page interval: " & Decimal (Bounds.Last - Bounds.First) & " us", Theme.muted, Theme.face);
         if V.Length (View) > 0 then
            UI.Draw_UI_Text (C, 16, 282, "Selected " & Decimal (Unsigned_64 (Selected + 1)) & ": " & Field ("trace.kind"), Theme.text, Theme.face);
            UI.Draw_UI_Text (C, 16, 306, "Time " & Field ("trace.time-us") & " us | Duration " & Field ("trace.duration-us") & " us", Theme.text, Theme.face);
            UI.Draw_UI_Text (C, 16, 330, "PID " & Field ("trace.pid") & " | Publisher " & Field ("trace.publisher"), Theme.text, Theme.face);
            UI.Draw_UI_Text (C, 16, 354, "Observer " & Field ("trace.observer") & " | Event " & Field ("trace.event-id"), Theme.text, Theme.face);
            UI.Draw_UI_Text (C, 16, 378, "Output " & Field ("trace.output") & " | Session " & Field ("trace-detail.session") & " | Frame " & Field ("trace-detail.frame"), Theme.text, Theme.face);
         end if;
      else
         UI.Draw_UI_Text (C, 16, 76,
           (case R.Status is
              when R.Loading => "Loading saved capture...",
              when R.Incomplete => "Capture is incomplete or invalid. R to reload.",
              when R.Unavailable => "Capture unavailable. Close and reopen to retry.",
              when others => "No checked capture loaded."), Theme.danger, Theme.face);
      end if;
      UI.Draw_UI_Text (C, 16, A.Height (Win) - 28,
        "Arrows select | N/P page | R reload | Esc close", Theme.muted, Theme.face);
   end Paint;
begin
   declare Error : CCL.Catalog.Catalog_Error;
      Resolved : CCL.Catalog.Resolved_Operation;
      Grant : CCL.Catalog.Grant_Result;
   begin
      CCL.Catalog.Initialize (Catalog); CCL.Catalog.Initialize (Grants);
      Q.Publish (Catalog, Error); if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      CCL.Sessions.Initialize (Session, Catalog);
      for Op in Q.Operation loop
         CCL.Catalog.Resolve (Catalog, Q.Namespace (Op) & "." & Q.Name (Op), Resolved, Found);
         if not Found then return; end if;
         CCL.Catalog.Install (Grants, Resolved, Q.Binding (Op), Grant);
         if Grant /= CCL.Catalog.Grant_Added then return; end if;
      end loop;
   end;
   A.Open (Win, 760, 520, A.WINDOW_FLAG_DECORATED or A.WINDOW_FLAG_CLOSEABLE or
     A.WINDOW_FLAG_MINIMIZABLE, OK, title => "Desktop trace", protected_frames => True,
     minimum_width => 640, minimum_height => 440);
   if not OK then return; end if;
   Reload;
   while Running and A.Is_Open (Win) loop
      Now := syscall (SYSCALL_GETTIME);
      for Drain in 1 .. 16 loop
         exit when Poll_Completion (Receipt'Address) = 0;
         A.Complete_Input_Wait (Win, Receipt, Event, Found, Consumed, Healthy);
         if Consumed then
            if not Healthy then Running := False; elsif Found then Handle; end if;
         elsif R.Matches (Receipt.token) then R.Collect (Receipt);
         else R.Close; end if;
      end loop;
      exit when not Running;
      R.Tick (Sequence, Micros (Now));
      if R.Status /= Seen_Status then
         Seen_Status := R.Status; Dirty := True;
         if R.Status = R.Complete then
            R.Take (View, OK);
            debugPrint ("trace-viewer: checked page=" & Page'Image & " rows=" & V.Length (View)'Image & ASCII.LF);
         elsif R.Status = R.Incomplete then debugPrint ("trace-viewer: incomplete capture" & ASCII.LF);
         elsif R.Status = R.Unavailable then debugPrint ("trace-viewer: file unavailable" & ASCII.LF); end if;
      end if;
      if Dirty or A.Frame_Pending (Win) then
         A.Begin_Paint (Win, (if Dirty then A.Full_Rect (Win) else (0, 0, 0, 0)), Repair, OK);
         if OK then Paint; A.Present (Win, Repair); Dirty := False; end if;
      end if;
      if not A.Input_Wait_Pending (Win) then
         Compositor_Requests.Allocate (Sequence, Token); if Token = 0 then exit; end if;
         A.Submit_Input_Wait (Win, Token, OK); if not OK then exit; end if;
      end if;
      Activity := Wait_For_Activity_Until
        ((if Dirty or R.Status = R.Loading or R.Cleanup_Pending or A.Frame_Pending (Win)
          then Add (Now, 10) else Unsigned_64'Last));
      exit when Activity = Unavailable;
   end loop;
   R.Close; A.Close (Win);
end Main;
