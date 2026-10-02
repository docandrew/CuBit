with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.UI;
with CuBit.UI.App;
with CuBit.Metric_Protocol;
with Observatory_Metric_Observer;
with Observatory_Metric_Queries;
with Observatory_CCL;
with Observatory_History;
with Observatory_Format_Budget;
with Compositor_Requests;
with CCL_Manifest_Bindings;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Sessions;
with CCL.Language;
procedure Main is
   package UI renames CuBit.UI;
   package A renames CuBit.UI.App;
   package P renames CuBit.Metric_Protocol;
   package V renames Observatory_CCL;
   package H renames Observatory_History;
   package F renames Observatory_Format_Budget;
   Formatting : F.State;
   History : H.History;
   Selected : P.Row_Index := 0;
   Graphs : Boolean := True;
   package O is new Observatory_Metric_Observer
     (CCL_Manifest_Bindings.Slot_metrics_observer);
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Language.Interpretation_Status;
   Win : A.Window;
   View : V.Context;
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   Outcome : CCL.Language.Interpretation_Result;
   procedure Submit is new CCL.Sessions.Submit_With_Values (V.Context, V.Invoke);
   type Text is record
      Data : String (1 .. 96) := [others => ' '];
      Length : Natural range 0 .. 96 := 0;
   end record;
   Unit_Text : Text;
   type Display_Row is record
      Name, Samples, Latency : Text;
   end record;
   type Display_Rows is array (P.Row_Index) of Display_Row;
   Cached, Staged : Display_Rows;
   Incoming_Count : P.Row_Count := 0;
   Incoming_Next : Observatory_Metric_Queries.Cursor := 0;
   Incoming_Lossy, Incoming_Saturated : Boolean := False;
   Count : P.Row_Count := 0;
   Rows : P.Summary_Page;
   Cursor, Next : Observatory_Metric_Queries.Cursor := 0;
   Sequence, Token, Next_Refresh, Now : Unsigned_64 := 0;
   Refreshes : Natural := 0;
   Busy, Paused, Faulted, Lossy, Saturated : Boolean := False;
   Dirty, Running : Boolean := True;
   OK, Found, Consumed, Healthy : Boolean;
   Repair : UI.Rect;
   Event : A.Input_Event;
   Receipt : aliased CompletionEntry;
   Activity : Activity_Result;
   function Add (Value, Delta_Time : Unsigned_64) return Unsigned_64 is
     (if Value > Unsigned_64'Last - Delta_Time then Unsigned_64'Last
      else Value + Delta_Time);
   function Micros (Value : Unsigned_64) return Unsigned_64 is
     (if Value > Unsigned_64'Last / 1000 then Unsigned_64'Last else Value * 1000);
   function Decimal (Value : Unsigned_64) return String is
      S : constant String := Value'Image;
   begin return S (S'First + 1 .. S'Last); end Decimal;
   function Image (Value : Text) return String is (Value.Data (1 .. Value.Length));
   procedure Disable is
   begin
      if not Faulted then debugPrint ("observatory: collection unavailable" & ASCII.LF); end if;
      Faulted := True; Busy := False; Dirty := True; O.Close; V.Clear (View); F.Cancel (Formatting);
      -- Retain the visible values, explicitly labelled stale; never interpret
      -- them as a new sample after an uncertain grant or malformed reply.
   end Disable;
   function Evaluate (Source : String) return Text is
      Result : Text;
   begin
      Submit (Session, Source, 4096, Grants, View, Outcome);
      if Outcome.Status /= CCL.Language.Succeeded then Disable; return Result; end if;
      declare
         S : constant String := CCL.Sessions.Result_Image (Outcome);
      begin
         if S'Length < 8 or else S (S'First .. S'First + 7) /= "String: " or else
           S'Length - 8 > Result.Data'Length
         then Disable; return Result; end if;
         Result.Length := S'Length - 8;
         Result.Data (1 .. Result.Length) := S (S'First + 8 .. S'Last);
      end;
      return Result;
   end Evaluate;
   procedure Commit_View is
      Chosen : constant P.Row_Index := (if Selected < Incoming_Count then Selected else 0);
      New_Unit : Text;
   begin
      if Faulted then return; end if;
      if Incoming_Count > 0 then
         New_Unit := Evaluate ("(metrics.unit " & Decimal (Unsigned_64 (Chosen)) & ")");
         if Faulted then return; end if;
      end if;
      -- The old visible page stays intact until every new field is formatted.
      Cached := Staged; Count := Incoming_Count; Next := Incoming_Next;
      Lossy := Incoming_Lossy; Saturated := Incoming_Saturated;
      Selected := Chosen; Unit_Text := New_Unit;
      if Count > 0 then
         H.Observe (History,
           (Rows (Selected) (P.Row_Source), Rows (Selected) (P.Row_Publisher_Tag),
            Rows (Selected) (P.Row_Key), Rows (Selected) (P.Row_Kind), Rows (Selected) (P.Row_Unit)),
           Rows (Selected) (P.Row_Count_Word), Rows (Selected) (P.Row_P99), Lossy or Saturated);
      else H.Clear (History); end if;
      Busy := False; Next_Refresh := Add (Now, 1000); Dirty := True;
      if Refreshes < Natural'Last then Refreshes := Refreshes + 1; end if;
      debugPrint ("observatory: page ready rows=" & Count'Image &
        " refresh=" & Refreshes'Image & ASCII.LF);
   end Commit_View;
   procedure Begin_Update is
   begin
      O.Take (Rows, Incoming_Count, Incoming_Next, OK);
      if not OK then Disable; return; end if;
      V.Replace (View, Rows, Incoming_Count, OK);
      if not OK then Disable; return; end if;
      Incoming_Lossy := False; Incoming_Saturated := False;
      F.Start (Formatting, Incoming_Count);
      if not F.Active (Formatting) then Commit_View; end if;
   end Begin_Update;
   procedure Format_One is
      I : constant P.Row_Index := F.Current (Formatting);
      Index : constant String := Decimal (Unsigned_64 (I));
   begin
      Staged (I).Name := Evaluate ("(metrics.name " & Index & ")");
      if Faulted then return; end if;
      Staged (I).Samples := Evaluate ("(metrics.samples " & Index & ")");
      if Faulted then return; end if;
      Staged (I).Latency := Evaluate
        ("(concat (metrics.p99-upper " & Index & ") (concat "" "" (metrics.unit " & Index & ")))" );
      if Faulted then return; end if;
      Incoming_Lossy := Incoming_Lossy or (Rows (I) (P.Row_Series_Rejected) or
        Rows (I) (P.Row_Source_Rejected) or Rows (I) (P.Row_Source_Batch_Gaps) or
        Rows (I) (P.Row_Source_Producer_Dropped)) /= 0;
      Incoming_Saturated := Incoming_Saturated or Rows (I) (P.Row_Flags) /= 0;
      F.Advance (Formatting);
      if not F.Active (Formatting) then Commit_View; end if;
   end Format_One;
   procedure Handle is
   begin
      A.Begin_Input_Event (Win, Event);
      if Event.kind = A.INPUT_CONFIGURE then Dirty := True;
      elsif Event.kind = A.INPUT_KEY_DOWN then
         case Event.payload0 is
            when A.KEY_ESC | A.KEY_Q => Running := False;
            when 16#39# =>
               Paused := not Paused; H.Break_Continuity (History); Next_Refresh := Now; Dirty := True;
               debugPrint ("observatory: paused=" & Paused'Image & ASCII.LF);
            when A.KEY_R => Next_Refresh := Now;
            when 16#14# => Graphs := not Graphs; Dirty := True;
            when 16#48# | 16#50# =>
               if Count > 0 and not Faulted then
                  if Event.payload0 = 16#50# then
                     Selected := (if Selected + 1 < Count then Selected + 1 else 0);
                  else Selected := (if Selected = 0 then Count - 1 else Selected - 1); end if;
                  H.Clear (History); Next_Refresh := Now; Dirty := True;
               end if;
            when 16#31# =>
               if not Busy then
                  Cursor := (if Next = 512 then 0 else Next);
                  Count := 0; Selected := 0; H.Clear (History); V.Clear (View); Next_Refresh := Now; Dirty := True;
               end if;
            when others => null;
         end case;
      end if;
      A.Finish_Input_Event (Win, Event);
   end Handle;
   procedure Paint is
      C : constant UI.Canvas := A.Canvas (Win, Repair);
      Theme : constant UI.Theme := UI.Current_Theme;
      Layout : constant UI.Table_Column_Layout := (340, 140, 8);
      Footer : constant Natural := (if A.Height (Win) > 56 then A.Height (Win) - 56 else 0);
   begin
      UI.Fill_Rect (C, A.Full_Rect (Win), Theme.face);
      UI.Draw_UI_Text (C, 16, 14, "Desktop metrics", Theme.text, Theme.face);
      UI.Draw_UI_Text (C, 16, 38,
        "Collector summaries | p99 is a histogram upper bound", Theme.muted, Theme.face);
      UI.Draw_Table_Header (C, (16, 66, A.Width (Win) - 32, 26), Theme,
        "Metric", "Samples", "p99 upper", Layout);
      if Graphs and Count > 0 then
         UI.Draw_Table_Row (C, (16, 94, A.Width (Win) - 32, 24), Theme, False, False,
           Image (Cached (Selected).Name), Image (Cached (Selected).Samples),
           Image (Cached (Selected).Latency), Layout);
         declare
            procedure Plot (Top : Natural; Latency : Boolean; Title : String) is
               Maximum : Unsigned_64 := 0;
               Value : Unsigned_64;
               Point : H.Sample;
               Valid : Boolean;
               Height : H.Pixel_Height;
               Plot_Height : constant H.Positive_Height := 110;
               Plot_Left : constant Natural := 100;
               Step : constant Natural := 10;
            begin
               for I in 0 .. H.Length (History) - 1 loop
                  Point := H.Sample_At (History, I);
                  Valid := (if Latency then Point.Has_Latency else Point.Has_Delta);
                  Value := (if Latency then Point.Upper else Point.Added);
                  if Valid then Maximum := Unsigned_64'Max (Maximum, Value); end if;
               end loop;
               UI.Draw_UI_Text (C, 16, Top, Title & " | max " & Decimal (Maximum), Theme.text, Theme.face);
               UI.Fill_Rect (C, (Plot_Left, Top + 24, 640, Plot_Height), Theme.field);
               UI.Draw_UI_Text (C, 16, Top + 24 + Plot_Height - UI.UI_Text_Height,
                 "0", Theme.muted, Theme.face);
               for I in 0 .. H.Length (History) - 1 loop
                  Point := H.Sample_At (History, I);
                  Valid := (if Latency then Point.Has_Latency else Point.Has_Delta);
                  Value := (if Latency then Point.Upper else Point.Added);
                  if Valid then
                     Height := H.Scale (Value, Maximum, Plot_Height);
                     if Height > 0 then
                        UI.Fill_Rect (C, (Plot_Left + Natural (I) * Step,
                          Top + 24 + Plot_Height - Height, Step - 2, Height),
                          (if Point.Lossy then Theme.danger
                           elsif Latency then Theme.accent else Theme.good));
                     end if;
                  end if;
               end loop;
            end Plot;
         begin
            Plot (136, True, "Cumulative p99 upper bound (" & Image (Unit_Text) & ")");
            Plot (302, False, "New samples per refresh (gaps after pause)");
            UI.Draw_UI_Text (C, 100, 446, "Oldest to newest | Up to 64 observations", Theme.muted, Theme.face);
         end;
      else
         for I in 0 .. Count - 1 loop
            exit when 94 + Natural (I) * 24 + 24 > Footer;
            UI.Draw_Table_Row (C, (16, 94 + Natural (I) * 24, A.Width (Win) - 32, 24),
              Theme, I = Selected, False, Image (Cached (I).Name), Image (Cached (I).Samples),
              Image (Cached (I).Latency), Layout);
         end loop;
      end if;
      if Count = 0 then
         UI.Draw_UI_Text (C, 24, 106, "No published series on this page.", Theme.muted, Theme.face);
      end if;
      UI.Draw_UI_Text (C, 16, Footer,
        (if Faulted then "Unavailable - displayed values are stale"
         elsif Paused then "Paused - displayed values are retained"
         elsif Lossy then "Live - collection loss detected"
         elsif Saturated then "Live - a counter or histogram has saturated"
         else "Live - refreshing once per second") & " | Cursor " & Cursor'Image,
        (if Faulted or Lossy or Saturated then Theme.danger else Theme.muted), Theme.face);
      UI.Draw_UI_Text (C, 16, Footer + 24,
        "Space pause | T table/graphs | Up/Down series | N page | Esc close", Theme.text, Theme.face);
   end Paint;
begin
   declare Error : CCL.Catalog.Catalog_Error;
      Resolved : CCL.Catalog.Resolved_Operation;
      Grant : CCL.Catalog.Grant_Result;
   begin
      CCL.Catalog.Initialize (Catalog); CCL.Catalog.Initialize (Grants);
      V.Publish (Catalog, Error);
      if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      CCL.Sessions.Initialize (Session, Catalog);
      for Op in V.Operation loop
         CCL.Catalog.Resolve (Catalog, "metrics." & V.Name (Op), Resolved, Found);
         if not Found then return; end if;
         CCL.Catalog.Install (Grants, Resolved, V.Binding (Op), Grant);
         if Grant /= CCL.Catalog.Grant_Added then return; end if;
      end loop;
   end;
   A.Open (Win, 760, 560, A.WINDOW_FLAG_DECORATED or A.WINDOW_FLAG_CLOSEABLE or
     A.WINDOW_FLAG_MINIMIZABLE or A.WINDOW_FLAG_FIXED_SIZE, OK,
     title => "Observatory", protected_frames => True);
   if not OK then return; end if;
   debugPrint ("observatory: native window ready" & ASCII.LF);
   while Running and A.Is_Open (Win) loop
      Now := syscall (SYSCALL_GETTIME);
      -- Drain at most 16 receipts, handling input before evaluation/painting.
      for Drain in 1 .. 16 loop
         exit when Poll_Completion (Receipt'Address) = 0;
         A.Complete_Input_Wait (Win, Receipt, Event, Found, Consumed, Healthy);
         if Consumed then
            if not Healthy then Running := False;
            elsif Found then Handle; end if;
         elsif O.Matches (Receipt.token) then O.Collect (Receipt);
         else Disable; end if;
      end loop;
      exit when not Running;
      O.Tick (Micros (Now));
      if O.Disabled and not Faulted then Disable; end if;
      if O.Ready then Begin_Update; end if;
      if F.Active (Formatting) then Format_One; end if;
      if not Busy and not Paused and not Faulted and Now >= Next_Refresh then
         O.Begin_Query (Cursor, Sequence, Micros (Now), OK);
         if OK then Busy := True; else Disable; end if;
      end if;
      if Dirty or A.Frame_Pending (Win) then
         A.Begin_Paint (Win, (if Dirty then A.Full_Rect (Win) else (0, 0, 0, 0)), Repair, OK);
         if OK then
            Paint;
            A.Present (Win, Repair); Dirty := False;
         end if;
      end if;
      if not A.Input_Wait_Pending (Win) then
         Compositor_Requests.Allocate (Sequence, Token);
         if Token = 0 then exit; end if;
         A.Submit_Input_Wait (Win, Token, OK);
         if not OK then exit; end if;
      end if;
      -- Only outstanding grant/frame work uses a short retry deadline. Idle
      -- and paused windows sleep until input or the next refresh, not frames.
      Activity := Wait_For_Activity_Until
        ((if F.Active (Formatting) then Now
          elsif Dirty or Busy or O.Cleanup_Pending or A.Frame_Pending (Win) then Add (Now, 10)
          elsif Paused or Faulted then Unsigned_64'Last else Next_Refresh));
      exit when Activity = Unavailable;
   end loop;
   O.Close; V.Clear (View); F.Cancel (Formatting); A.Close (Win);
   debugPrint ("observatory: closed" & ASCII.LF);
end Main;
