with CCL.Host_Values;
with CCL.Call_Context;
with CuBit.UI.Widgets;

package body CCL_REPL_View is
   use CuBit.UI;
   Entry_Height : constant := 48;
   function Type_Name (Kind : CCL.Host_Values.Value_Kind) return String is
     (case Kind is when CCL.Host_Values.Integer_Value => "Integer",
                   when CCL.Host_Values.Boolean_Value => "Boolean",
                   when CCL.Host_Values.Text_Value => "String",
                   when CCL.Host_Values.Handler_Value => "Handler() -> Boolean");
   type Geometry is record
      Input, Transcript, Clear : Rect;
      Capacity : Positive := 1;
   end record;
   function Layout (Bounds : Rect) return Geometry is
      G : Geometry;
   begin
      if Bounds.w < 120 or else Bounds.h < 140 then return G; end if;
      G.Clear := (Bounds.x + Bounds.w - 70, Bounds.y, 70, 24);
      G.Input := (Bounds.x, Bounds.y + Bounds.h - 27, Bounds.w, 26);
      G.Transcript := (Bounds.x, Bounds.y + 52, Bounds.w, Bounds.h - 86);
      G.Capacity := Positive'Max (1, G.Transcript.h / Entry_Height);
      return G;
   end Layout;
   procedure Initialize
     (State : out View_State; Catalog : CCL.Catalog.Interface_Catalog)
   is
      Accepted : Boolean;
   begin
      State := (others => <>);
      CCL.Sessions.Initialize (State.Session, Catalog);
      State.Visible_Interfaces := CCL.Catalog.Length (Catalog);
      CuBit.UI.Editor.Initialize (State.Input, "", Accepted);
   end Initialize;

   procedure Deactivate (State : in out View_State) is
   begin
      State.Capture := No_Capture;
   end Deactivate;

   procedure Handle_With_Executor
     (State : in out View_State; Event : View_Event; Bounds : CuBit.UI.Rect;
      Submitted : out Boolean)
   is
      G : constant Geometry := Layout (Bounds);
      Changed, Found : Boolean;
      Entry_Value : CCL.Sessions.Submission;
      Outcome : CCL.Language.Interpretation_Result;
      Count : constant CCL.Sessions.History_Count := CCL.Sessions.Length (State.Session);
      Maximum : constant Natural := Natural'Max
        (1, Count - Natural'Min (Count, G.Capacity) + 1);
      procedure Recall is
      begin
         CCL.Sessions.Recall (State.Session, State.Recalled, Entry_Value, Found);
         if Found and then not Entry_Value.Source_Truncated then
            CuBit.UI.Editor.Initialize
              (State.Input, Entry_Value.Source (1 .. Entry_Value.Source_Length), Changed);
         end if;
      end Recall;
      procedure Place_Cursor (Extend : Boolean) is
         Text : constant String := CuBit.UI.Editor.Content (State.Input);
         X : Natural := G.Input.x + 8;
         Position : CuBit.UI.Editor.Text_Position := State.First_Character;
      begin
         for I in State.First_Character .. Text'Last loop
            exit when Event.X < X + UI_Text_Width (Text (I .. I)) / 2;
            X := X + UI_Text_Width (Text (I .. I));
            Position := Position + 1;
         end loop;
         CuBit.UI.Editor.Place_Cursor (State.Input, Position, Extend);
      end Place_Cursor;
      procedure Refresh_Completion (Explain : Boolean := False) is
         use CCL.Catalog.Completion;
         Text : constant String := CuBit.UI.Editor.Content (State.Input);
         Cursor : constant Positive := CuBit.UI.Editor.Cursor (State.Input);
         Context : CCL.Call_Context.Context;
         Matches : Match_List;
         function Name_Character (C : Character) return Boolean is
           (C in 'a' .. 'z' | '0' .. '9' | '-' | '.');
         procedure Hint (Text : String) is
         begin
            if not Explain then return; end if;
            State.Completion_Hint_Length := Natural'Min
              (Text'Length, State.Completion_Hint'Length);
            State.Completion_Hint (1 .. State.Completion_Hint_Length) :=
              Text (Text'First .. Text'First + State.Completion_Hint_Length - 1);
         end Hint;
      begin
         State.Suggested_Length := 0;
         State.Signature_Visible := False;
         if CuBit.UI.Editor.Selection_First (State.Input) /=
           CuBit.UI.Editor.Selection_Last (State.Input)
         then return; end if;
         CCL.Call_Context.Inspect (Text, Cursor - 1, Context);
         if not Context.Available then
            Hint ("Ctrl+Space: complete an operation after '('");
            return;
         end if;
         if not Context.Arguments_Started and then Cursor <= Text'Length and then
           Name_Character (Text (Cursor))
         then return; end if;
         if Context.Arguments_Started then
            Matches := (others => <>);
            CCL.Sessions.Describe
              (State.Session, Context.Name (1 .. Context.Length),
               Matches.Items (1).Contract, Found);
            if Found then
               Matches.Items (1).Name := Context.Name;
               Matches.Items (1).Length := Context.Length;
               Matches.Count := 1;
               Matches.Total := 1;
            end if;
         else
            CCL.Sessions.Complete (State.Session, Context.Name (1 .. Context.Length), Matches);
         end if;
         if Matches.Total = 0 then
            Hint ("No matching operation in this session's visible catalog");
         else
            declare
               S : Suggestion renames Matches.Items (1);
               Name : constant String := S.Name (1 .. S.Length);
            begin
               if Matches.Total = 1 then
                  State.Signature := S;
                  State.Signature_Visible := True;
                  State.Signature_Arguments := Context.Arguments_Started;
                  if not Context.Arguments_Started and then S.Length - Context.Length >
                    CuBit.UI.Editor.MAX_TEXT_LENGTH - Text'Length
                  then
                     Hint ("Not enough input space for the complete operation name");
                     return;
                  end if;
                  --  Inline suggestions are only shown at the end of input;
                  --  never cover or displace existing source after the caret.
                  if not Context.Arguments_Started and then Cursor = Text'Length + 1 then
                     State.Suggested_Length := S.Length - Context.Length;
                     State.Suggested_Suffix (1 .. State.Suggested_Length) :=
                       Name (Context.Length + 1 .. Name'Last);
                  end if;
               end if;
               Hint (Name & "(" &
                 (if S.Contract.Parameters = 0 then "" else Type_Name (S.Contract.Import.Argument)) &
                 ") -> " & Type_Name (S.Contract.Import.Result) &
                 (if Matches.Total = 1 then " | invocation requires an explicit grant"
                  else " |" & Matches.Total'Image & " matches; refine prefix"));
            end;
         end if;
      end Refresh_Completion;
      procedure Accept_Suggestion is
      begin
         if State.Suggested_Length > 0 then
            CuBit.UI.Editor.Insert
              (State.Input, State.Suggested_Suffix (1 .. State.Suggested_Length), Changed);
            if Changed then State.Recalled := 0; end if;
         end if;
      end Accept_Suggestion;
   begin
      Submitted := False;
      if Event.Kind /= Complete and Event.Kind /= No_Event then
         State.Completion_Hint_Length := 0;
      end if;
      case Event.Kind is
         when Complete => Refresh_Completion (Explain => True);
         when Accept_Completion =>
            if not Event.Shift and not Event.Control then
               Refresh_Completion;
               Accept_Suggestion;
            end if;
         when Text_Input | Backspace | Delete =>
            State.Recalled := 0;
            case Event.Kind is
               when Text_Input =>
                  if Event.Character_Value in ' ' .. '~' then
                     CuBit.UI.Editor.Insert
                       (State.Input, String'(1 => Event.Character_Value), Changed);
                  end if;
               when Backspace => CuBit.UI.Editor.Backspace (State.Input, Changed);
               when Delete => CuBit.UI.Editor.Delete_Forward (State.Input, Changed);
               when others => null;
            end case;
         when Left | Right | Home | End_Key =>
            CuBit.UI.Editor.Move (State.Input,
              (case Event.Kind is
                  when Left => (if Event.Control then CuBit.UI.Editor.Move_Word_Left else CuBit.UI.Editor.Move_Left),
                  when Right => (if Event.Control then CuBit.UI.Editor.Move_Word_Right else CuBit.UI.Editor.Move_Right),
                  when Home => CuBit.UI.Editor.Move_Start,
                  when others => CuBit.UI.Editor.Move_End), Event.Shift);
         when Select_All => CuBit.UI.Editor.Select_All (State.Input);
         when Submit =>
            if CuBit.UI.Editor.Length (State.Input) > 0 then
               Execute (State.Session, CuBit.UI.Editor.Content (State.Input),
                 CCL.Sessions.Default_Fuel, Outcome);
               Submitted := True;
               CuBit.UI.Editor.Initialize (State.Input, "", Changed);
               State.Recalled := 0;
               State.First_Character := 1;
               State.Follow_Latest := True;
            end if;
         when Previous =>
            if Count > 0 then
               if State.Recalled = 0 then
                  State.Draft := State.Input;
                  State.Recalled := Count;
               elsif State.Recalled > 1 then State.Recalled := State.Recalled - 1;
               end if;
               Recall;
            end if;
         when Next =>
            if State.Recalled > 0 then
               if State.Recalled < Count then
                  State.Recalled := State.Recalled + 1;
                  Recall;
               else
                  State.Recalled := 0;
                  State.Input := State.Draft;
               end if;
            end if;
         when Wheel_Up | Wheel_Down =>
            State.Follow_Latest := False;
            Apply_Wheel_Scroll (State.First_Entry, 1, Maximum,
              (if Event.Kind = Wheel_Up then 1 else -1), 1);
         when Pointer_Down =>
            State.Capture := No_Capture;
            if Point_In_Rect (Event.X, Event.Y, G.Clear) then
               State.Capture := Clear_Capture;
            elsif Point_In_Rect (Event.X, Event.Y, G.Input) then
               State.Capture := Input_Capture;
               Place_Cursor (Event.Shift);
            end if;
         when Pointer_Drag =>
            if State.Capture = Input_Capture then Place_Cursor (True); end if;
         when Pointer_Up =>
            if State.Capture = Clear_Capture and then Point_In_Rect (Event.X, Event.Y, G.Clear) then
               CCL.Sessions.Clear_History (State.Session);
               State.Recalled := 0;
               State.First_Entry := 1;
               State.Follow_Latest := True;
            end if;
            State.Capture := No_Capture;
         when others => null;
      end case;
      --  Query only when editing/navigation changes the prefix, never during
      --  paint, pointer hover, or transcript scrolling. This is read-only data.
      if Event.Kind in Text_Input | Backspace | Delete | Left | Right | Home |
        End_Key | Select_All | Accept_Completion | Submit | Previous | Next |
        Pointer_Down | Pointer_Drag | Pointer_Up
      then
         Refresh_Completion;
      end if;
   end Handle_With_Executor;

   procedure Handle_Pure is new Handle_With_Executor (CCL.Sessions.Submit);
   procedure Handle
     (State : in out View_State; Event : View_Event; Bounds : CuBit.UI.Rect;
      Submitted : out Boolean) is
   begin
      Handle_Pure (State, Event, Bounds, Submitted);
   end Handle;

   function One_Line (Text : String) return String is
      Result : String := Text;
   begin
      for C of Result loop
         if C not in ' ' .. '~' then C := ' '; end if;
      end loop;
      return Result;
   end One_Line;

   procedure Draw
     (State : in out View_State; Canvas : CuBit.UI.Canvas;
      Bounds : CuBit.UI.Rect; Colors : CuBit.UI.Theme)
   is
      G : constant Geometry := Layout (Bounds);
      C : constant CuBit.UI.Canvas := With_Clip (Canvas, Bounds);
      Count : constant CCL.Sessions.History_Count := CCL.Sessions.Length (State.Session);
      Maximum : constant Natural := Natural'Max
        (1, Count - Natural'Min (Count, G.Capacity) + 1);
      Entry_Value : CCL.Sessions.Submission;
      Found : Boolean;
      Text : constant String := CuBit.UI.Editor.Content (State.Input);
      Cursor : constant Positive := CuBit.UI.Editor.Cursor (State.Input);
      Used : Natural := 0;
   begin
      if Is_Empty (G.Input) then return; end if;
      Fill_Rect (C, Bounds, Colors.face);
      CuBit.UI.Widgets.Label (C, (Bounds.x, Bounds.y, Bounds.w - 78, 24), Colors,
        "REPL |" & Natural'Image (Count) & "/16 | fuel 4096");
      Draw_Button (C, G.Clear, Colors,
        (if State.Capture = Clear_Capture then Button_Pressed else Button_Normal), "Clear");
      CuBit.UI.Widgets.Label (C, (Bounds.x, Bounds.y + 27, Bounds.w, 20), Colors,
        (if State.Completion_Hint_Length > 0 then
           State.Completion_Hint (1 .. State.Completion_Hint_Length)
         else "Catalog:" & Natural'Image (Natural (State.Visible_Interfaces)) &
          " visible | Tab: accept | Ctrl+Space: inspect"));
      Fill_Rect (C, G.Transcript, Colors.field);
      Stroke_Rect (C, G.Transcript, Colors.shadow, Colors.edge);
      if State.Follow_Latest then State.First_Entry := Maximum;
      else State.First_Entry := Natural'Min (State.First_Entry, Maximum);
      end if;
      if Count = 0 then
         Draw_UI_Text_Transparent (With_Clip (C, G.Transcript), G.Transcript.x + 6, G.Transcript.y + 6,
           "Try (+ 20 22), then Enter", Colors.muted);
         Draw_UI_Text_Transparent (With_Clip (C, G.Transcript), G.Transcript.x + 6, G.Transcript.y + 28,
           "Up/Down recall; wheel reviews history", Colors.muted);
      else
         for Row in 0 .. G.Capacity - 1 loop
            exit when State.First_Entry + Row > Count;
            CCL.Sessions.Recall (State.Session, State.First_Entry + Row, Entry_Value, Found);
            if Found then
               declare
                  Y : constant Natural := G.Transcript.y + Row * Entry_Height + 4;
                  TC : constant CuBit.UI.Canvas := With_Clip (C,
                    (G.Transcript.x + 3, Y, G.Transcript.w - 6, Entry_Height - 4));
               begin
                  Draw_Code_Text (TC, G.Transcript.x + 6, Y,
                    "> " & One_Line (Entry_Value.Source (1 .. Entry_Value.Source_Length)),
                    Colors.muted, Colors.field);
                  Draw_UI_Text_Transparent (TC, G.Transcript.x + 6, Y + 21,
                    One_Line (CCL.Sessions.Result_Image (Entry_Value.Outcome)), Colors.text);
               end;
            end if;
         end loop;
      end if;
      --  Follow the caret horizontally using a bounded linear scan, not a
      --  repeated measure of longer and longer prefixes.
      State.First_Character := Cursor;
      for I in reverse 1 .. Cursor - 1 loop
         Used := Used + UI_Text_Width (Text (I .. I));
         exit when Used > G.Input.w - 20;
         State.First_Character := I;
      end loop;
      Draw_Text_Edit_Field (C, G.Input, Colors, Text (State.First_Character .. Text'Last),
        Cursor - State.First_Character,
        Natural'Max (State.First_Character, CuBit.UI.Editor.Selection_First (State.Input)) - State.First_Character,
        Natural'Max (State.First_Character, CuBit.UI.Editor.Selection_Last (State.Input)) - State.First_Character,
        True, False,
        Suggestion => State.Suggested_Suffix (1 .. State.Suggested_Length));
      if State.Signature_Visible then
         declare
            S : CCL.Catalog.Completion.Suggestion renames State.Signature;
            Head : constant String := S.Name (1 .. S.Length) & "(";
            Argument : constant String :=
              (if S.Contract.Parameters = 0 then "" else Type_Name (S.Contract.Import.Argument));
            Tail : constant String := ") -> " & Type_Name (S.Contract.Import.Result);
            Detail : constant String :=
              (if State.Signature_Arguments then
                 (if S.Contract.Parameters = 0 then "No arguments expected"
                  else "Argument 1 expects " & Argument)
               elsif State.Suggested_Length > 0 then "Tab accepts the suggested name"
               else "Advertised signature; not an execution grant");
            W : constant Natural := Natural'Min (G.Input.w,
              Natural'Max (UI_Text_Width (Head & Argument & Tail),
                           UI_Text_Width (Detail)) + 16);
            H : constant Natural := Natural'Min
              (UI_Text_Height * 2 + 16, G.Input.y - Bounds.y - 4);
            Caret_X : constant Natural := G.Input.x + 8 +
              UI_Text_Width (Text (State.First_Character .. Cursor - 1));
            Popup : constant Rect :=
              (Natural'Min (Caret_X, G.Input.x + G.Input.w - W),
               G.Input.y - H - 4, W, H);
            PC : constant CuBit.UI.Canvas := With_Clip (C, Popup);
            Text_X : constant Natural := Popup.x + 8;
            Text_Y : constant Natural := Popup.y + 5;
            Inner : constant CuBit.UI.Canvas := With_Clip
              (PC, (Popup.x + 4, Popup.y + 3, Popup.w - 8, Popup.h - 6));
         begin
            Fill_Rect (PC, Popup, Colors.face);
            Stroke_Rect (PC, Popup, Colors.highlight, Colors.shadow);
            if State.Signature_Arguments and then S.Contract.Parameters > 0 then
               Fill_Rect (Inner,
                 (Text_X + UI_Text_Width (Head), Text_Y,
                  UI_Text_Width (Argument), UI_Text_Height), Colors.selection);
            end if;
            Draw_UI_Text_Transparent (Inner, Text_X, Text_Y, Head, Colors.text);
            Draw_UI_Text_Transparent (Inner, Text_X + UI_Text_Width (Head), Text_Y,
              Argument, (if State.Signature_Arguments then Colors.selectionText else Colors.text));
            Draw_UI_Text_Transparent (Inner, Text_X + UI_Text_Width (Head & Argument),
              Text_Y, Tail, Colors.text);
            Draw_UI_Text_Transparent (Inner, Text_X, Text_Y + UI_Text_Height + 5,
              Detail, Colors.muted);
         end;
      end if;
   end Draw;
end CCL_REPL_View;
