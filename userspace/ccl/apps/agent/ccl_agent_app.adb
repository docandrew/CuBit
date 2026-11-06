with Interfaces; use Interfaces;
with System;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Sessions;
with CCL.Streams;
with CCL_Agent_Bindings;
with CCL_Desktop_Platform;
with CCL_Host_Environment;
with CCL_Log_IO;
with CCL_Places;
with CCL_Secrets;
with CCL_Window;
with CCL_Execution;
with Client_Input_Budget;
with CuBit.UI;
with Agent_App_View;

package body CCL_Agent_App is
   package Platform renames CCL_Desktop_Platform;
   package View renames Agent_App_View;
   use type System.Address;
   use type Platform.Window_Event;
   use type View.Action;
   use type View.Input_Mode;
   use type CCL_Secrets.Store_Result;
   use type CCL_Secrets.Call_Result;
   use type CCL.Language.Interpretation_Status;

   WIDTH  : constant := 960;
   HEIGHT : constant := 640;
   MAXIMUM_WIDTH  : constant := 1_920;
   MAXIMUM_HEIGHT : constant := 1_200;

   --  The secret the startup plan grants llm.svc the use of (its
   --  api_key_secret parameter): the app stores it, never reads it back.
   KEY_SECRET : constant String := "anthropic-api-key";
   --  Every ask continues this recorded session (in the app's workspace,
   --  when it has one), each as a new mission with its own budgets.
   SESSION_NAME : constant String := "agent-app";

   --  Each mission's budgets, spelled out (no hidden defaults): the
   --  Diagnose profile's model, a few dollars, half an hour.
   MISSION_TOKENS : constant := 3_000_000;
   MISSION_COST_NANO_USD : constant := 5_000_000_000;
   MISSION_WALL_MS : constant := 1_800_000;
   MISSION_TOOL_CALLS : constant := 100;
   REQUEST_MAX_TOKENS : constant := 16_000;
   REQUEST_DEADLINE_MS : constant := 300_000;
   --  How often a running mission's panel is refreshed.
   REFRESH_MS : constant := CCL_Agent_Bindings.Poll_Ms;
   TRANSCRIPT_PIECE : constant := 4_096;

   function Live_Clock (Available : out Boolean) return Unsigned_64 is
      Answered : aliased Integer_32 := 0;
      Milliseconds : constant Unsigned_64 := CCL_Window.Clock_Monotonic (Answered'Access);
   begin
      Available := Answered /= 0;
      return Milliseconds;
   end Live_Clock;
   package Host is new CCL_Host_Environment (Live_Clock);

   Visible_Interfaces : CCL.Catalog.Interface_Catalog;
   Granted_Interfaces : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   State : View.View_State;

   type Live_Context is null record;
   Live_Host : Live_Context;
   procedure Invoke_Live
     (Context : in out Live_Context; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      Host.Invoke (Binding, Argument, Reply);
   end Invoke_Live;
   procedure Read_Live
     (Context : in out Live_Context; Request : CCL.Streams.View_Request;
      Reply : in out CCL.Streams.View_Reply)
   is
      pragma Unreferenced (Context);
   begin
      Host.Read_Stream (Request, Reply);
   end Read_Live;
   procedure Submit is new CCL.Sessions.Submit_With_Values (Live_Context, Invoke_Live, Read_Stream => Read_Live);

   --  The mission on screen, and how much of its transcript was taken.
   Current : Unsigned_64 := 0;
   Taken : Natural := 0;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --  A CCL string literal's inside.
   function Quoted (Text : String) return String is
   begin
      for K in Text'Range loop
         if Text (K) = '"' or else Text (K) = '\' then
            return Text (Text'First .. K - 1) & '\' & Text (K) & Quoted (Text (K + 1 .. Text'Last));
         end if;
      end loop;
      return Text;
   end Quoted;

   function Run_Source (Source : String) return CCL.Language.Interpretation_Result is
      Outcome : CCL.Language.Interpretation_Result;
   begin
      Submit (Session, Source, CCL.Sessions.Default_Fuel, Granted_Interfaces, Live_Host, Outcome);
      return Outcome;
   end Run_Source;

   procedure Refresh_Key_Status is
      Stored : Boolean;
      Holder : String (1 .. 64);
      Holder_Length : Natural;
      Result : CCL_Secrets.Call_Result;
   begin
      CCL_Secrets.Status (KEY_SECRET, Stored, Holder, Holder_Length, Result);
      if Result /= CCL_Secrets.Done then
         View.Set_Key_Status (State, False, "secrets.svc is not reachable from this app");
      else
         View.Set_Key_Status
           (State, Stored,
            (if Stored then "held in memory by secrets.svc; enter it again after a reboot"
             else "only llm.svc may use it; this app cannot read it back"));
      end if;
   end Refresh_Key_Status;

   --  The key goes from the masked field to secrets.svc, then the field is
   --  wiped; it is never logged, shown or written anywhere.
   procedure Store_Key is
      Result : CCL_Secrets.Store_Result;
   begin
      CCL_Secrets.Request_Value (KEY_SECRET, Result);
      if Result = CCL_Secrets.Stored then
         CCL_Secrets.Store_Value (View.Key_Text (State), Result);
      end if;
      View.Wipe_Key (State);
      if Result = CCL_Secrets.Stored then
         CCL_Log_IO.Note ("agent-app: API key stored in secrets.svc");
         View.Set_Mode (State, View.Prompt_Mode);
         View.Set_Status (State, "API key stored");
      else
         CCL_Secrets.Cancel_Value;
         CCL_Log_IO.Note ("agent-app: API key not stored: " & CCL_Secrets.Explain (Result));
         View.Set_Status (State, "key not stored: " & CCL_Secrets.Explain (Result));
      end if;
      Refresh_Key_Status;
   end Store_Key;

   procedure Ask is
      Prompt : constant String := View.Prompt (State);
      Home : constant String := CCL_Places.Home;
      Source : constant String :=
        "(agent.ask (Agent_Request mission => Mission.Diagnose prompt => """ & Quoted (Prompt) &
        """ model => Agent_Model.Profile budget => (Agent_Budget tokens =>" & Natural'Image (MISSION_TOKENS) &
        " cost_nano_usd =>" & Long_Long_Integer'Image (MISSION_COST_NANO_USD) &
        " wall_ms =>" & Natural'Image (MISSION_WALL_MS) & " tool_calls =>" & Natural'Image (MISSION_TOOL_CALLS) &
        " request_max_tokens =>" & Natural'Image (REQUEST_MAX_TOKENS) &
        " request_deadline_ms =>" & Natural'Image (REQUEST_DEADLINE_MS) & ") places => [" &
        (if Home'Length > 0 then """" & Quoted (Home) & """" else "") & "] session => """ & SESSION_NAME & """))";
      Outcome : constant CCL.Language.Interpretation_Result := Run_Source (Source);
      Started : CCL_Agent_Bindings.Started_Array;
      Count : Natural;
   begin
      CCL_Agent_Bindings.Take_Started (Started, Count);
      if Count = 0 then
         View.Set_Status (State, "not started: " & CCL.Sessions.Result_Image (Outcome));
         return;
      end if;
      Current := Started (Count);
      Taken := 0;
      View.Append_Transcript (State, (if View.Transcript_Length (State) > 0 then "" & ASCII.LF else "") &
                                "> " & Prompt & ASCII.LF);
      View.Clear_Prompt (State);
      View.Set_Status (State, "");
      CCL_Log_IO.Note ("agent-app: mission " & Image (Current) & " started");
   end Ask;

   procedure Cancel is
      Outcome : CCL.Language.Interpretation_Result;
   begin
      if Current /= 0 then
         Outcome := Run_Source ("(agent.cancel (Agent_Session id => " & Image (Current) & "))");
         if Outcome.Status /= CCL.Language.Succeeded then
            View.Set_Status (State, "cancel: " & CCL.Sessions.Result_Image (Outcome));
         end if;
      end if;
   end Cancel;

   --  The mission's new transcript text and its state; Changed when either moved.
   Last_Running : Boolean := False;
   procedure Follow (Now : Unsigned_64; Changed : out Boolean) is
      Piece : String (1 .. TRANSCRIPT_PIECE);
      Last : Natural;
      Item : CCL_Agent_Bindings.Mission_View;
   begin
      Changed := False;
      if Current = 0 then
         return;
      end if;
      loop
         CCL_Agent_Bindings.Transcript_Text (Current, Taken, Piece, Last);
         exit when Last < Piece'First;
         View.Append_Transcript (State, Piece (1 .. Last));
         Taken := Taken + Last;
         Changed := True;
      end loop;
      Item := CCL_Agent_Bindings.View_Of (Current);
      View.Set_Mission (State, Item, CCL_Agent_Bindings.Granted_Tools (Current), Now);
      if Item.Found and then Item.Running then
         Changed := True;
      end if;
      if Last_Running and then Item.Found and then not Item.Running then
         CCL_Log_IO.Note ("agent-app: mission " & Image (Current) & " ended");
         Changed := True;
      end if;
      Last_Running := Item.Found and then Item.Running;
   end Follow;

   function View_Event_Of
     (Event : Platform.Window_Event; Code, Modifiers : Unsigned_32; X, Y : Integer_32) return View.View_Event
   is
      use View;
      Control : constant Boolean := (Modifiers and Platform.CONTROL_MODIFIER) /= 0;
      Character_Value : constant Character := (if Code <= 127 then Character'Val (Code) else ' ');
      Kind : constant Event_Kind :=
        (case Event is
            when Platform.Text_Input =>
              (if Control and then Character_Value in 'k' | 'K' then Set_Key
               elsif Control then No_Event else Text_Input),
            when Platform.Backspace => Backspace,
            when Platform.Delete => Delete,
            when Platform.Left => Left,
            when Platform.Right => Right,
            when Platform.Home => Home,
            when Platform.End_Key => End_Key,
            when Platform.Enter | Platform.Run_Source => Enter,
            when Platform.Escape => Escape,
            when Platform.Page_Up | Platform.Up => Page_Up,
            when Platform.Page_Down | Platform.Down => Page_Down,
            when Platform.Wheel_Up => Wheel_Up,
            when Platform.Wheel_Down => Wheel_Down,
            when Platform.Pointer_Down => Pointer_Down,
            when Platform.Select_All => Select_All,
            when others => No_Event);
   begin
      return (Kind => Kind, Character_Value => Character_Value,
              X => (if X > 0 then Natural (X) else 0), Y => (if Y > 0 then Natural (Y) else 0));
   end View_Event_Of;

   MAXIMUM_PASTE : constant := 1_024;

   procedure Run is
      Installed : Boolean;
   begin
      CCL_Log_IO.Announce ("agent-app: started");
      Platform.Activate (Name => "agent-app", Title => "Agent");
      CCL.Catalog.Initialize (Visible_Interfaces);
      CCL.Catalog.Initialize (Granted_Interfaces);
      Host.Install (Visible_Interfaces, Granted_Interfaces, Installed);
      if not Installed then raise Program_Error with "invalid CCL host environment"; end if;
      CCL.Sessions.Initialize (Session, Visible_Interfaces);
      View.Initialize (State);
      Refresh_Key_Status;
      declare
         Handle_Window : constant System.Address := CCL_Window.Open (Integer_32 (WIDTH), Integer_32 (HEIGHT));
         Kind : aliased Integer_32 := 0;
         Code : aliased Unsigned_32 := 0;
         Modifiers : aliased Unsigned_32 := 0;
         Mouse_X, Mouse_Y : aliased Integer_32 := 0;
         Surface_Width : aliased Integer_32 := Integer_32 (WIDTH);
         Surface_Height : aliased Integer_32 := Integer_32 (HEIGHT);
         Running : Boolean := Handle_Window /= System.Null_Address;
         Needs_Render : Boolean := True;
         Input_Batch : Client_Input_Budget.Batch;
         Input_Drained : Boolean;
         Canvas : CuBit.UI.Canvas :=
           (addr => System.Null_Address, width => WIDTH, height => HEIGHT,
            pitch => 0, clipEnabled => False, clip => (others => 0), others => <>);
         Next_Refresh : Unsigned_64 := 0;

         procedure Prepare_Surface is
            Old_Width : constant Natural := Canvas.width;
            Old_Height : constant Natural := Canvas.height;
         begin
            if CCL_Window.Prepare_Frame
              (Handle_Window, Integer_32 (WIDTH), Integer_32 (HEIGHT),
               Integer_32 (MAXIMUM_WIDTH), Integer_32 (MAXIMUM_HEIGHT),
               Surface_Width'Access, Surface_Height'Access) /= 0
            then
               Running := False;
            else
               Canvas.width := Natural (Surface_Width);
               Canvas.height := Natural (Surface_Height);
               Needs_Render := Needs_Render or else
                 Canvas.width /= Old_Width or else Canvas.height /= Old_Height;
            end if;
         end Prepare_Surface;

         function Paint return Boolean is
            Repair : CuBit.UI.Rect;
            Ready : Boolean;
         begin
            Platform.Begin_Frame (Canvas, (0, 0, Canvas.width, Canvas.height), Repair, Ready);
            if not Ready then return True; end if;
            Canvas := CuBit.UI.With_Clip (Canvas, Repair);
            View.Draw (State, Canvas, (0, 0, Canvas.width, Canvas.height));
            return Platform.Submit_Frame (Handle_Window, Canvas, Repair);
         end Paint;

         procedure Act (Result : View.Action) is
         begin
            case Result is
               when View.Nothing => null;
               when View.Redraw => Needs_Render := True;
               when View.Ask => Ask; Needs_Render := True;
               when View.Store_Key => Store_Key; Needs_Render := True;
               when View.Cancel_Mission => Cancel; Needs_Render := True;
            end case;
         end Act;
      begin
         --  Without a key the first thing to do is to give one.
         declare
            Stored : Boolean;
            Holder : String (1 .. 64);
            Holder_Length : Natural;
            Result : CCL_Secrets.Call_Result;
         begin
            CCL_Secrets.Status (KEY_SECRET, Stored, Holder, Holder_Length, Result);
            if Result = CCL_Secrets.Done and then not Stored then
               View.Set_Mode (State, View.Key_Mode);
            end if;
         end;
         while Running loop
            Prepare_Surface;
            exit when not Running;
            Input_Batch := Client_Input_Budget.Open (CCL_Window.Ticks);
            Input_Drained := False;
            while Running and then Client_Input_Budget.Can_Poll (Input_Batch, CCL_Window.Ticks) loop
               Client_Input_Budget.Charge (Input_Batch);
               if CCL_Window.Poll
                 (Handle_Window, Kind'Access, Code'Access, Modifiers'Access,
                  Mouse_X'Access, Mouse_Y'Access) = 0
               then
                  Input_Drained := True;
                  exit;
               end if;
               declare
                  Polled : constant Platform.Window_Event := Platform.Event_Of (Kind);
                  Result : View.Action;
               begin
                  if Polled = Platform.Close_Request then
                     Running := False;
                  elsif Polled = Platform.Paste_Clipboard then
                     declare
                        Text : String (1 .. MAXIMUM_PASTE);
                        Last : Natural;
                        Typed, Pasted, Changed : Boolean;
                     begin
                        Platform.Paste (Text, Last, Typed, Pasted);
                        if Pasted then
                           View.Insert_Pasted (State, Text (1 .. Last), Changed);
                           Needs_Render := Needs_Render or else Changed;
                        end if;
                        Text := [others => ' '];
                     end;
                  else
                     View.Handle (State, View_Event_Of (Polled, Code, Modifiers, Mouse_X, Mouse_Y),
                                  (0, 0, Canvas.width, Canvas.height), Result);
                     Act (Result);
                  end if;
                  Platform.Finish_Input;
               end;
            end loop;
            exit when not Running;
            --  The mission runs here: the host pumps the agent loop.
            declare
               Arrived, Changed : Boolean;
               Now : constant Unsigned_64 := CCL_Window.Ticks;
            begin
               Host.Pump_Streams (Arrived);
               if Arrived or else Now >= Next_Refresh then
                  Follow (Now, Changed);
                  Needs_Render := Needs_Render or else Changed;
                  Next_Refresh := Now + REFRESH_MS;
               end if;
            end;
            if Needs_Render or else Platform.Frame_Pending then
               exit when not Paint;
               Needs_Render := False;
            end if;
            if not Input_Drained then
               Platform.Yield_Input;
            else
               declare
                  Stream_Delay : constant Unsigned_64 := Host.Next_Stream_Delay;
                  Ticks : constant Unsigned_64 := CCL_Window.Ticks;
                  Next : constant Unsigned_64 := Unsigned_64'Min
                    ((if Last_Running then Next_Refresh else Unsigned_64'Last),
                     (if Stream_Delay > Unsigned_64'Last - Ticks then Unsigned_64'Last else Ticks + Stream_Delay));
                  Wakeup : constant Unsigned_64 := Platform.Frame_Deadline (Next);
               begin
                  if Wakeup /= Unsigned_64'Last then
                     CCL_Window.Wait_Until (Wakeup);
                  else
                     CCL_Window.Wait (1);
                  end if;
               end;
            end if;
         end loop;
         View.Wipe_Key (State);
         CCL_Execution.Stop;
         CCL_Window.Close (Handle_Window);
      end;
   end Run;
end CCL_Agent_App;
