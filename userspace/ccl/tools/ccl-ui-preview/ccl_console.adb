with CCL_Log_IO;
with Interfaces; use Interfaces;
with System;
with CCL.Catalog;
with CCL.Host_Replay;
with CCL.Host_Values;
with CCL.Language;
with CCL.Sessions;
with CCL.Streams;
with CCL_Console_View;
with CCL_Desktop_Platform;
with CCL_Window;
with CCL_Execution;
with CCL_Host_Environment;
with CCL_Program_Bindings;
with CCL_Console_Bindings;
with CCL.Interfaces.Console;
with CCL_REPL_Commands;
with Client_Input_Budget;
with CuBit.Failures;
with CuBit.UI;

package body CCL_Console is
   package Platform renames CCL_Desktop_Platform;
   use type System.Address;
   use type Platform.Window_Event;
   use type CuBit.UI.Pointer_Cursor_Style;

   WIDTH  : constant := 880;
   HEIGHT : constant := 600;
   MAXIMUM_WIDTH  : constant := 1_280;
   MAXIMUM_HEIGHT : constant := 720;

   --  The window platform's monotonic clock, for clock.monotonic-ms.
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

   Console : CCL_Console_View.View_State;
   Window_Handle : System.Address := System.Null_Address;

   --  console.*: this console, programmable from its own cells.
   procedure Set_Title (Text : String) is
   begin
      if Window_Handle /= System.Null_Address and then Text'Length > 0 then
         CCL_Window.Set_Title (Window_Handle, Text'Address, Integer_32 (Text'Length));
      end if;
   end Set_Title;
   function Notation return CCL.Interfaces.Console.Notation is (CCL_Console_View.Notation (Console));
   procedure Set_Notation (Value : CCL.Interfaces.Console.Notation) is
   begin
      CCL_Console_View.Set_Notation (Console, Value);
   end Set_Notation;
   function Stats return CCL.Interfaces.Console.Statistics is
      Result : CCL.Interfaces.Console.Statistics := CCL_Console_View.Statistics (Console);
   begin
      Result.Streams := Host.Open_Streams;
      return Result;
   end Stats;
   package Endpoints is new CCL_Console_Bindings
     (Set_Title, Notation, Set_Notation, CCL_Console_View.Theme, CCL_Console_View.Set_Theme, Stats);

   --  Everything the console can call: the shared host environment, and
   --  console.* for itself.
   type Live_Context is null record;
   Live_Host : Live_Context;
   procedure Invoke_Live
     (Context : in out Live_Context; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      Reply.Value := CCL.Host_Values.Integer_Constant (0);
      Reply.Success := False;
      if Endpoints.Handles (Binding) then
         Endpoints.Invoke (Binding, Argument, Reply);
      elsif Host.Handles (Binding) then
         Host.Invoke (Binding, Argument, Reply);
      end if;
   end Invoke_Live;
   procedure Read_Live
     (Context : in out Live_Context; Request : CCL.Streams.View_Request;
      Reply : in out CCL.Streams.View_Reply)
   is
      pragma Unreferenced (Context);
   begin
      Host.Read_Stream (Request, Reply);
   end Read_Live;
   --  Every entry's host calls are logged, so one that stops at (wait t)
   --  on a pending task can run again once t completes without making
   --  them twice (CCL.Host_Replay, CCL.Sessions.Resume_With_Values).
   package Replay is new CCL.Host_Replay (Live_Context, Invoke_Live, Read_Live);
   procedure Submit_Live is new CCL.Sessions.Submit_With_Values
     (Replay.Context, Replay.Invoke_Logged, Read_Stream => Replay.Read_Logged);
   procedure Resume_Live is new CCL.Sessions.Resume_With_Values
     (Replay.Context, Replay.Invoke_Logged, Read_Stream => Replay.Read_Logged);
   Fresh : Replay.Context;
   --  Entries waiting on a task: the task, and the calls to answer again.
   MAXIMUM_PENDING_AWAITS : constant := 4;
   type Pending_Index is range 1 .. MAXIMUM_PENDING_AWAITS;
   Pending : array (Pending_Index) of Replay.Context;
   Waited_On : array (Pending_Index) of CCL.Streams.Handle := [others => CCL.Streams.No_Handle];

   function Not_Resumable (Why : String) return CuBit.Failures.Failure is
     (CuBit.Failures.Failed (CuBit.Failures.Exhausted, Why,
        "wait in an entry of its own, or wait for fewer tasks at a time"));

   --  Entry Index stopped waiting on task Handle; its calls are in Calls.
   procedure Keep_Wait
     (Item : in out CCL.Sessions.Session; Handle : CCL.Streams.Handle; Calls : Replay.Log)
   is
      use type CCL.Streams.Handle;
      Index : constant CCL.Sessions.History_Count := CCL.Sessions.Waiting_Entry (Item, Handle);
      Free : Pending_Index'Base := 0;
   begin
      if Index = 0 then return; end if;
      if Replay.Overflowed (Calls) then
         CCL.Sessions.Abandon_Wait
           (Item, Index, Not_Resumable ("the entry made more than" &
              Natural'Image (Replay.MAX_CALLS) & " service calls before it waited"));
         return;
      end if;
      for P in Pending_Index loop
         --  A slot whose entry is gone (cleared, or scrolled away) is free.
         if Waited_On (P) /= CCL.Streams.No_Handle
           and then CCL.Sessions.Waiting_Entry (Item, Waited_On (P)) = 0
         then
            Waited_On (P) := CCL.Streams.No_Handle;
         end if;
         if Waited_On (P) = CCL.Streams.No_Handle and then Free = 0 then
            Free := P;
         end if;
      end loop;
      if Free = 0 then
         CCL.Sessions.Abandon_Wait
           (Item, Index, Not_Resumable ("more than" & Natural'Image (MAXIMUM_PENDING_AWAITS) &
              " entries are waiting on tasks"));
         return;
      end if;
      Waited_On (Free) := Handle;
      Pending (Free).Calls := Calls;
   end Keep_Wait;

   --  Resume every entry whose task completed.
   procedure Resume_Ready (Item : in out CCL.Sessions.Session; Changed : out Boolean) is
      use type CCL.Streams.Handle;
      use type CCL.Language.Interpretation_Status;
      Outcome : CCL.Language.Interpretation_Result;
      Index : CCL.Sessions.History_Count;
      Resumed : Boolean;
   begin
      Changed := False;
      for P in Pending_Index loop
         if Waited_On (P) /= CCL.Streams.No_Handle and then Host.Task_Done (Waited_On (P)) then
            Index := CCL.Sessions.Waiting_Entry (Item, Waited_On (P));
            Waited_On (P) := CCL.Streams.No_Handle;
            if Index > 0 then
               Replay.Rewind (Pending (P).Calls);
               Resume_Live (Item, Index, CCL.Sessions.Default_Fuel, Granted_Interfaces,
                            Pending (P), Outcome, Resumed);
               Changed := Changed or else Resumed;
               if Resumed and then Outcome.Status = CCL.Language.Waiting_On_Task then
                  Keep_Wait (Item, Outcome.Waited_On, Pending (P).Calls);
               elsif Resumed then
                  --  Reported like an entry that completed when submitted.
                  Platform.REPL_Completed (CCL.Sessions.Result_Image (Outcome));
               end if;
            end if;
         end if;
      end loop;
   end Resume_Ready;
   procedure Submit_Granted
     (Item : in out CCL.Sessions.Session; Source : String;
      Fuel : CCL.Sessions.Fuel_Budget; Outcome : out CCL.Language.Interpretation_Result;
      Shown : String := "");
   procedure Submit_Granted
     (Item : in out CCL.Sessions.Session; Source : String;
      Fuel : CCL.Sessions.Fuel_Budget; Outcome : out CCL.Language.Interpretation_Result;
      Shown : String := "") is
   begin
      Replay.Clear (Fresh);
      Submit_Live (Item, Source, Fuel, Granted_Interfaces, Fresh, Outcome, Shown);
      if CCL.Language."=" (Outcome.Status, CCL.Language.Waiting_On_Task) then
         Keep_Wait (Item, Outcome.Waited_On, Fresh.Calls);
      end if;
   end Submit_Granted;
   procedure Submit_Entry is new CCL_REPL_Commands (Submit_Granted);
   function Now_Ms return Unsigned_64 is (CCL_Window.Ticks);
   procedure Handle is new CCL_Console_View.Handle (Submit_Entry, Now_Ms);
   procedure Follow is new CCL_Console_View.Follow (Submit_Entry, Now_Ms);

   --  Every outlet of a program the last entry started gets its own live
   --  card (docs/ccl-launch-parameters.md, "Every outlet gets a card"):
   --  by the run's name when the entry defined one, as in
   --  (window 64 (as.unix.stderr bad)), else by its session stream. Its
   --  outcome gets a live card showing its state: (as.outcome bad).
   procedure Follow_Started (State : in out CCL_Console_View.View_State) is
      Items : CCL_Program_Bindings.Started_Array;
      Count : Natural;
      Source : constant String := CCL_Console_View.Latest_Source (State);
      DEFINE : constant String := "(define ";
      function Defined_Name return String is
      begin
         if Source'Length > DEFINE'Length
           and then Source (Source'First .. Source'First + DEFINE'Length - 1) = DEFINE
         then
            for K in Source'First + DEFINE'Length .. Source'Last loop
               if Source (K) = ' ' then
                  return Source (Source'First + DEFINE'Length .. K - 1);
               elsif Source (K) not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' then
                  return "";
               end if;
            end loop;
         end if;
         return "";
      end Defined_Name;
      Name : constant String := Defined_Name;
   begin
      CCL_Program_Bindings.Take_Started (Items, Count);
      for I in 1 .. Count loop
         declare
            use type CCL_Program_Bindings.Started_Kind;
            O : CCL_Program_Bindings.Started_Outlet renames Items (I);
            Program : constant String := O.Program (1 .. O.Program_Length);
            function Image (Value : Integer_64) return String is
               Text : constant String := Integer_64'Image (Value);
            begin
               return Text (Text'First + 1 .. Text'Last);
            end Image;
            --  The run: by its name, else as a Run value.
            Run : constant String :=
              (if Name'Length > 0 then Name
               else "(Run program => """ & O.Launched (1 .. O.Launched_Length) & """ pid => " &
                    Image (O.Pid) & ")");
         begin
            if O.Kind = CCL_Program_Bindings.Outcome_Started then
               --  Its state, live: Running until it ends, then Done with
               --  how it ended (at once, for a run that was already over).
               Follow (State, "(" & Program & ".outcome " & Run & ")");
            else
               declare
                  Accessor : constant String := Program & "." & O.Outlet (1 .. O.Outlet_Length);
                  Reference : constant String :=
                    (if Name'Length > 0 then "(" & Accessor & " " & Name & ")"
                     else "(stream " & (if O.Integers then "Integer" else "String") & " " &
                          Image (O.Stream) & ")");
               begin
                  Follow (State, (if O.Integers then "(latest " & Reference & ")"
                                  else "(window 64 " & Reference & ")"));
               end;
            end if;
         end;
      end loop;
   end Follow_Started;
   procedure Reevaluate_Live is new CCL.Sessions.Reevaluate_With_Values
     (Live_Context, Invoke_Live, Read_Stream => Read_Live);
   procedure Reevaluate
     (Item : in out CCL.Sessions.Session; Index : CCL.Sessions.History_Index;
      Fuel : CCL.Sessions.Fuel_Budget;
      Outcome : out CCL.Language.Interpretation_Result; Reevaluated : out Boolean) is
   begin
      Reevaluate_Live (Item, Index, Fuel, Granted_Interfaces, Live_Host, Outcome, Reevaluated);
   end Reevaluate;
   procedure Refresh is new CCL_Console_View.Refresh (Reevaluate, Now_Ms);
   procedure Resume_Waits is new CCL_Console_View.Update_Session (Resume_Ready);

   function Console_Holds (Handle : CCL.Streams.Handle) return Boolean is
     (CCL_Console_View.Holds_Stream (Console, Handle) or else
      (for some Task_Handle of Waited_On => CCL.Streams."=" (Task_Handle, Handle)));
   --  After every entry and live run: close the streams nothing binds.
   procedure Retain_Streams is new Host.Retain_Streams (Console_Holds);
   Canvas : CuBit.UI.Canvas :=
     (addr => System.Null_Address, width => WIDTH, height => HEIGHT,
      pitch => 0, clipEnabled => False, clip => (others => 0), others => <>);

   --  The view's event for a platform event, or No_Event.
   function View_Event_Of
     (Event : Platform.Window_Event; Code, Modifiers : Unsigned_32; X, Y : Integer_32)
      return CCL_Console_View.View_Event
   is
      use CCL_Console_View;
      Shift : constant Boolean := (Modifiers and Platform.SHIFT_MODIFIER) /= 0;
      Control : constant Boolean := (Modifiers and Platform.CONTROL_MODIFIER) /= 0;
      Kind : constant Event_Kind :=
        (case Event is
            when Platform.Text_Input => Text_Input,
            when Platform.Backspace => Backspace,
            when Platform.Delete => Delete,
            when Platform.Left => Left,
            when Platform.Right => Right,
            when Platform.Home => Home,
            when Platform.End_Key => End_Key,
            when Platform.Up => Up,
            when Platform.Down => Down,
            when Platform.Page_Up => Page_Up,
            when Platform.Page_Down => Page_Down,
            when Platform.Enter => Enter,
            when Platform.Run_Source => Run,
            when Platform.Tab => Tab,
            when Platform.Complete_Operation => Complete,
            when Platform.Toggle_Syntax => Toggle_Notation,
            when Platform.Escape => Escape,
            when Platform.Select_All => Select_All,
            when Platform.Pointer_Hover => Pointer_Move,
            when Platform.Pointer_Down | Platform.Double_Click | Platform.Triple_Click => Pointer_Down,
            when Platform.Pointer_Drag => Pointer_Drag,
            when Platform.Pointer_Up => Pointer_Up,
            when Platform.Wheel_Up => Wheel_Up,
            when Platform.Wheel_Down => Wheel_Down,
            when others => No_Event);
   begin
      return (Kind => Kind,
              Character_Value => (if Code <= 127 then Character'Val (Code) else ' '),
              X => (if X > 0 then Natural (X) else 0),
              Y => (if Y > 0 then Natural (Y) else 0),
              Shift => Shift, Control => Control);
   end View_Event_Of;

   procedure Run is
      Installed : Boolean;
   begin
      CCL_Log_IO.Announce ("ccl-console: started");
      Platform.Activate (Name => "ccl-console", Title => "CCL Console");
      CCL.Catalog.Initialize (Visible_Interfaces);
      CCL.Catalog.Initialize (Granted_Interfaces);
      Host.Install (Visible_Interfaces, Granted_Interfaces, Installed);
      if Installed then Endpoints.Install (Visible_Interfaces, Granted_Interfaces, Installed); end if;
      if not Installed then raise Program_Error with "invalid CCL host environment"; end if;
      CCL_Console_View.Initialize (Console, Visible_Interfaces);
      declare
         Handle_Window : constant System.Address :=
           CCL_Window.Open (Integer_32 (WIDTH), Integer_32 (HEIGHT));
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
         Last_Pointer : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default;

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
            CCL_Console_View.Draw (Console, Canvas, (0, 0, Canvas.width, Canvas.height));
            return Platform.Submit_Frame (Handle_Window, Canvas, Repair);
         end Paint;
      begin
         Window_Handle := Handle_Window;
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
                  Submitted, Redraw : Boolean;
               begin
                  if Polled = Platform.Close_Request then
                     Running := False;
                  else
                     Handle (Console, View_Event_Of (Polled, Code, Modifiers, Mouse_X, Mouse_Y),
                             Submitted, Redraw);
                     Needs_Render := Needs_Render or else Redraw;
                     if Submitted then
                        Follow_Started (Console);
                        Retain_Streams;
                        Platform.REPL_Completed (CCL_Console_View.Latest_Result (Console));
                     end if;
                  end if;
                  Platform.Finish_Input;
               end;
            end loop;
            exit when not Running;
            if CCL_Console_View.Pointer_Style (Console) /= Last_Pointer then
               Last_Pointer := CCL_Console_View.Pointer_Style (Console);
               CCL_Window.Set_Cursor
                 (Handle_Window, Integer_32 (CuBit.UI.Pointer_Cursor_Style'Enum_Rep (Last_Pointer)));
            end if;
            --  Stream elements that are due; live cells read them next.
            declare
               Arrived : Boolean;
            begin
               Host.Pump_Streams (Arrived);
               if Arrived then CCL_Console_View.Note_Arrival (Console); end if;
            end;
            --  Entries whose task completed.
            declare
               Redraw : Boolean;
            begin
               Resume_Waits (Console, Redraw);
               if Redraw then Retain_Streams; end if;
               Needs_Render := Needs_Render or else Redraw;
            end;
            --  Live cells whose time has come.
            declare
               Due : constant Unsigned_64 := CCL_Console_View.Next_Deadline (Console);
               Redraw : Boolean;
            begin
               if Due /= 0 and then CCL_Window.Ticks >= Due then
                  Refresh (Console, Redraw);
                  Retain_Streams;
                  Needs_Render := Needs_Render or else Redraw;
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
                  Due : constant Unsigned_64 := CCL_Console_View.Next_Deadline (Console);
                  Stream_Delay : constant Unsigned_64 := Host.Next_Stream_Delay;
                  Ticks : constant Unsigned_64 := CCL_Window.Ticks;
                  --  The next live cell, or the next stream element.
                  Next : constant Unsigned_64 := Unsigned_64'Min
                    ((if Due = 0 then Unsigned_64'Last else Due),
                     (if Stream_Delay > Unsigned_64'Last - Ticks then Unsigned_64'Last
                      else Ticks + Stream_Delay));
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
         CCL_Execution.Stop;
         CCL_Window.Close (Handle_Window);
      end;
   end Run;
end CCL_Console;
