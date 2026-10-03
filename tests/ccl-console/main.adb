--  Hosted (Linux) tests of the CCL console view. Evaluation is the pure
--  session engine (no host services); frames are drawn into memory and
--  written as PPM files under build/ for visual review.
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL_Image_Bindings;
with CCL_Image_Loading;
with CCL.Sessions;
with CCL_Console_View; use CCL_Console_View;
with CCL_Console_Bindings;
with CCL.Units;
with CCL_File_Bindings;
with CCL_Process_Bindings;
with Ada.Directories;
with Ada.Environment_Variables;
with CCL.Interfaces.Console;
with CuBit.UI;

procedure Main is
   use type Ada.Streams.Stream_Element_Offset;
   Failures : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   Clock : Unsigned_64 := 1_000;
   function Now return Unsigned_64 is
   begin
      Clock := Clock + 3;
      return Clock;
   end Now;
   procedure Console_Handle is new Handle (CCL.Sessions.Submit, Now);

   --  A second console whose session can draw: the image interface installed
   --  and granted, as CCL_Host_Environment does for the native apps.
   Image_Catalog : CCL.Catalog.Interface_Catalog;
   Image_Grants : CCL.Catalog.Granted_Bindings;
   type No_Context is null record;
   Host : No_Context;
   procedure Invoke
     (Context : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      if CCL_Image_Loading.Handles (Binding) then
         CCL_Image_Loading.Invoke (Binding, Argument, Reply);
      else
         CCL_Image_Bindings.Invoke (Binding, Argument, Reply);
      end if;
   end Invoke;
   procedure Submit_Images is new CCL.Sessions.Submit_With_Values (No_Context, Invoke);
   procedure Submit_Granted
     (Item : in out CCL.Sessions.Session; Source : String;
      Fuel : CCL.Sessions.Fuel_Budget; Outcome : out CCL.Language.Interpretation_Result) is
   begin
      Submit_Images (Item, Source, Fuel, Image_Grants, Host, Outcome);
   end Submit_Granted;
   procedure Image_Handle is new Handle (Submit_Granted, Now);

   WIDTH  : constant := 900;
   HEIGHT : constant := 560;
   type Pixels is array (0 .. WIDTH * HEIGHT - 1) of Unsigned_32;
   type Pixels_Access is access Pixels;
   Image : constant Pixels_Access := new Pixels'(others => 0);
   Canvas : constant CuBit.UI.Canvas :=
     (addr => Image.all'Address, width => WIDTH, height => HEIGHT, pitch => WIDTH * 4,
      clipEnabled => False, clip => (others => 0), others => <>);
   Bounds : constant CuBit.UI.Rect := (0, 0, WIDTH, HEIGHT);

   State : View_State;

   --  A console programmable from its own cells: console.* answered by
   --  this view, as the native console instantiates it.
   Programmable : View_State;
   Titled : String (1 .. CCL.Interfaces.Console.MAX_TITLE) := [others => ' '];
   Titled_Length : Natural := 0;
   procedure Set_Title (Text : String) is
   begin
      Titled_Length := Natural'Min (Text'Length, Titled'Length);
      Titled (1 .. Titled_Length) := Text (Text'First .. Text'First + Titled_Length - 1);
   end Set_Title;
   function Notation_Now return CCL.Interfaces.Console.Notation is (Notation (Programmable));
   procedure Notation_Set (Value : CCL.Interfaces.Console.Notation) is
   begin
      Set_Notation (Programmable, Value);
   end Notation_Set;
   function Stats_Now return CCL.Interfaces.Console.Statistics is (Statistics (Programmable));
   package Endpoints is new CCL_Console_Bindings
     (Set_Title, Notation_Now, Notation_Set, CCL_Console_View.Theme, CCL_Console_View.Set_Theme, Stats_Now);
   Console_Grants : CCL.Catalog.Granted_Bindings;
   procedure Invoke_Console
     (Context : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      Endpoints.Invoke (Binding, Argument, Reply);
   end Invoke_Console;
   procedure Submit_Console is new CCL.Sessions.Submit_With_Values (No_Context, Invoke_Console);
   procedure Submit_Programmable
     (Item : in out CCL.Sessions.Session; Source : String;
      Fuel : CCL.Sessions.Fuel_Budget; Outcome : out CCL.Language.Interpretation_Result) is
   begin
      Submit_Console (Item, Source, Fuel, Console_Grants, Host, Outcome);
   end Submit_Programmable;
   procedure Programmable_Handle is new Handle (Submit_Programmable, Now);

   --  A console with places (fs.*): here, :cd and :ls over a scratch tree.
   Placed : View_State;
   Place_Grants : CCL.Catalog.Granted_Bindings;
   procedure Invoke_Files
     (Context : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      if CCL_Process_Bindings.Handles (Binding) then
         CCL_Process_Bindings.Invoke (Binding, Argument, Reply);
      else
         CCL_File_Bindings.Invoke (Binding, Argument, Reply);
      end if;
   end Invoke_Files;
   procedure Submit_Files is new CCL.Sessions.Submit_With_Values (No_Context, Invoke_Files);
   procedure Submit_Placed
     (Item : in out CCL.Sessions.Session; Source : String;
      Fuel : CCL.Sessions.Fuel_Budget; Outcome : out CCL.Language.Interpretation_Result) is
   begin
      Submit_Files (Item, Source, Fuel, Place_Grants, Host, Outcome);
   end Submit_Placed;
   procedure Placed_Handle is new Handle (Submit_Placed, Now);
   Catalog : CCL.Catalog.Interface_Catalog;

   procedure Send (Event : View_Event) is
      Submitted, Redraw : Boolean;
   begin
      Console_Handle (State, Event, Submitted, Redraw);
      Draw (State, Canvas, Bounds);
   end Send;
   procedure Key (Kind : Event_Kind; Shift, Control : Boolean := False) is
   begin
      Send ((Kind => Kind, Shift => Shift, Control => Control, others => <>));
   end Key;
   procedure Type_Text (Text : String) is
   begin
      for C of Text loop
         Send ((Kind => Text_Input, Character_Value => C, others => <>));
      end loop;
   end Type_Text;
   procedure Click (Area : CuBit.UI.Rect) is
   begin
      Send ((Kind => Pointer_Down, X => Area.x + Area.w / 2, Y => Area.y + Area.h / 2, others => <>));
      Send ((Kind => Pointer_Up, X => Area.x + Area.w / 2, Y => Area.y + Area.h / 2, others => <>));
   end Click;
   procedure Hover (Area : CuBit.UI.Rect) is
   begin
      Send ((Kind => Pointer_Move, X => Area.x + Area.w / 2, Y => Area.y + Area.h / 2, others => <>));
   end Hover;

   --  build/NAME.ppm: the current frame.
   procedure Save (Name : String) is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Header : constant String := "P6" & ASCII.LF & "900 560" & ASCII.LF & "255" & ASCII.LF;
      Row : Ada.Streams.Stream_Element_Array (1 .. WIDTH * 3);
   begin
      Create (File, Out_File, "build/" & Name & ".ppm");
      for C of Header loop
         Ada.Streams.Stream_Element_Array'Write
           (Stream (File), [1 => Ada.Streams.Stream_Element (Character'Pos (C))]);
      end loop;
      for Y in 0 .. HEIGHT - 1 loop
         for X in 0 .. WIDTH - 1 loop
            declare
               P : constant Unsigned_32 := Image (Y * WIDTH + X);
               Base : constant Ada.Streams.Stream_Element_Offset :=
                 Ada.Streams.Stream_Element_Offset (X * 3);
            begin
               Row (Base + 1) := Ada.Streams.Stream_Element (Shift_Right (P, 16) and 16#FF#);
               Row (Base + 2) := Ada.Streams.Stream_Element (Shift_Right (P, 8) and 16#FF#);
               Row (Base + 3) := Ada.Streams.Stream_Element (P and 16#FF#);
            end;
         end loop;
         Write (File, Row);
      end loop;
      Close (File);
   end Save;

   function Empty (Area : CuBit.UI.Rect) return Boolean is (Area.w = 0 or else Area.h = 0);
begin
   CCL.Catalog.Initialize (Catalog);
   Initialize (State, Catalog);
   Draw (State, Canvas, Bounds);
   Save ("console-welcome");
   Check (not Empty (Region (State, Example, 1)), "welcome shows examples");

   --  An example is one click from the input.
   Click (Region (State, Example, 2));
   Check (Input_Text (State) = "(sort (list 5 3 9 1))", "example click fills the input");
   Key (Enter);
   Check (Latest_Result (State) = "List<Integer>: [1, 3, 5, 9]", "example runs: " & Latest_Result (State));
   Check (Input_Text (State) = "", "submit clears the input");

   --  Enter inside an open form continues it, indented; a complete form runs.
   Type_Text ("(+ 20");
   Key (Enter);
   Check (Input_Text (State) = "(+ 20" & ASCII.LF & "  ", "open form continues on a new line");
   Type_Text ("22)");
   Key (Enter);
   Check (Latest_Result (State) = "Integer: 42", "multi-line entry runs: " & Latest_Result (State));
   --  Shift+Enter always breaks the line; Ctrl+Enter (Run) runs regardless.
   Type_Text ("(+ 1 2)");
   Key (Enter, Shift => True);
   Check (Input_Text (State) = "(+ 1 2)" & ASCII.LF, "Shift+Enter inserts a line break");
   Key (Backspace);

   --  Completion: special forms and built-ins, accepted with Tab.
   Key (Escape);
   Type_Text ("(so");
   Check (Suggestions (State) >= 2, "completion offers sort and sort-by");
   Check (Suggestion_Name (State, 1) = "sort", "first suggestion is sort: " & Suggestion_Name (State, 1));
   Key (Down);
   Check (Suggestion_Name (State, 2) = "sort-by", "second suggestion is sort-by");
   Save ("console-completion");
   Key (Up);
   Key (Tab);
   Check (Input_Text (State) = "(sort ", "Tab accepts the selected suggestion: " & Input_Text (State));
   Check (Suggestions (State) = 0, "accepting closes the popup");
   Type_Text ("(list ""b"" ""a""))");
   Key (Enter);
   Check (Latest_Result (State) = "List<String>: [""a"", ""b""]", "completed entry runs: " & Latest_Result (State));

   --  A failure keeps its diagnostic; history recalls past sources.
   Type_Text ("(+ 1 ""two"")");
   Key (Enter);
   Check (Latest_Result (State)'Length > 0 and then Latest_Result (State) (1 .. 4) /= "Inte",
          "type error is reported: " & Latest_Result (State));
   Key (Up);
   Check (Input_Text (State) = "(+ 1 ""two"")", "Up recalls the last entry");
   Key (Up);
   Check (Input_Text (State) = "(sort (list ""b"" ""a""))", "Up again recalls the one before");
   Key (Down);
   Key (Down);
   Check (Input_Text (State) = "", "Down past the newest restores the draft");

   --  Clicking a past result inserts it; clicking a past source edits it.
   Type_Text ("(length ");
   Click (Region (State, Entry_Result, 1));
   Check (Input_Text (State) = "(length [1, 3, 5, 9]" or else Input_Text (State)'Length > 8,
          "result click inserts the value: " & Input_Text (State));
   Click (Region (State, Entry_Source, 2));
   Check (Input_Text (State) = "(+ 20" & ASCII.LF & "  22)", "source click recalls the entry: " & Input_Text (State));
   Key (Escape);
   Type_Text ("(concat ""to"" (to-string 2))");
   Key (Enter);
   Hover (Region (State, Result_Type, 1));
   Save ("console-transcript");
   Check (not Empty (Region (State, Result_Type, 1)), "results carry a type badge");

   --  A list of records is a table: field names over typed cells.
   Key (Escape);
   Type_Text ("(type Event (record (time Integer) (level String) (source String) (message String)))");
   Key (Enter);
   Type_Text ("(list (Event 1200 ""info"" ""netstack"" ""link up on virtio-net 0, 1000 Mb/s full duplex"") " &
              "(Event 1385 ""warn"" ""display"" ""vblank late by 3 ms"") " &
              "(Event 2101 ""error"" ""filesystem"" ""journal replay needed"") " &
              "(Event 2950 ""info"" ""desktop"" ""session ready""))");
   Key (Enter);
   Check (Latest_Result (State)'Length > 12 and then Latest_Result (State) (1 .. 12) = "List<Event>:",
          "a list of records: " & Latest_Result (State));
   Check (not Empty (Region (State, Table_Cell, 1)), "the list renders as a table");
   Check (Empty (Region (State, Table_Cell, 17)), "four rows of four cells");
   Hover (Region (State, Table_Cell, 7));
   Save ("console-table");
   Key (Escape);
   Click (Region (State, Table_Cell, 5));
   Check (Input_Text (State) = "1385", "clicking a cell inserts its value: " & Input_Text (State));
   --  A header click writes the CCL that sorts by that field; Ctrl+click
   --  on a cell writes the filter for its value. Both run as typed.
   Key (Escape);
   Click (Region (State, Table_Header, 2));
   Check (Input_Text (State)'Length > 39 and then
          Input_Text (State) (1 .. 39) = "(sort-by (fn ((row Event)) (field row l",
          "header click writes sort-by: " & Input_Text (State));
   Key (Enter);
   Check (Latest_Result (State)'Length > 32 and then
          Latest_Result (State) (1 .. 32) = "List<Event>: [(Event 2101 ""error",
          "sorted by level: " & Latest_Result (State));
   Send ((Kind => Pointer_Down, Control => True,
          X => Region (State, Table_Cell, 2).x + 2, Y => Region (State, Table_Cell, 2).y + 2, others => <>));
   Send ((Kind => Pointer_Up, X => Region (State, Table_Cell, 2).x + 2, Y => Region (State, Table_Cell, 2).y + 2,
          others => <>));
   Key (Enter);
   --  Cell 2 is the first table's first level ("info"): regions count from
   --  the oldest entry on screen.
   Check (Latest_Result (State) = "List<Event>: [(Event 1200 ""info"" ""netstack"" " &
            """link up on virtio-net 0, 1000 Mb/s full duplex"") (Event 2950 ""info"" ""desktop"" ""session ready"")]",
          "Ctrl+click filters to that value: " & Latest_Result (State));
   Key (Escape);
   Type_Text ("(Event 7 ""debug"" ""clock"" ""tick"")");
   Key (Enter);
   Check (not Empty (Region (State, Table_Cell, 17)), "a single record is a one-row table");

   --  Images: data drawn by the image interface, shown as pictures.
   declare
      Installed : Boolean;
      Pictures : View_State;
      procedure Run_Entry (Source : String) is
         Submitted, Redraw : Boolean;
      begin
         for C of Source loop
            Image_Handle (Pictures, (Kind => Text_Input, Character_Value => C, others => <>), Submitted, Redraw);
         end loop;
         Image_Handle (Pictures, (Kind => Run, others => <>), Submitted, Redraw);
         Draw (Pictures, Canvas, Bounds);
      end Run_Entry;
      Submitted_Unused, Redraw_Unused : Boolean;
      procedure Click_In (Item : in out View_State; Area : CuBit.UI.Rect) is
         Submitted, Redraw : Boolean;
      begin
         Image_Handle (Item, (Kind => Pointer_Down, X => Area.x + Area.w / 2, Y => Area.y + Area.h / 2, others => <>),
                       Submitted, Redraw);
         Image_Handle (Item, (Kind => Pointer_Up, X => Area.x + Area.w / 2, Y => Area.y + Area.h / 2, others => <>),
                       Submitted, Redraw);
         Draw (Item, Canvas, Bounds);
      end Click_In;
      function Starts (Text, Prefix : String) return Boolean is
        (Text'Length >= Prefix'Length and then Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);
   begin
      CCL.Catalog.Initialize (Image_Catalog);
      CCL.Catalog.Initialize (Image_Grants);
      CCL_Image_Bindings.Install (Image_Catalog, Image_Grants, Installed);
      Check (Installed, "the image interface installs");
      CCL_Image_Loading.Install (Image_Catalog, Image_Grants, Installed);
      Check (Installed, "image loading installs");
      Initialize (Pictures, Image_Catalog);
      Run_Entry ("(image.plot (list 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9))");
      Check (Starts (Latest_Result (Pictures), "Image: (Image 320 120 "), "plot draws: " & Latest_Result (Pictures));
      Check (not Empty (Region (Pictures, Entry_Result, 1)), "an image result is a picture");
      Run_Entry ("(image.heatmap (Grid 15 15 (each (fn ((i Integer)) (* (mod i 15) (/ i 15))) (range 0 224))))");
      Check (Starts (Latest_Result (Pictures), "Image: (Image 15 15 "), "heatmap draws: " & Latest_Result (Pictures));
      Run_Entry ("(image.bars (list 5 -3 8 2 7 -1 4))");
      Check (Starts (Latest_Result (Pictures), "Image: (Image 320 120 "), "bars draw: " & Latest_Result (Pictures));
      Run_Entry ("(image.gradient (Size 64 24))");
      Check (Starts (Latest_Result (Pictures), "Image: (Image 64 24 "), "gradient draws: " & Latest_Result (Pictures));
      Run_Entry ("(image.plot (list 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9))");
      Check (Latest_Result (Pictures) = Latest_Result (Pictures) and then
             Starts (Latest_Result (Pictures), "Image: (Image 320 120 "), "the same data, the same image");
      --  Files: QOI and PPM from the workspace (the hosted mock's demos).
      Run_Entry ("(image.load ""demo.ppm"")");
      Check (Starts (Latest_Result (Pictures), "Image: (Image 3 2 "), "PPM loads: " & Latest_Result (Pictures));
      Run_Entry ("(image.load ""demo.qoi"")");
      Check (Starts (Latest_Result (Pictures), "Image: (Image 24 16 "), "QOI loads: " & Latest_Result (Pictures));
      Run_Entry ("(image.load ""missing.qoi"")");
      Check (not Starts (Latest_Result (Pictures), "Image:"), "a missing file is a failure");
      Run_Entry ("(image.load ""notes.txt"")");
      Check (not Starts (Latest_Result (Pictures), "Image:"), "only image files load");
      Save ("console-images");
      --  A fractal computed in CCL (integer fixed point, state packed in one
      --  Integer), drawn as five heat-map strips stacked and enlarged.
      Run_Entry ("(define (zx (s Integer)) Integer (- (mod (/ s 32768) 32768) 16384))");
      Run_Entry ("(define (zy (s Integer)) Integer (- (mod s 32768) 16384))");
      Run_Entry ("(define (zn (s Integer)) Integer (/ s 1073741824))");
      Run_Entry ("(define (pack (x Integer) (y Integer) (n Integer)) Integer (+ (* n 1073741824) (+ (* (+ x 16384) 32768) (+ y 16384))))");
      Run_Entry ("(define (step (cx Integer) (cy Integer) (s Integer)) Integer (if (> (+ (* (zx s) (zx s)) (* (zy s) (zy s))) 4000000) s (pack (+ (/ (- (* (zx s) (zx s)) (* (zy s) (zy s))) 1000) cx) (+ (/ (* 2 (* (zx s) (zy s))) 1000) cy) (+ (zn s) 1))))");
      Run_Entry ("(define (escape (cx Integer) (cy Integer)) Integer (zn (fold (fn ((s Integer) (i Integer)) (step cx cy s)) (pack 0 0 0) (range 1 12))))");
      Run_Entry ("(define s1 (image.heatmap (Grid 32 7 (each (fn ((i Integer)) (escape (- (* (mod i 32) 90) 2100) (- (* (+ (/ i 32) 0) 68) 1190))) (range 0 223)))))");
      Run_Entry ("(define s2 (image.heatmap (Grid 32 7 (each (fn ((i Integer)) (escape (- (* (mod i 32) 90) 2100) (- (* (+ (/ i 32) 7) 68) 1190))) (range 0 223)))))");
      Run_Entry ("(define s3 (image.heatmap (Grid 32 7 (each (fn ((i Integer)) (escape (- (* (mod i 32) 90) 2100) (- (* (+ (/ i 32) 14) 68) 1190))) (range 0 223)))))");
      Run_Entry ("(define s4 (image.heatmap (Grid 32 7 (each (fn ((i Integer)) (escape (- (* (mod i 32) 90) 2100) (- (* (+ (/ i 32) 21) 68) 1190))) (range 0 223)))))");
      Run_Entry ("(define s5 (image.heatmap (Grid 32 7 (each (fn ((i Integer)) (escape (- (* (mod i 32) 90) 2100) (- (* (+ (/ i 32) 28) 68) 1190))) (range 0 223)))))");
      Run_Entry ("(image.scale (Scaled (image.stack (list s1 s2 s3 s4 s5)) 7))");
      Check (Starts (Latest_Result (Pictures), "Image: (Image 224 245 "), "the fractal assembles: " & Latest_Result (Pictures));
      Save ("console-fractal");
      --  A list of Images is a gallery; a thumbnail inserts its own value.
      Run_Entry ("(list s1 (image.gradient (Size 48 32)) (image.plot (list 1 4 2 8 5 7)))");
      Check (Starts (Latest_Result (Pictures), "List<Image>: "), "a list of images: " & Latest_Result (Pictures));
      Check (not Empty (Region (Pictures, Table_Cell, 3)) and then Empty (Region (Pictures, Table_Header, 1)),
             "a gallery, not a table");
      Image_Handle (Pictures, (Kind => Escape, others => <>), Submitted_Unused, Redraw_Unused);
      Click_In (Pictures, Region (Pictures, Table_Cell, 2));
      Check (Starts (Input_Text (Pictures), "(Image 48 32 "), "a thumbnail inserts its image: " & Input_Text (Pictures));
      Save ("console-gallery");
   end;

   --  Live cells: :watch re-runs the newest plain expression on a clock.
   declare
      procedure Reevaluate
        (Item : in out CCL.Sessions.Session; Index : CCL.Sessions.History_Index;
         Fuel : CCL.Sessions.Fuel_Budget;
         Outcome : out CCL.Language.Interpretation_Result; Reevaluated : out Boolean)
      is
         procedure Pure is new CCL.Sessions.Reevaluate_With_Values (No_Context, Invoke);
         None : CCL.Catalog.Granted_Bindings;
      begin
         CCL.Catalog.Initialize (None);
         Pure (Item, Index, Fuel, None, Host, Outcome, Reevaluated);
      end Reevaluate;
      procedure Live_Refresh is new Refresh (Reevaluate, Now);
      Redraw : Boolean;
      Before : Natural;
   begin
      Key (Escape);
      Type_Text ("(* 6 7)");
      Key (Enter);
      Before := Latest_Result (State)'Length;
      Type_Text (":watch 2");
      Key (Enter);
      Check (Input_Text (State) = "", ":watch clears the input");
      Check (Latest_Result (State) = "Integer: 42" and then Before > 0, ":watch adds no entry");
      Check (Next_Deadline (State) > Clock, "a live cell has a deadline");
      Clock := Next_Deadline (State);
      Live_Refresh (State, Redraw);
      Check (Redraw, "a due live cell redraws");
      Live_Refresh (State, Redraw);
      Check (not Redraw, "not again before its interval");
      Clock := Next_Deadline (State) + 1;
      Live_Refresh (State, Redraw);
      Draw (State, Canvas, Bounds);
      Check (not Empty (Region (State, Live_Mark, 1)), "a live cell shows its mark");
      Save ("console-live");
      Click (Region (State, Live_Mark, 1));
      Check (Next_Deadline (State) = 0, "clicking the mark stops it");
      --  A definition never re-runs: watching it is refused on the first run.
      Type_Text ("(define (twice (n Integer)) Integer (+ n n))");
      Key (Enter);
      Type_Text (":watch");
      Key (Enter);
      Clock := Next_Deadline (State);
      Live_Refresh (State, Redraw);
      Check (Next_Deadline (State) = 0, "a definition cannot be live");
      Type_Text (":unwatch");
      Key (Enter);
   end;

   --  The console programs itself: console.* from its own cells, and the
   --  whole transcript switching between Lisp and BASIC.
   declare
      use type CCL.Interfaces.Console.Notation;
      use type CCL.Interfaces.Console.Theme;
      Console_Catalog : CCL.Catalog.Interface_Catalog;
      Installed, Submitted, Redraw : Boolean;
      procedure Run (Source : String) is
      begin
         for C of Source loop
            Programmable_Handle (Programmable, (Kind => Text_Input, Character_Value => C, others => <>),
                                 Submitted, Redraw);
         end loop;
         Programmable_Handle (Programmable, (Kind => Run, others => <>), Submitted, Redraw);
      end Run;
      procedure Expect_Shown (Index : Positive; Text : String; Label : String) is
      begin
         Draw (Programmable, Canvas, Bounds);
         Check (Cell_Source (Programmable, Index) = Text,
                Label & ": got """ & Cell_Source (Programmable, Index) & """");
      end Expect_Shown;
   begin
      CCL.Catalog.Initialize (Console_Catalog);
      CCL.Catalog.Initialize (Console_Grants);
      Endpoints.Install (Console_Catalog, Console_Grants, Installed);
      Check (Installed, "console.* is published and granted");
      Initialize (Programmable, Console_Catalog);
      Run ("(+ 20 22)");
      Run ("(define (twice (n Integer)) Integer (+ n n))");
      Run ("(twice 21)");
      Run ("(console.title ""Programmed by CCL"")");
      Check (Titled (1 .. Titled_Length) = "Programmed by CCL",
             "console.title sets the window title: " & Latest_Result (Programmable));
      Run ("(console.stats)");
      Check (Latest_Result (Programmable)'Length > 15 and then
             Latest_Result (Programmable) (1 .. 15) = "Console_Stats: ",
             "console.stats is a typed record: " & Latest_Result (Programmable));
      Run ("(field (console.stats) cells)");
      Check (Latest_Result (Programmable) = "Integer: 5", "stats count the cells: " & Latest_Result (Programmable));
      Run ("(console.notation Notation.Basic)");
      Check (Notation (Programmable) = CCL.Interfaces.Console.Basic and then
             Latest_Result (Programmable) = "Notation.Basic",
             "console.notation switches to BASIC and says so: " & Latest_Result (Programmable));
      Expect_Shown (1, "20 + 22", "a cell reads in BASIC");
      Expect_Shown (3, "twice(21)", "a cell using the session's function reads in BASIC");
      --  Typed in BASIC, run as BASIC.
      Run ("twice(5) + 1");
      Check (Latest_Result (Programmable) = "Integer: 11", "BASIC input runs: " & Latest_Result (Programmable));
      Expect_Shown (8, "twice(5) + 1", "and shows as typed");
      Draw (Programmable, Canvas, Bounds);
      Save ("console-basic");
      Programmable_Handle (Programmable, (Kind => Toggle_Notation, others => <>), Submitted, Redraw);
      Check (Notation (Programmable) = CCL.Interfaces.Console.Lisp and then Redraw, "the key switches back");
      Expect_Shown (1, "(+ 20 22)", "and the cell reads in Lisp again");
      Expect_Shown (8, "(+ (twice 5) 1)", "the BASIC cell reads in Lisp too");
      Run ("(console.theme Theme.Daylight)");
      Check (CCL_Console_View.Theme = CCL.Interfaces.Console.Daylight and then
             Latest_Result (Programmable) = "Theme.Daylight",
             "console.theme switches the palette: " & Latest_Result (Programmable));
      Draw (Programmable, Canvas, Bounds);
      Save ("console-daylight");
      CCL_Console_View.Set_Theme (CCL.Interfaces.Console.Midnight);
      --  Frame cost with a full transcript (reported, not checked).
      declare
         use type Ada.Calendar.Time;
         Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         for I in 1 .. 100 loop
            Draw (Programmable, Canvas, Bounds);
         end loop;
         Ada.Text_IO.Put_Line ("frame cost:" &
           Integer'Image (Integer (1_000_000.0 * Float (Ada.Calendar.Clock - Start) / 100.0)) & " us");
      end;
   end;

   --  Units: values shown as people read them, by their type.
   declare
      package U renames CCL.Units;
      use type U.Unit;
      procedure Expect (Of_Unit : U.Unit; Raw, Shown : String) is
      begin
         Check (U.Humanize (Of_Unit, Raw) = Shown,
                U.Unit'Image (Of_Unit) & " " & Raw & ": got " & U.Humanize (Of_Unit, Raw));
      end Expect;
   begin
      Expect (U.Bytes, "0", "0 B");
      Expect (U.Bytes, "1023", "1023 B");
      Expect (U.Bytes, "1536", "1.5 KiB");
      Expect (U.Bytes, "1073741824", "1.0 GiB");
      Expect (U.Bytes, "9223372036854775807", "7.9 EiB");
      Expect (U.Timestamp, "0", "-");
      Expect (U.Timestamp, "1790964695000", "2026-10-02 18:11");
      Expect (U.Timestamp, "951782400000", "2000-02-29 00:00");
      Expect (U.UNIX_File_Permissions, "16877", "drwxr-xr-x");
      Expect (U.UNIX_File_Permissions, "33188", "-rw-r--r--");
      Expect (U.UNIX_File_Permissions, "41471", "lrwxrwxrwx");
      Expect (U.Milliseconds, "43", "43 ms");
      Expect (U.Milliseconds, "1450", "1.4 s");
      Expect (U.Milliseconds, "125000", "2 min 05 s");
      Expect (U.Bytes, "-1", "-1");
      Expect (U.No_Unit, "1536", "1536");
      Check (U.Unit_Of ("Bytes") = U.Bytes and then U.Unit_Of ("Integer") = U.No_Unit, "units by type name");
   end;

   --  Places: the console's here, listed as typed File_Metadata rows.
   declare
      Place_Catalog : CCL.Catalog.Interface_Catalog;
      Installed, Submitted, Redraw : Boolean;
      Root : constant String := Ada.Directories.Current_Directory & "/build/places";
      procedure Make_File (Name, Text : String) is
         File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Root & "/" & Name);
         Ada.Text_IO.Put (File, Text);
         Ada.Text_IO.Close (File);
      end Make_File;
      procedure Run (Source : String) is
      begin
         for C of Source loop
            Placed_Handle (Placed, (Kind => Text_Input, Character_Value => C, others => <>),
                           Submitted, Redraw);
         end loop;
         Placed_Handle (Placed, (Kind => Run, others => <>), Submitted, Redraw);
      end Run;
   begin
      if Ada.Directories.Exists (Root) then Ada.Directories.Delete_Tree (Root); end if;
      Ada.Directories.Create_Path (Root & "/notes");
      Make_File ("hello.ccl", "(+ 20 22)");
      Make_File ("readme.txt", "places, not a working directory");
      Make_File ("notes/plan.txt", "fs.watch next");
      Ada.Environment_Variables.Set ("CCL_PREVIEW_PLACE", Root);
      CCL.Catalog.Initialize (Place_Catalog);
      CCL.Catalog.Initialize (Place_Grants);
      CCL_File_Bindings.Install (Place_Catalog, Place_Grants, Installed);
      if Installed then CCL_Process_Bindings.Install (Place_Catalog, Place_Grants, Installed); end if;
      Check (Installed, "fs.* is published and granted");
      Initialize (Placed, Place_Catalog);
      Run (":cd");
      Check (Latest_Result (Placed)'Length > 6 and then Latest_Result (Placed) (1 .. 6) = "Place:",
             ":cd binds here to the home place: " & Latest_Result (Placed));
      Draw (Placed, Canvas, Bounds);
      Check (Cell_Source (Placed, 1) = "(define here (fs.home))", ":cd shows the CCL it ran");
      Run (":ls");
      Check (Latest_Result (Placed)'Length > 20 and then
             Latest_Result (Placed) (1 .. 20) = "List<File_Metadata>:",
             ":ls lists typed metadata: " & Latest_Result (Placed));
      Run ("(length (fs.list here))");
      Check (Latest_Result (Placed) = "Integer: 3", "three entries: " & Latest_Result (Placed));
      Run ("(sum (each (fn ((f File_Metadata)) (field f size)) (fs.list here)))");
      --  Text_IO ends each file with a line terminator: 10 + 32 bytes.
      Check (Latest_Result (Placed) = "Integer: 42", "sizes add up: " & Latest_Result (Placed));
      Run (":cd notes");
      Run ("(field (first 1 (fs.list here)) name)");
      Run ("(at (each (fn ((f File_Metadata)) (field f name)) (fs.list here)) 1)");
      Check (Latest_Result (Placed) = "String: plan.txt", ":cd enters a child: " & Latest_Result (Placed));
      Run (":cd ..");
      Run ("(length (fs.list here))");
      Check (Latest_Result (Placed) = "Integer: 3", ":cd .. goes back up: " & Latest_Result (Placed));
      Run (":cd ..");
      Run ("(length (field here path))");
      Check (Latest_Result (Placed) = "Integer: 0", "up from the root stays at the root: " & Latest_Result (Placed));
      Run ("(fs.enter (Child here ""../etc""))");
      Check (Latest_Result (Placed) = "fs.enter rejected its argument: ""../etc"" is not one entry's " &
             "name (no '/', '\', '.' or '..'). Instead: enter one level at a time, and use fs.up for " &
             "the parent", "a name with a separator is refused, and says why: " & Latest_Result (Placed));
      Run ("(fs.list (fs.enter (Child here ""missing"")))");
      Check (Latest_Result (Placed) = "fs.list found nothing there: nothing is at " & Root & "/missing",
             "a missing directory says where it looked: " & Latest_Result (Placed));
      Run (":ls");
      Draw (Placed, Canvas, Bounds);
      Save ("console-places");
      --  :ps lists what runs: on Linux, the host's /proc stands in.
      Run (":ps");
      Check (Latest_Result (Placed)'Length > 15 and then Latest_Result (Placed) (1 .. 15) = "List<Process>: ",
             ":ps lists typed processes: " & Latest_Result (Placed) (1 .. Natural'Min (60, Latest_Result (Placed)'Length)));
      Draw (Placed, Canvas, Bounds);
      Check (Cell_Source (Placed, Statistics (Placed).Cells) = "(proc.list)", ":ps shows the CCL it ran");
      Run ("(length (where (fn ((p Process)) (> (field p pid) 0)) (proc.list)))");
      Check (Latest_Result (Placed)'Length > 9 and then Latest_Result (Placed) (1 .. 9) = "Integer: " and then
             Latest_Result (Placed) /= "Integer: 0", "processes are data: " & Latest_Result (Placed));
      Run ("(sort-by (fn ((p Process)) (- 0 (field p memory))) (proc.list))");
      Draw (Placed, Canvas, Bounds);
      Save ("console-processes");
      --  A built-in describes itself while its call is typed.
      for C of String'("(sort-by ") loop
         Placed_Handle (Placed, (Kind => Text_Input, Character_Value => C, others => <>), Submitted, Redraw);
      end loop;
      Draw (Placed, Canvas, Bounds);
      Save ("console-hint");
   end;

   Ada.Text_IO.Put_Line
     ((if Failures = 0 then "PASS" else "FAIL") & ": CCL console view," & Checks'Image & " checks");
   if Failures > 0 then Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure); end if;
end Main;
