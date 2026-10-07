--  Stream views in both engines (docs/ccl-streams.md, phase 1): every
--  expression runs in the interpreter and as verified bytecode against the
--  same session table, and both must give the same answer or failure.
with CCL.Evaluation;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Host_Values;
with CCL.Language;
with CCL.Objects;
with CCL.Streams;
with CCL.VM;
with CCL.VM.Native_Objects;

procedure Main is
   package L renames CCL.Language;
   package S renames CCL.Streams;
   package N renames CCL.VM.Native_Objects;
   use type L.Interpretation_Status;
   use type L.Analysis_Status;
   use type L.Diagnostic_Code;
   use type CCL.VM.Execution_Status;
   use type CCL.VM.Value_Kind;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.VM.Validation_Error;
   use type CCL.Objects.Build_Result;
   use type S.View_Kind;

   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("PASS " & Name);
      else
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   --  The session's table. 1: Integers 10 20 30 40, 2 lost to overflow.
   --  2: an Integer stream nothing has arrived on. 3: Strings.
   --  4: (P x y) records.
   TICKS : constant := 1;
   QUIET : constant := 2;
   WORDS : constant := 3;
   POINTS : constant := 4;
   --  Tasks: 5 completed with 42, 6 still pending, 7 completed with (P 5 6).
   DONE : constant := 5;
   PENDING : constant := 6;
   POINT_DONE : constant := 7;
   --  8: completed with (O.Finished (P 5 6)); 9: completed with (O.Stopped).
   FINISHED_DONE : constant := 8;
   STOPPED_DONE : constant := 9;
   type Integer_Ring is array (Positive range <>) of Integer_64;
   Tick_Ring : constant Integer_Ring := [10, 20, 30, 40];
   Tick_Losses : constant := 2;
   type Word is record
      Text : String (1 .. 5);
      Length : Natural;
   end record;
   Word_Ring : constant array (1 .. 3) of Word :=
     [("alpha", 5), ("beta ", 4), ("gamma", 5)];
   Point_Ring : constant array (1 .. 2, 1 .. 2) of Integer_64 := [[1, 2], [3, 4]];

   type Session is record
      Reads : Natural := 0;
   end record;

   procedure Read
     (Context : in out Session; Request : S.View_Request; Reply : in out S.View_Reply)
   is
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Add (Value : CCL.Objects.Cell) is
      begin
         if Built = CCL.Objects.Added then
            CCL.Objects.Append (Reply.Elements, Value, Built);
         end if;
      end Add;
      procedure Add_Text (Value : String) is
      begin
         if Built = CCL.Objects.Added then
            CCL.Objects.Append_Text (Reply.Elements, Value, Built);
         end if;
      end Add_Text;
      --  The newest Count of Total elements, oldest first; Latest alone
      --  is not a list.
      procedure Elements (Total : Natural; Put : not null access procedure (Index : Positive)) is
         Shown : constant Natural := Natural'Min (Request.Count, Total);
      begin
         if Request.View = S.Latest_View then
            if Total = 0 then
               Reply.Status := S.Stream_Empty;
            else
               Put (Total);
            end if;
         else
            Add (CCL.Objects.Sequence_Cell (Shown));
            for I in Total - Shown + 1 .. Total loop
               Put (I);
            end loop;
         end if;
      end Elements;
      procedure Put_Tick (Index : Positive) is
      begin
         Add (CCL.Objects.Integer_Cell (Tick_Ring (Index)));
      end Put_Tick;
      procedure Put_Word (Index : Positive) is
      begin
         Add_Text (Word_Ring (Index).Text (1 .. Word_Ring (Index).Length));
      end Put_Word;
      procedure Put_Point (Index : Positive) is
      begin
         Add (CCL.Objects.Product_Cell (2));
         Add (CCL.Objects.Integer_Cell (Point_Ring (Index, 1)));
         Add (CCL.Objects.Integer_Cell (Point_Ring (Index, 2)));
      end Put_Point;
      procedure Put_Nothing (Index : Positive) is null;
   begin
      Context.Reads := Context.Reads + 1;
      Reply.Status := S.View_Answered;
      if Request.View = S.Wait_View then
         case Request.Stream is
            when DONE => Add (CCL.Objects.Integer_Cell (42));
            when PENDING => Reply.Status := S.Stream_Empty;
            when POINT_DONE =>
               Add (CCL.Objects.Product_Cell (2));
               Add (CCL.Objects.Integer_Cell (5));
               Add (CCL.Objects.Integer_Cell (6));
            when FINISHED_DONE =>
               Add (CCL.Objects.Variant_Cell (1));
               Add (CCL.Objects.Product_Cell (2));
               Add (CCL.Objects.Integer_Cell (5));
               Add (CCL.Objects.Integer_Cell (6));
            when STOPPED_DONE =>
               Add (CCL.Objects.Variant_Cell (2));
               Add (CCL.Objects.Unit_Cell);
            --  A stream's handle (or none): as the session table answers.
            when others => Reply.Status := S.No_Such_Stream;
         end case;
         return;
      end if;
      case Request.Stream is
         when TICKS =>
            Reply.Total := (if Request.View = S.Lost_View then Tick_Losses
                            else Tick_Ring'Length + Tick_Losses);
            Elements (Tick_Ring'Length, Put_Tick'Access);
         when QUIET =>
            Reply.Total := 0;
            Elements (0, Put_Nothing'Access);
         when WORDS =>
            Reply.Total := (if Request.View = S.Lost_View then 0 else Word_Ring'Length);
            Elements (Word_Ring'Length, Put_Word'Access);
         when POINTS =>
            Reply.Total := (if Request.View = S.Lost_View then 0 else Point_Ring'Length (1));
            Elements (Point_Ring'Length (1), Put_Point'Access);
         when others =>
            Reply.Status := S.No_Such_Stream;
      end case;
      if Built /= CCL.Objects.Added then
         Reply.Status := S.No_Such_Stream;
      end if;
   end Read;

   procedure Deny
     (Context : in out Session; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context, Binding, Argument);
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
   end Deny;

   procedure Interpret is new CCL.Evaluation.Evaluate_With_Values (Session, Deny, Read_Stream => Read);

   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   FUEL : constant := 100_000;

   Prelude : constant String :=
     "(type P (record (x Integer) (y Integer))) (type O (variant (Finished P) (Stopped))) " &
     "(let ((ticks (stream Integer 1))) (let ((quiet (stream Integer 2))) " &
     "(let ((words (stream String 3))) (let ((points (stream P 4))) ";
   Closing : constant String := "))))";
   function Twenty_Reads return String is
     ("(latest ticks) (latest ticks) (latest ticks) (latest ticks) (latest ticks) " &
      "(latest ticks) (latest ticks) (latest ticks) (latest ticks) (latest ticks) " &
      "(latest ticks) (latest ticks) (latest ticks) (latest ticks) (latest ticks) " &
      "(latest ticks) (latest ticks) (latest ticks) (latest ticks) (latest ticks)");

   procedure Run_Both
     (Body_Text : String; Interpreted : out L.Interpretation_Result;
      Executed : out CCL.VM.Execution_Result; Compiled_Ok : out Boolean)
   is
      Source : constant String := Prelude & Body_Text & Closing;
      Context : Session;
      Analysis : L.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Checked : CCL.VM.Validated_Program;
      Error : CCL.VM.Validation_Error;
      Machine : N.Machine;
      Reply : S.View_Reply;
   begin
      Interpret (Source, FUEL, Catalog, Grants, Context, Interpreted);
      Executed := (others => <>);
      Compiled_Ok := False;
      L.Analyze (Source, Catalog, Analysis);
      if L.Analysis_Status_Of (Analysis) /= L.Analysis_Succeeded then return; end if;
      CCL.Compiler.Compile (Analysis, Compiled);
      if Compiled.Status /= CCL.Compiler.Compilation_Succeeded then
         Put_Line ("  compile: " & CCL.Compiler.Compilation_Status'Image (Compiled.Status));
         return;
      end if;
      CCL.VM.Verify (Compiled.Program, Checked, Error);
      if Error /= CCL.VM.Valid then
         Put_Line ("  verify: " & CCL.VM.Validation_Error'Image (Error));
         return;
      end if;
      Compiled_Ok := True;
      N.Initialize (Checked, FUEL, Machine);
      loop
         N.Continue_Execution_For (Checked, Machine, FUEL, Executed);
         exit when Executed.Status /= CCL.VM.Waiting_For_Host or else not Executed.Stream_Requested;
         Reply := (others => <>);
         Read (Context, Executed.Stream_Request, Reply);
         --  A pending task: the host does not answer, so the VM stays
         --  suspended on it (a live host answers when it completes).
         exit when S."=" (Executed.Stream_Request.View, S.Wait_View) and then
                   S."=" (Reply.Status, S.Stream_Empty);
         N.Complete_Stream_View (Checked, Machine, Reply);
      end loop;
   end Run_Both;

   procedure Expect_Integer (Body_Text : String; Value : Integer_64; Name : String) is
      Interpreted : L.Interpretation_Result;
      Executed : CCL.VM.Execution_Result;
      Compiled_Ok : Boolean;
   begin
      Run_Both (Body_Text, Interpreted, Executed, Compiled_Ok);
      if Interpreted.Status /= L.Succeeded or else Executed.Status /= CCL.VM.Completed then
         Put_Line ("  interpreter " & L.Interpretation_Status'Image (Interpreted.Status) & " " &
                   L.Diagnostic_Code'Image (Interpreted.Diagnostic) &
                   ", bytecode " & CCL.VM.Execution_Status'Image (Executed.Status));
      end if;
      Check (Interpreted.Status = L.Succeeded and then Interpreted.Result_Value.Integer = Value,
             Name & " (interpreter)");
      Check (Compiled_Ok and then Executed.Status = CCL.VM.Completed and then Executed.Has_Value and then
             Executed.Result_Value.Kind = CCL.VM.Integer_Value and then
             Executed.Result_Value.Integer = Value, Name & " (bytecode)");
   end Expect_Integer;

   procedure Expect_Text (Body_Text : String; Value : String; Name : String) is
      Interpreted : L.Interpretation_Result;
      Executed : CCL.VM.Execution_Result;
      Compiled_Ok : Boolean;
   begin
      Run_Both (Body_Text, Interpreted, Executed, Compiled_Ok);
      Check (Interpreted.Status = L.Succeeded and then Interpreted.Has_Text and then
             Interpreted.Result_Text.Data (1 .. Interpreted.Result_Text.Length) = Value,
             Name & " (interpreter)");
      Check (Compiled_Ok and then Executed.Status = CCL.VM.Completed and then Executed.Has_Result_Text and then
             Executed.Result_Text_Value.Data (1 .. Executed.Result_Text_Value.Length) = Value,
             Name & " (bytecode)");
   end Expect_Text;

   procedure Expect_Failure
     (Body_Text : String; Interpreted_Status : L.Interpretation_Status;
      Executed_Status : CCL.VM.Execution_Status; Name : String)
   is
      Interpreted : L.Interpretation_Result;
      Executed : CCL.VM.Execution_Result;
      Compiled_Ok : Boolean;
   begin
      Run_Both (Body_Text, Interpreted, Executed, Compiled_Ok);
      Check (Interpreted.Status = Interpreted_Status, Name & " (interpreter)");
      Check (Compiled_Ok and then Executed.Status = Executed_Status and then not Executed.Has_Value,
             Name & " (bytecode)");
   end Expect_Failure;

   procedure Expect_Refused (Source : String; Diagnostic : L.Diagnostic_Code; Name : String) is
      Interpreted : L.Interpretation_Result;
      Context : Session;
   begin
      Interpret (Source, FUEL, Catalog, Grants, Context, Interpreted);
      Check (Interpreted.Status in L.Parse_Failed | L.Type_Check_Failed and then
             Interpreted.Diagnostic = Diagnostic, Name);
      Check (Context.Reads = 0, Name & ": refused before any read");
   end Expect_Refused;
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);

   --  Views.
   Expect_Integer ("(latest ticks)", 40, "latest is the newest element");
   Expect_Integer ("(sum (window 3 ticks))", 90, "window is the newest n, as a list");
   Expect_Integer ("(sum (first 1 (window 3 ticks)))", 20, "a window is oldest first");
   Expect_Integer ("(length (window 255 ticks))", 4, "a window holds what arrived, at most n");
   Expect_Integer ("(arrived ticks)", 6, "arrived counts every delivery");
   Expect_Integer ("(lost ticks)", 2, "lost counts the ring's drops");
   Expect_Integer ("(length (window 5 quiet))", 0, "an empty stream has an empty window");
   Expect_Integer ("(arrived quiet)", 0, "nothing arrived yet");
   Expect_Text ("(latest words)", "gamma", "a String element");
   Expect_Text ("(join ""+"" (window 2 words))", "beta+gamma", "a window of Strings");
   Expect_Integer ("(field (latest points) y)", 4, "a record element is copied in");
   Expect_Integer ("(fold (fn ((a Integer) (p P)) (+ a (field p x))) 0 (window 2 points))", 4,
                   "a window of records");
   Expect_Integer ("(+ (latest ticks) (latest ticks))", 80, "a stream read twice");
   --  More reads than an evaluation has object slots (16).
   Expect_Integer ("(sum [" & Twenty_Reads & "])", 800, "latest read 20 times does not use up object slots");

   --  Streams as values: through functions, conditionals and lets.
   Expect_Integer ("(let ((pick (fn ((s (Stream Integer))) (latest s)))) (pick ticks))", 40,
                   "a stream passed to a function");
   Expect_Integer ("(latest (if (> (arrived quiet) 0) quiet ticks))", 40,
                   "a stream chosen by a conditional");

   --  Failures are typed, and the same in both engines.
   Expect_Failure ("(latest quiet)", L.Stream_Empty, CCL.VM.Stream_Empty,
                   "latest before the first element");
   Expect_Failure ("(latest (stream Integer 9))", L.Stream_Unavailable, CCL.VM.Stream_Unavailable,
                   "a stream the session does not hold");
   Expect_Failure ("(sum (window 0 ticks))", L.Stream_Window_Out_Of_Range,
                   CCL.VM.Stream_Window_Out_Of_Range, "an empty window request");
   Expect_Failure ("(sum (window 256 ticks))", L.Stream_Window_Out_Of_Range,
                   CCL.VM.Stream_Window_Out_Of_Range, "a window beyond what one image holds");
   Expect_Failure ("(latest (stream Integer 3))", L.Stream_Element_Mismatch,
                   CCL.VM.Stream_Element_Mismatch, "Strings named as Integers are refused");
   Expect_Failure ("(field (latest (stream P 1)) x)", L.Stream_Element_Mismatch,
                   CCL.VM.Stream_Element_Mismatch, "Integers named as records are refused");

   --  The handle is opaque: no arithmetic, comparison, printing or storage.
   Expect_Refused ("(+ (stream Integer 1) 1)", L.Expected_Integer, "no arithmetic on a stream");
   Expect_Refused ("(= (stream Integer 1) (stream Integer 1))", L.Expected_Comparable,
                   "no comparison of streams");
   Expect_Refused ("(to-string (stream Integer 1))", L.Expected_Printable, "a stream does not print");
   Expect_Refused ("[(stream Integer 1)]", L.Unsupported_List_Element, "no list of streams");
   Expect_Refused ("(latest 5)", L.Expected_Stream, "latest needs a stream");
   Expect_Refused ("(window ""a"" (stream Integer 1))", L.Expected_Integer, "a window count is an Integer");
   Expect_Refused ("(stream Integer x)", L.Expected_Stream, "a stream number is a literal");
   Expect_Refused ("(stream Integer 0)", L.Value_Out_Of_Range, "stream numbers start at 1");
   Expect_Refused ("(stream (Stream Integer) 1)", L.Unsupported_Stream_Element, "no stream of streams");

   --  Tasks (Task<T>, docs/control-language.md): wait a completed task in
   --  both engines; a pending one stops the interpreter (the session runs
   --  the entry again) and suspends the VM on that task.
   Expect_Integer ("(wait (task Integer 5))", 42, "wait a completed task");
   Expect_Integer ("(field (wait (task P 7)) y)", 6, "wait a task of a record");
   Expect_Integer ("(match (wait (task O 8)) ((O.Finished p) (field p y)) ((O.Stopped) -1))", 6,
                   "wait a task of a variant with a record payload");
   Expect_Integer ("(match (wait (task O 9)) ((O.Finished p) (field p y)) ((O.Stopped) -1))", -1,
                   "wait a task of a variant without payload");
   declare
      Interpreted : L.Interpretation_Result;
      Executed : CCL.VM.Execution_Result;
      Compiled_Ok : Boolean;
   begin
      Run_Both ("(+ 1 (wait (task Integer 6)))", Interpreted, Executed, Compiled_Ok);
      Check (Interpreted.Status = L.Waiting_On_Task and then
             S."=" (Interpreted.Waited_On, S.Handle (PENDING)),
             "a pending task stops the interpreter, naming it");
      Check (Compiled_Ok and then Executed.Status = CCL.VM.Waiting_For_Host and then
             Executed.Stream_Requested and then
             S."=" (Executed.Stream_Request.View, S.Wait_View) and then
             S."=" (Executed.Stream_Request.Stream, S.Handle (PENDING)),
             "a pending task suspends the VM on it");
   end;
   --  A handle written as the other kind names nothing: an error, never a
   --  wait that cannot end or another kind's elements.
   Expect_Failure ("(wait (task Integer 1))", L.Stream_Unavailable, CCL.VM.Stream_Unavailable,
                   "waiting on a stream's handle");
   Expect_Failure ("(latest (stream Integer 5))", L.Stream_Unavailable, CCL.VM.Stream_Unavailable,
                   "a stream view of a task's handle");
   Expect_Refused (Prelude & "(latest (task Integer 5))" & Closing, L.Expected_Stream,
                   "a task has no latest element");
   Expect_Refused (Prelude & "(wait ticks)" & Closing, L.Expected_Task,
                   "a stream is not waited for");
   Expect_Refused ("(type R (record (t (Task Integer)))) 0", L.Stream_Not_Data,
                   "a task is not a record field");

   if Failures = 0 then
      Put_Line ("ccl-streams: all passed");
   else
      Put_Line ("ccl-streams:" & Natural'Image (Failures) & " failed");
      Set_Exit_Status (Failure);
   end if;
end Main;
