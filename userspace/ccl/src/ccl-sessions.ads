with CuBit.Failures;
with CCL.Catalog;
with CCL.Catalog.Completion;
with CCL.Completions;
with CCL.Language;
with CCL.Language.Views;
with CCL.VM;
with CCL.Host_Values;
with CCL.Streams;
with Interfaces;

--  Transport/UI-independent REPL foundation. No IPC, filesystem, or window
--  dependency. A catalog is discovery data, not permission to invoke a service.
package CCL.Sessions with SPARK_Mode is
   Maximum_History : constant := 16;
   subtype History_Count is Natural range 0 .. Maximum_History;
   subtype History_Index is Positive range 1 .. Maximum_History;
   --  Fuel bounds work, not wall time: a REPL entry may do real work (sort a
   --  thousand items, map over a range) and still always ends.
   Maximum_Fuel : constant := 16_777_216;
   subtype Fuel_Budget is Natural range 0 .. Maximum_Fuel;
   Default_Fuel : constant Fuel_Budget := 1_000_000;
   type Submission is record
      Source : String (1 .. CCL.Language.MAX_SOURCE_LENGTH) := [others => ' '];
      Source_Length : Natural range 0 .. CCL.Language.MAX_SOURCE_LENGTH := 0;
      Source_Truncated : Boolean := False;
      Fuel : Fuel_Budget := 0;
      Outcome : CCL.Language.Interpretation_Result;
   end record;
   type Session is private;

   procedure Initialize (Item : out Session);
   procedure Initialize
     (Item : out Session; Visible_Interfaces : CCL.Catalog.Interface_Catalog);
   procedure Clear_History (Item : in out Session);

   --  The session environment (docs/ccl-repl.md, "Persistent session"):
   --  - definitions (define/type, or BASIC FUNCTION ... END) are kept as
   --    canonical Lisp and replayed ahead of every later entry; redefining a
   --    name replaces it in place;
   --  - named values, (define x expr) in Lisp or LET x = expr in BASIC, keep
   --    their value as a literal, so replay never repeats a host call.
   --  An entry changes the environment only if it succeeds. The commands
   --  :env (what is kept) and :reset (forget it all) are typed like entries.
   Maximum_Kept_Values : constant := 16;
   Maximum_Definitions_Length : constant := CCL.Language.MAX_SOURCE_LENGTH / 2;
   procedure Reset_Environment (Item : in out Session);
   --  The kept definitions as canonical Lisp source ("" if none), to save.
   function Definitions_Source (Item : Session) return String;
   --  A transcript entry produced outside evaluation (a front end's command,
   --  such as :save): Message is shown as its result.
   procedure Note
     (Item : in out Session; Source, Message : String;
      Outcome : out CCL.Language.Interpretation_Result);
   function Kept_Definitions (Item : Session) return Natural;
   function Kept_Values (Item : Session) return Natural;
   --  Whether a kept value is the stream Handle. A host closes the streams
   --  its session no longer holds (docs/ccl-streams.md).
   function Holds_Stream (Item : Session; Handle : CCL.Streams.Handle) return Boolean;
   --  The program this session runs for Source as a plain expression: its
   --  definitions and kept values around it. Plain is False when Source
   --  defines or binds something, or the program does not fit. A host runs
   --  it later (a live monitor) without changing the session.
   --  Source as Into writes it: Lisp or BASIC (CCL.Language.Views). An entry
   --  that names the session's definitions or kept values is rendered in
   --  their context, so (window 6 t) reads back as window(6, t). Converted
   --  is False when it cannot be rendered (the caller keeps Source).
   procedure View_Source
     (Item : Session; Source : String; Into : CCL.Language.Views.Surface;
      Shown : out CCL.Language.Views.Text; Converted : out Boolean);
   procedure Expression_Program
     (Item : Session; Source : String; Program : out String; Length : out Natural;
      Plain : out Boolean)
     with Pre => Program'First = 1;
   function Length (Item : Session) return History_Count;
   procedure Complete
     (Item : Session; Prefix : String;
      Matches : out CCL.Catalog.Completion.Match_List);
   --  Completion for the text before the caret, against this session's
   --  catalog (CCL.Completions).
   procedure Complete_At
     (Item : Session; Before : String; After_Caret : Character;
      Result : out CCL.Completions.Result);
   procedure Describe
     (Item : Session; Name : String;
      Operation : out CCL.Catalog.Resolved_Operation; Found : out Boolean);
   --  Oldest to newest; invalid lookups return Found=False and empty data.
   procedure Recall
     (Item : Session; Index : History_Index; Entry_Value : out Submission;
      Found : out Boolean);
   --  Evaluate exactly this source once. Older entries are never replayed.
   --  Invalid/oversized input is recorded as a diagnostic, not evaluated as a
   --  truncated expression. When full, the oldest history entry is evicted.
   procedure Submit
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Outcome : out CCL.Language.Interpretation_Result)
   with Post => Outcome.Fuel_Remaining <= Fuel;

   --  Grants and the statically bound host remain explicit per submission.
   --  Admit/execute once, then record one transcript entry; never retry effects.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.VM.Value; Value : out CCL.VM.Value; Success : out Boolean);
   procedure Submit_With_Host
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result)
     with Post => Outcome.Fuel_Remaining <= Fuel;

   --  A live cell: evaluate entry Index again against the current
   --  environment and replace its result in place. Only a plain expression
   --  is re-run: an entry that names a value, defines, or is a command is
   --  not (Reevaluated = False), so a live cell never changes the session
   --  environment or the history's order.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
      --  Answers stream views (docs/ccl-streams.md); none by default.
      with procedure Read_Stream
        (Context : in out Host_Context; Request : CCL.Streams.View_Request;
         Reply : in out CCL.Streams.View_Reply) is null;
   procedure Reevaluate_With_Values
     (Item : in out Session; Index : History_Index; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result; Reevaluated : out Boolean)
     with Post => Outcome.Fuel_Remaining <= Fuel;

   --  An entry that stopped at (wait t) on a pending task
   --  (Waiting_On_Task): run it again once t completes. Unlike a live cell
   --  this finishes the entry as its submission would have, so a name it
   --  defines is bound now. The caller answers the host calls the first run
   --  made from its log (CCL.Host_Replay), so none is made twice. Resumed
   --  is False when entry Index is not waiting on a task.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
      with procedure Read_Stream
        (Context : in out Host_Context; Request : CCL.Streams.View_Request;
         Reply : in out CCL.Streams.View_Reply) is null;
   procedure Resume_With_Values
     (Item : in out Session; Index : History_Index; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result; Resumed : out Boolean)
     with Post => Outcome.Fuel_Remaining <= Fuel;
   --  The entry waiting on task Waited_On, or 0 when none does.
   function Waiting_Entry (Item : Session; Waited_On : CCL.Streams.Handle) return History_Count;
   --  A waiting entry that cannot run again (its host calls were not all
   --  logged): it fails with Why instead of waiting for ever.
   procedure Abandon_Wait
     (Item : in out Session; Index : History_Index; Why : CuBit.Failures.Failure);

   function Result_Type (Outcome : CCL.Language.Interpretation_Result)
     return CCL.Language.Static_Type;
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
      with procedure Read_Stream
        (Context : in out Host_Context; Request : CCL.Streams.View_Request;
         Reply : in out CCL.Streams.View_Reply) is null;
   --  Shown, when given, is what the transcript records instead of Source
   --  (for example ":load tools.ccl" rather than the file's text).
   procedure Submit_With_Values
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result;
      Shown : String := "")
     with Post => Outcome.Fuel_Remaining <= Fuel;
   --  "Type: value", the transcript's one-line form of a result.
   function Result_Image (Outcome : CCL.Language.Interpretation_Result) return String;
   --  Its parts: the result's type as written ("" for a failure or plain
   --  "ok"), and its value (or the failure's message).
   function Result_Type_Image (Outcome : CCL.Language.Interpretation_Result) return String;
   function Result_Value_Image (Outcome : CCL.Language.Interpretation_Result) return String;
private
   type History_Array is array (History_Index) of Submission;
   Maximum_Literal_Length : constant := 2 * CCL.Language.MAX_TEXT_BYTES;
   type Kept_Value is record
      Name : String (1 .. CCL.Language.MAX_NAME_LENGTH) := [others => ' '];
      Name_Length : Natural range 0 .. CCL.Language.MAX_NAME_LENGTH := 0;
      Literal : String (1 .. Maximum_Literal_Length) := [others => ' '];
      Literal_Length : Natural range 0 .. Maximum_Literal_Length := 0;
      --  A stream binding: the handle its literal names.
      Stream : CCL.Streams.Handle := CCL.Streams.No_Handle;
   end record;
   type Kept_Value_Array is array (1 .. Maximum_Kept_Values) of Kept_Value;
   type Session is record
      Catalog : CCL.Catalog.Interface_Catalog;
      Entries : History_Array := [others => <>];
      Count : History_Count := 0;
      Oldest : History_Index := 1;
      --  Kept definitions: canonical Lisp top-level forms, space-separated.
      Definitions : String (1 .. Maximum_Definitions_Length) := [others => ' '];
      Definitions_Length : Natural range 0 .. Maximum_Definitions_Length := 0;
      Definition_Count : Natural range 0 .. CCL.Language.MAX_FUNCTIONS := 0;
      Values : Kept_Value_Array := [others => <>];
      Value_Count : Natural range 0 .. Maximum_Kept_Values := 0;
   end record;
end CCL.Sessions;
