with CCL.Catalog;
with CCL.Catalog.Completion;
with CCL.Language;
with CCL.VM;
with CCL.Host_Values;
with Interfaces;

--  Transport/UI-independent REPL foundation. No IPC, filesystem, or window
--  dependency. A catalog is discovery data, not permission to invoke a service.
package CCL.Sessions with SPARK_Mode is
   Maximum_History : constant := 16;
   subtype History_Count is Natural range 0 .. Maximum_History;
   subtype History_Index is Positive range 1 .. Maximum_History;
   subtype Fuel_Budget is Natural range 0 .. 65_536;
   Default_Fuel : constant Fuel_Budget := 4_096;
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
   function Length (Item : Session) return History_Count;
   procedure Complete
     (Item : Session; Prefix : String;
      Matches : out CCL.Catalog.Completion.Match_List);
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

   function Result_Type (Outcome : CCL.Language.Interpretation_Result)
     return CCL.Language.Static_Type;
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Value : out CCL.Host_Values.Value;
         Success : out Boolean);
   procedure Submit_With_Values
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result)
     with Post => Outcome.Fuel_Remaining <= Fuel;
   function Result_Image (Outcome : CCL.Language.Interpretation_Result) return String;
private
   type History_Array is array (History_Index) of Submission;
   type Session is record
      Catalog : CCL.Catalog.Interface_Catalog;
      Entries : History_Array := [others => <>];
      Count : History_Count := 0;
      Oldest : History_Index := 1;
   end record;
end CCL.Sessions;
