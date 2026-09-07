with CCL.Catalog;
with CCL.Language;

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

   function Result_Type (Outcome : CCL.Language.Interpretation_Result)
     return CCL.Language.Static_Type;
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
