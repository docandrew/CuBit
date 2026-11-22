with Interfaces; use Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CuBit.Metric_Protocol;
with Observatory_Metric_Summaries;
package Observatory_CCL with SPARK_Mode is
   package P renames CuBit.Metric_Protocol;
   type Operation is (Ready, Row_Count, Metric_Name, Unit_Name, Source, Publisher, Samples, Minimum, Maximum, P50, P99, Total, Dropped, Batch_Gaps, Lossy, Saturated);
   Binding_Base : constant Unsigned_32 := 16#4F42_0100#;
   function Binding (Op : Operation) return Unsigned_32 is
     (Binding_Base + Operation'Pos (Op));
   function Name (Op : Operation) return String;
   procedure Publish (Catalog : in out CCL.Catalog.Interface_Catalog;
      Error : out CCL.Catalog.Catalog_Error);
   type Context is private;
   -- Caller owns a private, retired page. Replacement is all-or-nothing;
   -- rejection invalidates the view so old observations cannot appear fresh.
   procedure Replace (Item : in out Context; Rows : P.Summary_Page;
      Count : P.Row_Count; Accepted : out Boolean);
   procedure Clear (Item : in out Context);
   procedure Invoke (Item : in out Context; Host_Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
private
   type Context is record
      Page : P.Summary_Page := [others => [others => 0]];
      Count : P.Row_Count := 0;
      Available : Boolean := False;
   end record with Type_Invariant =>
     (if Context.Available then
         (for all I in P.Row_Index =>
             (if I < Context.Count then
                 Observatory_Metric_Summaries.Valid_Row (Context.Page (I))))
      else Context.Count = 0);
end Observatory_CCL;
