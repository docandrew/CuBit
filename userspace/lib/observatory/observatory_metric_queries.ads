with Interfaces; use Interfaces;
with CuBit.Metric_Protocol;
with Observatory_Metric_Summaries;

-- Pure observer reply admission. The asynchronous adapter must first match a
-- live, non-reused request token and establish grant quiescence before copying
-- rows. A deadline never permits reusing memory still owned by the collector.
package Observatory_Metric_Queries with SPARK_Mode is
   package P renames CuBit.Metric_Protocol;
   -- Viewer work budget, not an authority or a guessed collector allocation.
   Maximum_Series : constant := 512;
   subtype Cursor is Unsigned_64 range 0 .. Maximum_Series;
   type Words is array (0 .. 3) of Unsigned_64;
   type Reply is record
      Kernel_Valid : Boolean := False;
      Kernel_Status : Unsigned_64 := 0;
      Label : Unsigned_32 := 0;
      Length, Flags : Unsigned_8 := 0;
      Reserved : Unsigned_16 := 0;
      Payload : Words := [others => 0];
   end record;
   -- OK: written, next ordinal, total ordinal slots, zero.
   -- Progress must cover at least the returned rows. Only a full page can
   -- continue; empty/non-progressing continuations would spin the reader.
   function Admitted (Value : Reply; Requested : Cursor) return Boolean is
     (Value.Kernel_Valid and then Value.Kernel_Status = 0 and then
      Value.Label = P.Status'Enum_Rep (P.OK) and then
      Value.Length = P.Message_Words and then Value.Flags = 0 and then
      Value.Reserved = 0 and then Value.Payload (3) = 0 and then
      Value.Payload (0) <= P.Rows_Per_Page and then
      Value.Payload (2) <= Maximum_Series and then
      Requested <= Value.Payload (1) and then
      Value.Payload (1) <= Value.Payload (2) and then
      Value.Payload (0) <= Value.Payload (1) - Requested and then
      (if Value.Payload (1) < Value.Payload (2) then
          Value.Payload (0) = P.Rows_Per_Page))
     with Post => (if Admitted'Result then
        Value.Payload (0) <= P.Rows_Per_Page and
        Requested <= Value.Payload (1) and
        Value.Payload (1) <= Value.Payload (2) and
        Value.Payload (2) <= Maximum_Series and
        (Value.Payload (1) = Value.Payload (2) or Value.Payload (1) > Requested));
   function Valid_Page
     (Value : Reply; Requested : Cursor; Rows : P.Summary_Page) return Boolean
     with Post => (if Valid_Page'Result then
       Admitted (Value, Requested) and then
       (for all I in P.Row_Index =>
           (if Unsigned_64 (I) < Value.Payload (0) then
               Observatory_Metric_Summaries.Valid_Row (Rows (I)))));
end Observatory_Metric_Queries;
