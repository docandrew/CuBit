with Interfaces; use Interfaces;
with CuBit.Metric_Protocol;
with CuBit.Metric_Records;

-- Observer-side validation only. Identity is a collector claim until the IPC
-- adapter authenticates the reply. No allocation, IPC, JSON or producer work.
package Observatory_Metric_Summaries with SPARK_Mode is
   package P renames CuBit.Metric_Protocol;
   package R renames CuBit.Metric_Records;
   use type R.Record_Kind;
   function Valid_Row (Row : P.Summary_Row) return Boolean
     with Annotate => (GNATprove, Inline_For_Proof);
   type View is private;
   function Valid (Item : View) return Boolean;
   function Word (Item : View; Index : P.Row_Word_Index) return Unsigned_64;
   -- A validated view retains all 64 bits, including IDs, loss and saturation.
   -- Percentiles remain histogram upper bounds; never label them exact values.
   type Decoded (Success : Boolean := False) is record
      case Success is
         when False => null;
         when True => Value : View;
      end case;
   end record;
   function Decode (Row : P.Summary_Row) return Decoded
     with Post => Decode'Result.Success = Valid_Row (Row) and then
       (if Decode'Result.Success then
           Valid (Decode'Result.Value) and then
           (for all I in P.Row_Word_Index =>
               Word (Decode'Result.Value, I) = Row (I)));
   function Declaration (Item : View) return R.Decoded_Record
     with Pre => Valid (Item),
          Post => Declaration'Result.Success and then
            Declaration'Result.Value.Kind = R.Describe;
private
   function Metadata (Row : P.Summary_Row) return R.Decoded_Record;
   function Valid_Row (Row : P.Summary_Row) return Boolean is
     (declare D : constant R.Decoded_Record := Metadata (Row);
      begin D.Success and then D.Value.Kind = R.Describe and then
        Row (P.Row_Source) /= 0 and then
        P.Is_Publisher (Row (P.Row_Publisher_Tag)) and then
        (Row (P.Row_Flags) and not
           (P.Flag_Total_Saturated or P.Flag_Histogram_Saturated)) = 0 and then
        (for all I in 21 .. 23 => Row (I) = 0) and then
        (for all I in 28 .. 31 => Row (I) = 0) and then
        (if Row (P.Row_Count_Word) = 0 then
            (for all I in P.Row_Minimum .. P.Row_P999 => Row (I) = 0)
         else Row (P.Row_Minimum) <= Row (P.Row_Maximum) and then
           Row (P.Row_Minimum) <= Row (P.Row_P50) and then
           (for all I in P.Row_P50 .. P.Row_P999 - 1 =>
               Row (I) <= Row (I + 1))));

   type View is record
      Words : P.Summary_Row := [others => 0];
   end record;
   function Valid (Item : View) return Boolean is (Valid_Row (Item.Words));
   function Word (Item : View; Index : P.Row_Word_Index) return Unsigned_64 is
     (Item.Words (Index));
end Observatory_Metric_Summaries;
