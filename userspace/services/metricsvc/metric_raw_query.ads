with Interfaces; use Interfaces;
with Metric_Store;
with CuBit.Metric_Protocol;
package Metric_Raw_Query with SPARK_Mode is
   package P renames CuBit.Metric_Protocol;
   procedure Fill (Store : Metric_Store.Store; Cursor : Unsigned_64;
      Page : out P.Raw_Page; Written : out P.Raw_Row_Count;
      Next, Gap : out Unsigned_64; Valid : out Boolean)
     with Pre => Metric_Store.Valid (Store),
       Post => (if Valid then Next >= Cursor else Written = 0 and Next = Cursor and Gap = 0);
end Metric_Raw_Query;
