with Interfaces;
with CuBit.Failures;
with CuBit.Metric_Query_Pages;
--  The CCL hosts' view of metrics.svc (MET-002; platform bodies: native/,
--  and the hosted preview's host/, which has none).
package CCL_Metric_IO is
   use Interfaces;
   package Pages renames CuBit.Metric_Query_Pages;
   --  Whether this process has a metrics-observer endpoint at all.
   function Available return Boolean;
   --  Microseconds of the published monotonic clock, the series' clock.
   function Now_Us return Unsigned_64;
   type Name_Match is (Any_Name, Exact_Name, Name_Prefix);
   --  The pages of CuBit.Metric_Query_Pages, answered by metrics.svc.
   procedure List
     (Match : Name_Match; Name, Source : String; Skip : Unsigned_64; Answer : out Pages.Page;
      Rows, Total : out Unsigned_64; Success : out Boolean; Why : out CuBit.Failures.Failure);
   procedure Read_Range
     (Id, From_Us, To_Us, Skip, Step_Us : Unsigned_64; Answer : out Pages.Page;
      Rows, Total, Gaps, Resolution_Us : out Unsigned_64; Success : out Boolean;
      Why : out CuBit.Failures.Failure);
   procedure Read_Window
     (Match : Name_Match; Name, Source : String; From_Us, To_Us : Unsigned_64; Answer : out Pages.Page;
      Merged : out Unsigned_64; Success : out Boolean; Why : out CuBit.Failures.Failure);
end CCL_Metric_IO;
