pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Metric_Protocol;
with CuBit.Metric_Query_Pages;
with Metric_Scopes;
with Metric_Series;

--  metrics.svc's answers to List_Series, Read_Range and Read_Window
--  (CuBit.Metric_Query_Pages), from a private copy of the reader's page
--  into a page the caller then copies back. Every series is checked
--  against the reader's read scopes (Metric_Scopes): a series or name
--  outside them is refused (Out_Of_Scope); a listing or a merge over a
--  broader selection leaves out what the reader may not read.
package Metric_Reads is
   package Pages renames CuBit.Metric_Query_Pages;

   procedure List
     (Store : Metric_Series.Store; Scopes : Metric_Scopes.Table; Reader : Unsigned_64;
      Skip : Unsigned_64; Item : in out Pages.Page; Rows, Total : out Unsigned_64;
      Result : out CuBit.Metric_Protocol.Status);
   procedure Read_Range
     (Store : Metric_Series.Store; Scopes : Metric_Scopes.Table; Reader : Unsigned_64;
      Id, From_Us, To_Us : Unsigned_64; Item : in out Pages.Page;
      Rows, Total, Gaps, Resolution_Us : out Unsigned_64; Result : out CuBit.Metric_Protocol.Status);
   procedure Read_Window
     (Store : Metric_Series.Store; Scopes : Metric_Scopes.Table; Reader : Unsigned_64;
      Id, From_Us, To_Us : Unsigned_64; Item : in out Pages.Page;
      Merged : out Unsigned_64; Result : out CuBit.Metric_Protocol.Status);
end Metric_Reads;
