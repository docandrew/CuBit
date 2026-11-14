with CuBit.Handles.Startup;
with CuBit.Metric_Protocol;
with CuBit.Metric_Queries;
with CuBit.Monotonic;

--  metrics.svc through this program's metrics-observer endpoint (its
--  manifest's request and metrics scopes; procmgr grants it only from the
--  startup plan or with approval).
package body CCL_Metric_IO is
   use type CuBit.Metric_Protocol.Status;
   package Queries renames CuBit.Metric_Queries;

   Reader : constant Queries.Reader :=
     Queries.Bound_To (CuBit.Handles.Startup.Optional_Endpoint_Named (Queries.Observer_Binding));

   function Available return Boolean is (Queries.Available (Reader));
   function Now_Us return Unsigned_64 is (CuBit.Monotonic.Microseconds);

   function Match_Of (Match : Name_Match) return Queries.Name_Match is
     (case Match is
        when Any_Name => Queries.Any_Name, when Exact_Name => Queries.Exact_Name,
        when Name_Prefix => Queries.Name_Prefix);

   --  metrics.svc's status as a typed CCL failure.
   procedure Settle (Result : CuBit.Metric_Protocol.Status; Success : out Boolean;
                     Why : out CuBit.Failures.Failure);
   procedure Settle (Result : CuBit.Metric_Protocol.Status; Success : out Boolean;
                     Why : out CuBit.Failures.Failure) is
      package F renames CuBit.Failures;
   begin
      Success := Result = CuBit.Metric_Protocol.OK;
      Why := (case Result is
                when CuBit.Metric_Protocol.OK => F.Failed (F.Unavailable, ""),
                when CuBit.Metric_Protocol.Out_Of_Scope =>
                  F.Failed (F.Refused, "metrics: those series are outside this program's metrics scopes",
                            "its manifest's scopes name what it may read: (metrics ""desktop."") or (metrics-all)"),
                when CuBit.Metric_Protocol.Denied =>
                  F.Failed (F.Refused, "metrics: this program's endpoint lacks the metrics-observer badge",
                            "request (service ""metrics-observer"" ""metrics-observer"") from the startup plan"),
                when CuBit.Metric_Protocol.Not_Found => F.Failed (F.Not_Found, "metrics: no such series"),
                when CuBit.Metric_Protocol.Invalid_Request | CuBit.Metric_Protocol.Exhausted =>
                  F.Failed (F.Invalid_Argument, "metrics: metrics.svc refused the request as malformed"),
                when CuBit.Metric_Protocol.Unavailable =>
                  F.Failed (F.Unavailable, "metrics: metrics.svc did not answer"));
   end Settle;

   procedure List
     (Match : Name_Match; Name, Source : String; Skip : Unsigned_64; Answer : out Pages.Page;
      Rows, Total : out Unsigned_64; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Result : CuBit.Metric_Protocol.Status;
   begin
      Queries.List (Reader, Match_Of (Match), Name, Source, Skip, Answer, Rows, Total, Result);
      Settle (Result, Success, Why);
   end List;

   procedure Read_Range
     (Id, From_Us, To_Us, Skip, Step_Us : Unsigned_64; Answer : out Pages.Page;
      Rows, Total, Gaps, Resolution_Us : out Unsigned_64; Success : out Boolean;
      Why : out CuBit.Failures.Failure)
   is
      Result : CuBit.Metric_Protocol.Status;
   begin
      Queries.Read_Range (Reader, Id, From_Us, To_Us, Skip, Step_Us, Answer, Rows, Total, Gaps, Resolution_Us,
                          Result);
      Settle (Result, Success, Why);
   end Read_Range;

   procedure Read_Window
     (Match : Name_Match; Name, Source : String; From_Us, To_Us : Unsigned_64; Answer : out Pages.Page;
      Merged : out Unsigned_64; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Result : CuBit.Metric_Protocol.Status;
   begin
      Queries.Read_Window (Reader, 0, Match_Of (Match), Name, Source, From_Us, To_Us, Answer, Merged, Result);
      Settle (Result, Success, Why);
   end Read_Window;
end CCL_Metric_IO;
