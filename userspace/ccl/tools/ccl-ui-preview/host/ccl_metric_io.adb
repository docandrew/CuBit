--  The Linux preview has no metrics.svc: a Linux-hosted stand-in, not
--  CuBit. metrics.* is not published there.
package body CCL_Metric_IO is
   NO_METRICS : constant CuBit.Failures.Failure :=
     CuBit.Failures.Failed (CuBit.Failures.Unavailable, "metrics: the Linux preview has no metrics.svc");
   function Available return Boolean is (False);
   function Now_Us return Unsigned_64 is (0);
   procedure List
     (Match : Name_Match; Name, Source : String; Skip : Unsigned_64; Answer : out Pages.Page;
      Rows, Total : out Unsigned_64; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      pragma Unreferenced (Match, Name, Source, Skip);
   begin
      Answer := [others => 0];
      Rows := 0;
      Total := 0;
      Success := False;
      Why := NO_METRICS;
   end List;
   procedure Read_Range
     (Id, From_Us, To_Us, Skip, Step_Us : Unsigned_64; Answer : out Pages.Page;
      Rows, Total, Gaps, Resolution_Us : out Unsigned_64; Success : out Boolean;
      Why : out CuBit.Failures.Failure)
   is
      pragma Unreferenced (Id, From_Us, To_Us, Skip, Step_Us);
   begin
      Answer := [others => 0];
      Rows := 0;
      Total := 0;
      Gaps := 0;
      Resolution_Us := 0;
      Success := False;
      Why := NO_METRICS;
   end Read_Range;
   procedure Read_Window
     (Match : Name_Match; Name, Source : String; From_Us, To_Us : Unsigned_64; Answer : out Pages.Page;
      Merged : out Unsigned_64; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      pragma Unreferenced (Match, Name, Source, From_Us, To_Us);
   begin
      Answer := [others => 0];
      Merged := 0;
      Success := False;
      Why := NO_METRICS;
   end Read_Window;
end CCL_Metric_IO;
