--  The hosted preview has no logstore: logs.recent is not offered.
package body CCL_Log_IO is
   function Available return Boolean is (False);
   procedure Announce (Text : String) is null;
   procedure Recent
     (Service : String; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Image; Success : out Boolean;
      Why : out CuBit.Failures.Failure)
   is
      pragma Unreferenced (Service);
   begin
      Image := CCL.Objects.Empty (Contract);
      Success := False;
      Why := CuBit.Failures.Failed
        (CuBit.Failures.Unavailable, "the hosted preview has no log store");
   end Recent;

   procedure Minimum
     (Level : out CCL.Interfaces.Logs.Severity; Success : out Boolean; Why : out CuBit.Failures.Failure) is
   begin
      Level := CCL.Interfaces.Logs.Trace;
      Success := False;
      Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "the hosted preview has no log store");
   end Minimum;

   procedure Set_Minimum
     (Level : CCL.Interfaces.Logs.Severity; Previous : out CCL.Interfaces.Logs.Severity;
      Success : out Boolean; Why : out CuBit.Failures.Failure) is
   begin
      Previous := Level;
      Success := False;
      Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "the hosted preview has no log store");
   end Set_Minimum;
end CCL_Log_IO;
