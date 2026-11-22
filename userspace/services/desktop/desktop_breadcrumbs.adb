with Interfaces;
with Desktop_Logs;
package body Desktop_Breadcrumbs with SPARK_Mode,
  Refined_State => (State => Seen) is
 Seen : array (Stage) of Boolean := [others => False];
 function Name (Point : Stage) return String is
   (case Point is
      when Entered => "ENTERED",
      when Init_Before => "INIT_BEFORE",
      when Init_After => "INIT_AFTER",
      when Health_Before => "HEALTH_BEFORE",
      when Health_After => "HEALTH_AFTER",
      when Targets_Before => "TARGETS_BEFORE",
      when Targets_After => "TARGETS_AFTER",
      when Pipeline_Before => "PIPELINE_BEFORE",
      when Pipeline_After => "PIPELINE_AFTER",
      when Upload_Before => "UPLOAD_BEFORE",
      when Upload_After => "UPLOAD_AFTER",
      when Readback_Before => "READBACK_BEFORE",
      when Readback_After => "READBACK_AFTER",
      when Selected => "SELECTED",
      when Begin_Before => "BEGIN_BEFORE",
      when Begin_After => "BEGIN_AFTER",
      when Deferred => "DEFERRED",
      when Pending => "PENDING",
      when Unsafe => "UNSAFE",
      when Complete => "COMPLETE",
      when Submit => "SUBMIT",
      when Released => "RELEASED");
 procedure Mark (Point : Stage) is
   Elapsed : Interfaces.Unsigned_64;
 begin
   if Seen (Point) then return; end if;
   Seen (Point) := True;
   Elapsed := Desktop_Startup_Clock.Elapsed_Ms;
   Desktop_Logs.Write ("DESKTOP-CHECKPOINT: " & Name (Point) & " t=" &
     Interfaces.Unsigned_64'Image (Elapsed) & "ms" & ASCII.LF);
 end Mark;
end Desktop_Breadcrumbs;
