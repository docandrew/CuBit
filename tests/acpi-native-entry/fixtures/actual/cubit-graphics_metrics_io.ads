with CuBit.Graphics_Metrics;
package CuBit.Graphics_Metrics_IO is
   type Reporter is private;
   --  Call only from periodic diagnostics, never once per row or input event.
   --  Emit a baseline (including zero), then only changed cumulative data.
   procedure Publish
     (Source : CuBit.Graphics_Metrics.Stage;
      Item : CuBit.Graphics_Metrics.Counter; State : in out Reporter);
private
   type Reporter is record
      Seen : Boolean := False;
      Last : CuBit.Graphics_Metrics.Counter;
   end record;
end CuBit.Graphics_Metrics_IO;
