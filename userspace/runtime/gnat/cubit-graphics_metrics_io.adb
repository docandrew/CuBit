with CuBit.Messages;
package body CuBit.Graphics_Metrics_IO is
   use CuBit.Graphics_Metrics;
   function Name (Source : Stage) return String is
     (case Source is
        when Desktop_Staging => "desktop_staging",
        when Display_Backend => "display_backend",
        when Display_Repair => "display_repair",
        when GPU_Upload_Request => "gpu_upload_request",
        when GPU_Legacy_Copy => "gpu_legacy_copy");
   procedure Publish
     (Source : Stage; Item : Counter; State : in out Reporter) is
   begin
      if not State.Seen or else Item /= State.Last then
         CuBit.Messages.debugPrint
           ("GRAPHICS: stage=" & Name (Source) &
            " bytes=" & Item.Bytes'Image & " regions=" & Item.Regions'Image &
            " overflow=" & (if Item.Overflowed then "1" else "0") & ASCII.LF);
         State := (True, Item);
      end if;
   end Publish;
end CuBit.Graphics_Metrics_IO;
