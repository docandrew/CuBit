with Interfaces;
with Intel_GPU_Display_Topology;
with Intel_GPU_Cursor_Decode;
generic
   Item : Intel_GPU_Display_Topology.Pipe;
   with function Power_Held return Boolean;
package Intel_GPU_Native_Cursor is
   type Collection_Status is
     (Rejected, Power_Unavailable, Read_Failed, Access_End_Failed, Complete);
   type Observation is record
      Status : Collection_Status := Rejected;
      Collected : Boolean := False;
      Reads : Natural range 0 .. 8 := 0;
      Before, After : Intel_GPU_Cursor_Decode.Sample;
      Decoded : Intel_GPU_Cursor_Decode.Decoded;
   end record;
   function Diagnostic (Value : Observation) return String;
   -- Initial boot, serialized, all ADL-N pipes. Owner is trusted retained
   -- local display ownership, not an IPC argument. Power remains retained.
   function Inspect (Owner : Boolean; Table_Bytes : Interfaces.Unsigned_64)
     return Observation;
end Intel_GPU_Native_Cursor;
