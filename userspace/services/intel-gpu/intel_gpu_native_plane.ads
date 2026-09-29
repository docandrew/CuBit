with Interfaces;
with Intel_GPU_Display_Topology;
with Intel_GPU_Plane_Decode;
with Intel_GPU_Plane_Registers;
generic
   Item : Intel_GPU_Display_Topology.Pipe;
   with function Power_Held return Boolean;
package Intel_GPU_Native_Plane is
   type Collection_Status is
     (Rejected, Power_Unavailable, Read_Failed, Access_End_Failed, Complete);
   type Observation is record
      Status : Collection_Status := Rejected;
      Collected : Boolean := False;
      Reads : Natural range 0 .. 12 := 0;
      Before, After : Intel_GPU_Plane_Decode.Sample;
      Decoded : Intel_GPU_Plane_Decode.Decoded;
   end record;
   -- Explicit text, independent of the freestanding enum Image implementation.
   function Diagnostic (Value : Observation) return String;
   -- ADL-N planes 1..5, all four pipes, serialized initial boot invocation.
   -- Owner is retained local display ownership, never an IPC-supplied claim.
   -- No cursor coverage or GPU-address ownership is implied.
   function Inspect (Owner : Boolean; Table_Bytes : Interfaces.Unsigned_64;
                     Plane : Intel_GPU_Plane_Registers.Plane_Number)
     return Observation;
end Intel_GPU_Native_Plane;
