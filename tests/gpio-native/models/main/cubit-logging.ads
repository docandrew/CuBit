with CuBit.Handles.Services;
with CuBit.Log_Records;
package CuBit.Logging is
   package Logstore_Handles renames CuBit.Handles.Services.Logstore;
   type Publisher is record Present : Boolean := False; end record;
   function Bound_To (Target : Logstore_Handles.Optional_Endpoint) return Publisher is ((Present => Target.Present));
   procedure Emit (Object : in out Publisher; Item : CuBit.Log_Records.Log_Record; Submitted : out Boolean);
   procedure Disconnect (Object : in out Publisher; Done : out Boolean);
end CuBit.Logging;
