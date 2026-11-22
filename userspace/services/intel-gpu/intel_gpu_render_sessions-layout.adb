with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Render_Sessions.Layout with SPARK_Mode => Off is
   Entry_Bytes : constant Storage_Offset :=
     Storage_Offset ((Entries'Component_Size + System.Storage_Unit - 1) /
                     System.Storage_Unit);
   function Bytes_Through
     (Object : Registry; Base : System.Address; Count : Slot_Count)
      return Interfaces.Unsigned_64 is
      Last : constant System.Address :=
        Object.Items (Slot'First)'Address + Storage_Offset (Count) * Entry_Bytes;
   begin
      return Interfaces.Unsigned_64 (Last - Base);
   end Bytes_Through;
end Intel_GPU_Render_Sessions.Layout;
