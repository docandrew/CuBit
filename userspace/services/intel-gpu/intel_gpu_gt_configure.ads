with Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
generic
   with function Owner_Ready return Boolean;
   with function Read32 (Offset : Interfaces.Unsigned_32; MCR : Boolean)
     return Interfaces.Unsigned_32;
   with procedure Write32
     (Offset, Value : Interfaces.Unsigned_32; MCR : Boolean;
      Success : out Boolean);
package Intel_GPU_GT_Configure is
   type Result is (Rejected, Ownership_Lost, Read_Failed, Write_Failed,
                   Readback_Failed, Ready, Ready_With_Firmware_Override);
   type Attempt is limited private;
   procedure Configure
     (Object : in out Attempt;
      Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Topology : Intel_GPU_ADLN_Steering.Topology; Status : out Result);
   function Last_Offset (Object : Attempt) return Interfaces.Unsigned_32;
   function Last_Readback (Object : Attempt) return Interfaces.Unsigned_32;
private
   type Attempt is limited record
      Started : Boolean := False;
      Offset, Raw : Interfaces.Unsigned_32 := 0;
   end record;
end Intel_GPU_GT_Configure;
