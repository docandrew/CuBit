with Interfaces;
with Intel_GPU_ADLN_Inventory;
generic
   -- Exclusive reset engine, retained forcewake, MOCS table established.
   with function Owner_Ready return Boolean;
   -- MCR reads select an enabled instance. MCR writes MUST multicast, not
   -- write only the read-selected instance. Adapter owns/restores steering.
   with function Read32 (Offset : Interfaces.Unsigned_32; MCR : Boolean)
     return Interfaces.Unsigned_32;
   with procedure Write32
     (Offset, Value : Interfaces.Unsigned_32; MCR : Boolean;
      Success : out Boolean);
package Intel_GPU_Engine_Configure is
   type Result is (Rejected, Ownership_Lost, Read_Failed, Write_Failed,
                   Readback_Failed, Ready);
   type Attempt is limited private;
   procedure Configure
     (Object : in out Attempt;
      Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Engine : Intel_GPU_ADLN_Inventory.Engine; Status : out Result);
private
   type Attempt is limited record
      Started : Boolean := False;
   end record;
end Intel_GPU_Engine_Configure;
