with Interfaces;
package CuBit.Handles.IPC is
   type Description is record
      Current : Boolean := False;
      Kind : ABI.Kind := ABI.No_Capability;
      Object : Interfaces.Unsigned_64 := 0;
   end record;
   function Describe (Item : Process) return Description;
end CuBit.Handles.IPC;
