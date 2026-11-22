with Interfaces;
package CuBit.Handles.IPC is
   type Description is record
      Current : Boolean := False;
      Kind : ABI.Kind := ABI.No_Capability;
      Object : Interfaces.Unsigned_64 := 0;
      Parameter : Interfaces.Unsigned_64 := 0;
      Held : Rights;
   end record;
   function Describe (Item : Endpoint) return Description;
   function Describe (Item : Interrupt) return Description;
end CuBit.Handles.IPC;
