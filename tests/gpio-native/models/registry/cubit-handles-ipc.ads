with Interfaces;
package CuBit.Handles.IPC is
   type Description (Current : Boolean := False) is record
      case Current is
         when True =>
            Kind : ABI.Kind := ABI.No_Capability;
            Held : Rights;
            Object, Parameter : Interfaces.Unsigned_64 := 0;
         when False => null;
      end case;
   end record;
   Metadata : Description;
   Calls : Natural := 0;
   Expected_Authority : Device_Memory := 1;
   function Describe (Item : Device_Memory) return Description;
end CuBit.Handles.IPC;
