package body CuBit.Handles.IPC is
   function Describe (Item : Device_Memory) return Description is
   begin
      if Item /= 1 then raise Program_Error with "wrong authority inspected"; end if;
      Calls := Calls + 1;
      return Metadata;
   end Describe;
end CuBit.Handles.IPC;
