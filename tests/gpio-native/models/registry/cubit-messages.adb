package body CuBit.Messages is
   use type Interfaces.Unsigned_64;
   function syscall (Number : Interfaces.Unsigned_64) return Interfaces.Unsigned_64 is
      Value : constant Interfaces.Unsigned_64 := Sample;
   begin
      if Number /= SYSCALL_GETTIME then raise Program_Error with "wrong clock syscall"; end if;
      Calls := Calls + 1;
      if Change_After_First and then Calls = 1 then Sample := Following_Sample; end if;
      if Change_After_Second and then Calls = 2 then Sample := Following_Sample; end if;
      return Value;
   end syscall;
end CuBit.Messages;
