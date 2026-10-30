with Interfaces;
package CuBit.Messages is
   SYSCALL_GETTIME : constant := 27;
   Sample : Interfaces.Unsigned_64 := 0;
   Calls : Natural := 0;
   Change_After_First : Boolean := False;
   Following_Sample : Interfaces.Unsigned_64 := 0;
   function syscall (Number : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
end CuBit.Messages;
