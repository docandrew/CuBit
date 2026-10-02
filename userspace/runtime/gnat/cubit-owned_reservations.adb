with CuBit.Messages;
package body CuBit.Owned_Reservations is
   function Reserve (Bytes : Interfaces.Unsigned_64)
                     return Interfaces.Unsigned_64 is
     (CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_RESERVE_OWNED_MEMORY, Bytes));

   function Commit_Prefix (Base, Offset, Bytes : Interfaces.Unsigned_64)
                           return Interfaces.Unsigned_64 is
     (CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_COMMIT_OWNED_MEMORY_PREFIX,
         Base, Offset, Bytes));

   function Release (Base, Bytes : Interfaces.Unsigned_64)
                     return Interfaces.Unsigned_64 is
     (CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_RELEASE_OWNED_RESERVATION, Base, Bytes));
end CuBit.Owned_Reservations;
