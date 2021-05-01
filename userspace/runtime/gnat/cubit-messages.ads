 ------------------------------------------------------------------------------
 -- CuBit
 -- Copyright (C) 2021 Jon Andrew
 --
 -- @summary
 -- IPC Messages / Syscalls
 ------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.Messages is

   procedure syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0);

   procedure sendMsg (to : Unsigned_64; msg : Unsigned_64);

end CuBit.Messages;
