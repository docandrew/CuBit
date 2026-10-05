------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Libc_ABI;
with CuBit.Libc_Start;

package body CuBit.Libc_Threads is

   use type Interfaces.C.int;
   use type System.Address;

   --  struct pthread (x86-64): after self, dtv, prev, next, sysinfo,
   --  canary, tid, errno_val; then cancel flags and the map.
   Detach_State_Offset : constant := 56;
   Stack_Offset        : constant := 88;
   Stack_Size_Offset   : constant := 96;
   Guard_Size_Offset   : constant := 104;
   DT_DETACHED : constant := 3;
   --  pthread_attr_t: _a_stacksize, _a_guardsize, _a_stackaddr (__s[0..2]),
   --  _a_detach (__i[6]).
   Attribute_Stack_Size_Offset : constant := 0;
   Attribute_Guard_Size_Offset : constant := 8;
   Attribute_Stack_Offset      : constant := 16;
   Attribute_Detach_Offset     : constant := 24;

   function Get_Attributes (Thread, Attributes : System.Address) return Interfaces.C.int is
      Detach_State : constant Interfaces.C.int with Import, Volatile,
        Address => Thread + Detach_State_Offset;
      Stack : constant Unsigned_64 with Import, Address => Thread + Stack_Offset;
      Stack_Size : constant Unsigned_64 with Import, Address => Thread + Stack_Size_Offset;
      Guard_Size : constant Unsigned_64 with Import, Address => Thread + Guard_Size_Offset;
      Attribute_Bytes : Storage_Array (1 .. CuBit.Libc_ABI.Pthread_Attribute_Bytes)
      with Import, Address => Attributes;
      A_Stack_Size : Unsigned_64 with Import, Address => Attributes + Attribute_Stack_Size_Offset;
      A_Guard_Size : Unsigned_64 with Import, Address => Attributes + Attribute_Guard_Size_Offset;
      A_Stack : Unsigned_64 with Import, Address => Attributes + Attribute_Stack_Offset;
      A_Detach : Interfaces.C.int with Import, Address => Attributes + Attribute_Detach_Offset;
   begin
      Attribute_Bytes := [others => 0];
      A_Detach := (if Detach_State >= DT_DETACHED then 1 else 0);
      A_Guard_Size := Guard_Size;
      if Stack /= 0 then
         A_Stack := Stack;
         A_Stack_Size := Stack_Size;
      else
         A_Stack := CuBit.Libc_Start.Stack_Top;
         A_Stack_Size := CuBit.Libc_Start.Stack_Size;
      end if;
      return 0;
   end Get_Attributes;

end CuBit.Libc_Threads;
