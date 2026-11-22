------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Futex_Locks;
with CuBit.Deadlines;
with CuBit.Program_Endings.Last_Chance;

package body CuBit.Unwinder_Support is

   use type Interfaces.C.size_t;
   use type System.Address;

   Success : constant int := 0;

   ------------------------------
   -- dl_iterate_phdr (static) --
   ------------------------------

   --  The program's ELF header, where the linker scripts load it
   --  (userspace/c/link.ld, userspace/libc/link.ld): GNU ld defines
   --  __ehdr_start when the header is in a loaded segment.
   ELF_Header : constant Storage_Element
   with Import, Convention => C, External_Name => "__ehdr_start";

   --  Elf64_Ehdr: e_phoff (8 bytes) and e_phnum (2 bytes).
   Program_Header_Offset_Field : constant := 32;
   Program_Header_Count_Field  : constant := 56;

   --  struct dl_phdr_info up to dlpi_phnum (and its padding): what the
   --  unwinder reads. dlpi_adds/dlpi_subs are left out on purpose (see the
   --  spec): libgcc caches only when the size includes them.
   type Header_Info is record
      Load_Bias : Unsigned_64;       --  dlpi_addr: 0, the program is not PIE
      Name      : System.Address;    --  dlpi_name
      Headers   : System.Address;    --  dlpi_phdr
      Count     : Unsigned_16;       --  dlpi_phnum
   end record with Convention => C;

   Info_Bytes : constant := 32;
   for Header_Info'Size use Info_Bytes * System.Storage_Unit;

   Program_Name : aliased constant String := "" & ASCII.NUL;

   function Iterate_Program_Headers
     (Callback : Header_Callback; Data : System.Address) return int
   is
      Base : constant System.Address := ELF_Header'Address;
      Offset : constant Unsigned_64
      with Import, Address => Base + Program_Header_Offset_Field;
      Count : constant Unsigned_16
      with Import, Address => Base + Program_Header_Count_Field;
      Info : aliased constant Header_Info :=
        (Load_Bias => 0,
         Name      => Program_Name'Address,
         Headers   => Base + Storage_Offset (Offset),
         Count     => Count);
   begin
      return Callback (Info'Address, Info_Bytes, Data);
   end Iterate_Program_Headers;

   ----------
   -- Once --
   ----------

   --  pthread_once_t: an int, 0 before the first call. Done marks it run;
   --  every first call goes through one lock, which the unwinder's single
   --  initializer (its register-size table) never re-enters.
   type Once_Word is mod 2 ** 32 with Atomic, Size => 32;
   Done : constant Once_Word := 2;
   Once_Lock : CuBit.Futex_Locks.Mutex;

   function Once (Control : System.Address; Init : Init_Routine) return int is
      State : Once_Word with Import, Address => Control;
   begin
      if State /= Done then
         CuBit.Futex_Locks.Lock (Once_Lock);
         if State /= Done then
            Init.all;
            State := Done;
         end if;
         CuBit.Futex_Locks.Unlock (Once_Lock);
      end if;
      return Success;
   end Once;

   -----------------------------
   -- Mutexes and conditions --
   -----------------------------

   --  The unwinder's pthread_mutex_t and pthread_cond_t objects are static
   --  and zeroed (PTHREAD_*_INITIALIZER); their first word is the futex
   --  word of a CuBit.Futex_Locks.Mutex or Event, whose zero is the
   --  initial state too.

   function Lock (Mutex : System.Address) return int is
      M : CuBit.Futex_Locks.Mutex with Import, Address => Mutex;
   begin
      CuBit.Futex_Locks.Lock (M);
      return Success;
   end Lock;

   function Unlock (Mutex : System.Address) return int is
      M : CuBit.Futex_Locks.Mutex with Import, Address => Mutex;
   begin
      CuBit.Futex_Locks.Unlock (M);
      return Success;
   end Unlock;

   function Wait (Condition, Mutex : System.Address) return int is
      E : CuBit.Futex_Locks.Event with Import, Address => Condition;
      M : CuBit.Futex_Locks.Mutex with Import, Address => Mutex;
      Seen : constant Unsigned_32 := CuBit.Futex_Locks.Current (E);
      Ignore : Boolean;
   begin
      CuBit.Futex_Locks.Unlock (M);
      Ignore := CuBit.Futex_Locks.Wait (E, Seen, CuBit.Deadlines.Forever);
      CuBit.Futex_Locks.Lock (M);
      return Success;
   end Wait;

   function Broadcast (Condition : System.Address) return int is
      E : CuBit.Futex_Locks.Event with Import, Address => Condition;
   begin
      CuBit.Futex_Locks.Signal (E);
      return Success;
   end Broadcast;

   ------------
   -- Memory --
   ------------

   Allocation_Alignment : constant := 16;
   --  malloc's: the largest fundamental alignment on x86-64

   function CuAlloc_Allocate (Bytes, Alignment : Unsigned_64) return System.Address
   with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free (Item : System.Address)
   with Import, Convention => C, External_Name => "cualloc_free";

   function Allocate (Bytes : size_t) return System.Address is
     (CuAlloc_Allocate (Unsigned_64 (Bytes), Allocation_Alignment));

   procedure Free (Item : System.Address) is
   begin
      if Item /= System.Null_Address then
         CuAlloc_Free (Item);
      end if;
   end Free;

   -------------------
   -- String_Length --
   -------------------

   function String_Length (Item : System.Address) return size_t is
      Length : size_t := 0;
   begin
      loop
         declare
            C : constant Character with Import, Address => Item + Storage_Offset (Length);
         begin
            exit when C = ASCII.NUL;
         end;
         Length := Length + 1;
      end loop;
      return Length;
   end String_Length;

   ---------------------
   -- Abort_Unwinding --
   ---------------------

   procedure Abort_Unwinding is
   begin
      CuBit.Program_Endings.Last_Chance.Write_Console
        ("Exception unwinding failed (corrupt or missing unwind tables); stopping" & ASCII.LF);
      CuBit.Program_Endings.Last_Chance.Stop;
   end Abort_Unwinding;

end CuBit.Unwinder_Support;
