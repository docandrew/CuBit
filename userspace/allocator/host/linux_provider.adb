with Interfaces.C; use Interfaces.C;
with System;
with System.Storage_Elements; use System.Storage_Elements;
package body Linux_Provider is
   PROT_NONE : constant := 0;
   PROT_RW : constant := 3;
   MAP_PRIVATE_ANONYMOUS_NORESERVE : constant := 16#02# + 16#20# + 16#4000#;
   PAGE : constant := 4096;
   function mmap (Address : System.Address; Length : size_t; Prot, Flags, Fd : int; Offset : long)
     return System.Address with Import, Convention => C, External_Name => "mmap";
   function mprotect (Address : System.Address; Length : size_t; Prot : int) return int
     with Import, Convention => C, External_Name => "mprotect";
   function munmap (Address : System.Address; Length : size_t) return int
     with Import, Convention => C, External_Name => "munmap";
   MAP_FAILED : constant Integer_Address := Integer_Address'Last;

   --  What each reservation holds, to check the exact-prefix contract.
   MAXIMUM : constant := 70_000;
   type Entry_Record is record
      Base, Bytes, Committed : Unsigned_64 := 0;
   end record;
   Table : array (1 .. MAXIMUM) of Entry_Record;
   Count : Natural := 0;

   Lock_Word : aliased Unsigned_8 := 0;
   function Exchange (Ptr : System.Address; Value : Unsigned_8; Model : Integer) return Unsigned_8
     with Import, Convention => Intrinsic, External_Name => "__atomic_exchange_1";
   procedure Store (Ptr : System.Address; Value : Unsigned_8; Model : Integer)
     with Import, Convention => Intrinsic, External_Name => "__atomic_store_1";
   procedure Pause with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pause";
   procedure Lock is
   begin
      while Exchange (Lock_Word'Address, 1, 2) /= 0 loop
         Pause;
      end loop;
   end Lock;
   procedure Unlock is
   begin
      Store (Lock_Word'Address, 0, 3);
   end Unlock;

   function Find (Base : Unsigned_64) return Natural is
   begin
      for I in 1 .. Count loop
         if Table (I).Base = Base then return I; end if;
      end loop;
      return 0;
   end Find;

   function To_Address (Value : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (Integer_Address (Value)));

   function Reserve (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Align : constant Unsigned_64 := Unsigned_64'Max (Alignment, PAGE);
      Span : Unsigned_64;
      Address : System.Address;
      Start, Aligned : Unsigned_64;
      Ignore : int;
   begin
      Lock;
      if Bytes = 0 or else Bytes mod PAGE /= 0 or else Count = MAXIMUM
        or else Align > Maximum_Alignment or else (Align and (Align - 1)) /= 0
        or else Bytes > Unsigned_64'Last - Align
      then
         Contract_Violations := Contract_Violations + 1;
         Unlock;
         return 0;
      end if;
      Span := Bytes + (Align - PAGE);
      Address := mmap (System.Null_Address, size_t (Span), PROT_NONE, MAP_PRIVATE_ANONYMOUS_NORESERVE, -1, 0);
      if To_Integer (Address) = MAP_FAILED then
         Unlock;
         return 0;
      end if;
      Start := Unsigned_64 (To_Integer (Address));
      Aligned := (Start + (Align - 1)) / Align * Align;
      --  Trim the slack on both sides: the reservation is exactly Bytes.
      if Aligned > Start then
         Ignore := munmap (Address, size_t (Aligned - Start));
      end if;
      if Start + Span > Aligned + Bytes then
         Ignore := munmap (To_Address (Aligned + Bytes), size_t (Start + Span - (Aligned + Bytes)));
      end if;
      Count := Count + 1;
      Table (Count) := (Base => Aligned, Bytes => Bytes, Committed => 0);
      Unlock;
      return Aligned;
   end Reserve;

   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
      I : Natural;
   begin
      Lock;
      I := Find (Base);
      if I = 0 or else Offset /= Table (I).Committed or else Bytes = 0 or else Bytes mod PAGE /= 0
        or else Bytes > Maximum_Commit or else Bytes > Table (I).Bytes - Offset
      then
         Contract_Violations := Contract_Violations + 1;
         Unlock;
         return False;
      end if;
      if Committed > Quota or else Bytes > Quota - Committed
        or else mprotect (To_Address (Base + Offset), size_t (Bytes), PROT_RW) /= 0
      then
         Unlock;
         return False;
      end if;
      Table (I).Committed := Offset + Bytes;
      Committed := Committed + Bytes;
      Unlock;
      return True;
   end Commit;

   function Release (Base, Bytes : Unsigned_64) return Boolean is
      I : Natural;
   begin
      Lock;
      I := Find (Base);
      if I = 0 or else Table (I).Bytes /= Bytes then
         Contract_Violations := Contract_Violations + 1;
         Unlock;
         return False;
      end if;
      if Refuse_Release or else munmap (To_Address (Base), size_t (Bytes)) /= 0 then
         Unlock;
         return False;
      end if;
      Committed := Committed - Table (I).Committed;
      Table (I) := Table (Count);
      Count := Count - 1;
      Unlock;
      return True;
   end Release;

   function Live_Reservations return Natural is
      N : Natural;
   begin
      Lock;
      N := Count;
      Unlock;
      return N;
   end Live_Reservations;

   procedure Set_Quota (Bytes : Unsigned_64) is
   begin
      Lock;
      Quota := Bytes;
      Unlock;
   end Set_Quota;
end Linux_Provider;
