with Interfaces.C; use Interfaces.C;
with System;
with System.Storage_Elements; use System.Storage_Elements;
package body Linux_Provider is
   PROT_NONE : constant := 0;
   PROT_RW : constant := 3;
   MAP_PRIVATE_ANONYMOUS_NORESERVE : constant := 16#02# + 16#20# + 16#4000#;
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

   function Find (Base : Unsigned_64) return Natural is
   begin
      for I in 1 .. Count loop
         if Table (I).Base = Base then return I; end if;
      end loop;
      return 0;
   end Find;

   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
      Address : System.Address;
   begin
      if Bytes = 0 or else Bytes mod 4096 /= 0 or else Count = MAXIMUM then
         Contract_Violations := Contract_Violations + 1;
         return 0;
      end if;
      Address := mmap (System.Null_Address, size_t (Bytes), PROT_NONE, MAP_PRIVATE_ANONYMOUS_NORESERVE, -1, 0);
      if To_Integer (Address) = MAP_FAILED then return 0; end if;
      Count := Count + 1;
      Table (Count) := (Base => Unsigned_64 (To_Integer (Address)), Bytes => Bytes, Committed => 0);
      return Table (Count).Base;
   end Reserve;

   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
      I : constant Natural := Find (Base);
   begin
      if I = 0 or else Offset /= Table (I).Committed or else Bytes = 0 or else Bytes mod 4096 /= 0
        or else Bytes > Maximum_Commit or else Bytes > Table (I).Bytes - Offset
      then
         Contract_Violations := Contract_Violations + 1;
         return False;
      end if;
      if Bytes > Quota - Committed then
         return False;
      end if;
      if mprotect (To_Address (Integer_Address (Base + Offset)), size_t (Bytes), PROT_RW) /= 0 then
         return False;
      end if;
      Table (I).Committed := Offset + Bytes;
      Committed := Committed + Bytes;
      return True;
   end Commit;

   function Release (Base, Bytes : Unsigned_64) return Boolean is
      I : constant Natural := Find (Base);
   begin
      if I = 0 or else Table (I).Bytes /= Bytes then
         Contract_Violations := Contract_Violations + 1;
         return False;
      end if;
      if munmap (To_Address (Integer_Address (Base)), size_t (Bytes)) /= 0 then return False; end if;
      Committed := Committed - Table (I).Committed;
      Table (I) := Table (Count);
      Count := Count - 1;
      return True;
   end Release;

   function Live_Reservations return Natural is (Count);
end Linux_Provider;
