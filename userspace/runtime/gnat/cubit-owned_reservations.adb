with CuBit.Kernel_Calls;
package body CuBit.Owned_Reservations is
   package ABI renames CuBit.Kernel_ABI;
   package KC renames CuBit.Kernel_Calls;

   function Reserve (Bytes : Unsigned_64; Alignment : Unsigned_64 := 0)
                     return Unsigned_64 is
     (KC.Call (ABI.Reserve_Owned_Region, Bytes, Alignment));

   function Change (Address, Bytes : Unsigned_64; Operation : Range_Operation)
                    return Boolean is
     (KC.Call (ABI.Change_Owned_Range, Address, Bytes,
               Range_Operation'Pos (Operation)) = 0);

   function Release (Base, Bytes : Unsigned_64) return Boolean is
     (KC.Call (ABI.Release_Owned_Region, Base, Bytes) = 0);

   function Commit_Steps (Address, Bytes : Unsigned_64) return Boolean is
      Done, Step : Unsigned_64 := 0;
   begin
      if Bytes = 0 then
         return False;
      end if;
      while Done < Bytes loop
         Step := Unsigned_64'Min (Bytes - Done, Maximum_Commit_Bytes);
         if not Change (Address + Done, Step, Commit) then
            return False;
         end if;
         Done := Done + Step;
      end loop;
      return True;
   end Commit_Steps;

   function Allocate (Bytes : Unsigned_64) return Unsigned_64 is
      Size : constant Unsigned_64 := Page_Round (Bytes);
      Base : Unsigned_64;
   begin
      if Size = 0 then
         return 0;
      end if;
      Base := Reserve (Size);
      if Base = 0 or else Commit_Steps (Base, Size) then
         return Base;
      end if;
      --  Release retires whatever was committed; a refused release keeps
      --  the region until the process exits.
      declare
         Ignore : constant Boolean := Release (Base, Size);
      begin
         return 0;
      end;
   end Allocate;

   function Reserve_From_C (Bytes : Unsigned_64) return Unsigned_64 is
     (Reserve (Bytes));
   --  All or nothing, as one kernel commit was before commits were bounded
   --  to Maximum_Commit_Bytes: steps already committed are dropped again.
   function Commit_From_C (Address, Bytes : Unsigned_64) return Unsigned_64 is
   begin
      if Commit_Steps (Address, Bytes) then
         return 0;
      end if;
      declare
         Ignore : constant Boolean := Change (Address, Bytes, Decommit);
      begin
         return 1;
      end;
   end Commit_From_C;
   function Release_From_C (Base, Bytes : Unsigned_64) return Unsigned_64 is
     (if Release (Base, Bytes) then 0 else 1);
end CuBit.Owned_Reservations;
