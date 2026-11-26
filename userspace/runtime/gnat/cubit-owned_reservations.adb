with CuBit.Kernel_Calls;
package body CuBit.Owned_Reservations is
   package ABI renames CuBit.Kernel_ABI;
   package KC renames CuBit.Kernel_Calls;
   package OM renames Owned_Memory_ABI;

   function Reserve (Bytes, Alignment : Unsigned_64; Why : out Refusal)
                     return Unsigned_64 is
      Answer : constant Unsigned_64 := KC.Call (ABI.Reserve_Owned_Region, Bytes, Alignment);
   begin
      if OM.Is_Refusal_Word (Answer) then
         Why := OM.To_Refusal (Answer);
         return 0;
      end if;
      Why := OM.Invalid_Range;
      return Answer;
   end Reserve;

   function Reserve (Bytes : Unsigned_64; Alignment : Unsigned_64 := 0)
                     return Unsigned_64 is
      Ignore : Refusal;
   begin
      return Reserve (Bytes, Alignment, Ignore);
   end Reserve;

   function Change (Address, Bytes : Unsigned_64; Operation : Range_Operation)
                    return Boolean is
     (KC.Call (ABI.Change_Owned_Range, Address, Bytes,
               Range_Operation'Pos (Operation)) = OM.Done);

   function Release (Base, Bytes : Unsigned_64) return Boolean is
     (KC.Call (ABI.Release_Owned_Region, Base, Bytes) = OM.Done);

   function Allocate (Bytes : Unsigned_64) return Unsigned_64 is
      Size : constant Unsigned_64 := Page_Round (Bytes);
      Base : Unsigned_64;
   begin
      if Size = 0 then
         return 0;
      end if;
      Base := Reserve (Size);
      --  One commit, any size (MEM-007); all or nothing.
      if Base = 0 or else Change (Base, Size, Commit) then
         return Base;
      end if;
      declare
         Ignore : constant Boolean := Release (Base, Size);
      begin
         return 0;
      end;
   end Allocate;

   function Reserve_From_C (Bytes : Unsigned_64) return Unsigned_64 is
     (Reserve (Bytes));
   function Commit_From_C (Address, Bytes : Unsigned_64) return Unsigned_64 is
     (KC.Call (ABI.Change_Owned_Range, Address, Bytes, Range_Operation'Pos (Commit)));
   function Release_From_C (Base, Bytes : Unsigned_64) return Unsigned_64 is
     (KC.Call (ABI.Release_Owned_Region, Base, Bytes));
end CuBit.Owned_Reservations;
