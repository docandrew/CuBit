with DMA_Record_Blocks;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO;
procedure DMA_Record_Blocks_Test is
   type Memory is array (1 .. 65536) of aliased Unsigned_64;
   RAM : Memory with Alignment => 4096;
   Offset : Integer_Address := 0;
   Fail : Boolean := False;
   Misalign : Boolean := False;
   function Allocate (Bytes, Alignment : Storage_Count) return Address is
      Result : Address;
   begin
      if Fail then return Null_Address; end if;
      Offset := (Offset + Integer_Address (Alignment) - 1) /
        Integer_Address (Alignment) * Integer_Address (Alignment);
      pragma Assert (Offset + Integer_Address (Bytes) <= RAM'Size / 8);
      Result := To_Address (To_Integer (RAM'Address) + Offset);
      Offset := Offset + Integer_Address (Bytes);
      if Misalign then return To_Address (To_Integer (Result) + 1); end if;
      return Result;
   end Allocate;
   package R is new DMA_Record_Blocks (Unsigned_64, 16, Allocate);
   use type R.Result;
   use type R.Reference;
   P : R.Pool;
   Live, Orphans : R.List;
   Item, First : R.Reference;
   Status : R.Result;
   Bytes : Unsigned_64;
begin
   R.Reserve (P, 0, R.Block_Bytes - 1, Item, Status);
   pragma Assert (Status = R.Metadata_Quota and Item = null and Offset = 0);
   Misalign := True;
   R.Reserve (P, 0, 65536, Item, Status);
   pragma Assert (Status = R.Invalid_Backing and Item = null);
   pragma Assert (R.Metadata_Bytes (P) = 0);
   Misalign := False;
   for I in 1 .. 128 loop
      R.Reserve (P, Unsigned_64 (I), 65536, Item, Status);
      pragma Assert (Status = R.Ready);
      if I = 1 then First := Item; end if;
      R.Push (Live, Item);
      pragma Assert (R.Value (First) = 1);
   end loop;
   Bytes := R.Metadata_Bytes (P);
   pragma Assert (Bytes = 8 * R.Block_Bytes);
   Fail := True;
   R.Reserve (P, 129, 65536, Item, Status);
   pragma Assert (Status = R.No_Memory and Item = null);
   pragma Assert (R.Metadata_Bytes (P) = Bytes and R.Value (First) = 1);
   R.Move (Live, Orphans);
   pragma Assert (R.Empty (Live) and not R.Empty (Orphans));
   for I in 1 .. 128 loop
      R.Pop (Orphans, Item);
      pragma Assert (R.Value (Item) = Unsigned_64 (I));
      R.Release (P, Item);
   end loop;
   pragma Assert (R.Empty (Orphans));
   R.Reserve (P, 200, 65536, Item, Status);
   pragma Assert (Status = R.Ready and R.Value (Item) = 200);
   R.Set_Value (Item, 201);
   pragma Assert (R.Value (Item) = 201);
   pragma Assert (R.Metadata_Bytes (P) = Bytes);
   Ada.Text_IO.Put_Line ("PASS:128 stable records, fallible growth, orphan list transfer, bounded pop and metadata reuse");
end DMA_Record_Blocks_Test;
