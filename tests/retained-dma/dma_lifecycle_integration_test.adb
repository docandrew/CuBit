with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with DMA_Record_Blocks;
with DMA_Retirement_Steps;
with Retained_DMA_Budget;
with Ada.Text_IO;
procedure DMA_Lifecycle_Integration_Test is
   Released, Freed, Calls : Natural := 0;
   Expected_Base : Unsigned_64 := 0;
   procedure Release_Owner (Physical_Page, Owner : Unsigned_64) is
   begin
      pragma Assert (Owner = 7 and Physical_Page = Expected_Base + Unsigned_64 (Released) * 4096);
      Released := Released + 1;
      Calls := Calls + 1;
   end Release_Owner;
   procedure Free_Block (Physical : Unsigned_64; Order : Natural) is
   begin
      pragma Assert (Physical = Expected_Base and Order = 9 and Released = 512);
      Freed := Freed + 1;
   end Free_Block;
   package Retirement is new DMA_Retirement_Steps (Release_Owner, Free_Block);
   type Memory is array (1 .. 65536) of aliased Unsigned_64;
   RAM : Memory with Alignment => 4096;
   Offset : Integer_Address := 0;
   function Allocate (Bytes, Alignment : Storage_Count) return Address is
      Result : Address;
   begin
      Offset := (Offset + Integer_Address (Alignment) - 1) /
        Integer_Address (Alignment) * Integer_Address (Alignment);
      pragma Assert (Offset + Integer_Address (Bytes) <= RAM'Size / 8);
      Result := To_Address (To_Integer (RAM'Address) + Offset);
      Offset := Offset + Integer_Address (Bytes);
      return Result;
   end Allocate;
   package Records is new DMA_Record_Blocks (Retirement.Allocation, 16, Allocate);
   use type Records.Result;
   use type Records.Reference;
   Store : Records.Pool;
   Owner, Orphans : Records.List;
   Old : array (1 .. 40) of Records.Reference;
   Ref : Records.Reference;
   Status : Records.Result;
   Charged : Unsigned_64 := 0;
   OK, Complete : Boolean;
   Limit : constant Unsigned_64 := Retained_DMA_Budget.Limit_Pages (8 * 1024 ** 3);
begin
   for I in Old'Range loop
      Retained_DMA_Budget.Reserve (Limit, Charged, 512, OK);
      pragma Assert (OK);
      Records.Reserve (Store,
        (Unsigned_64 (I) * 2 * 1024 ** 2, 7, 42, 9, True),
        65536, Ref, Status);
      pragma Assert (Status = Records.Ready);
      Old (I) := Ref;
      Records.Push (Owner, Ref);
   end loop;
   pragma Assert (Charged * 4096 = 80 * 1024 ** 2);
   -- Model worker calls only; this does not test actual kernel scheduling.
   for I in Old'Range loop
      Records.Pop (Owner, Ref);
      pragma Assert (Ref = Old (I));
      declare
         Cursor : Retirement.Cursor;
         Item : constant Retirement.Allocation := Records.Value (Ref);
         Before : Natural;
      begin
         Released := 0;
         Expected_Base := Item.Physical;
         Before := Calls;
         Retirement.Step (Item, Cursor, False, Complete);
         pragma Assert (not Complete and Calls = Before);
         for Work in 1 .. 8 loop
            Before := Calls;
            Retirement.Step (Item, Cursor, True, Complete);
            pragma Assert (Calls - Before = 64 and Complete = (Work = 8));
         end loop;
         Records.Push (Orphans, Ref);
      end;
   end loop;
   pragma Assert (Freed = 0 and Records.Empty (Owner));
   pragma Assert (Charged = 40 * 512);
   -- Same numerical PID, new incarnation; no orphan metadata is reused.
   Records.Reserve (Store, (16#10000000#, 7, 43, 9, False), 65536, Ref, Status);
   pragma Assert (Status = Records.Ready);
   for I in Old'Range loop
      pragma Assert (Old (I) /= Ref and Records.Value (Old (I)).Generation = 42);
   end loop;
   declare
      Cursor : Retirement.Cursor;
   begin
      Released := 0;
      Expected_Base := 16#10000000#;
      for Work in 1 .. 8 loop
         Retirement.Step (Records.Value (Ref), Cursor, True, Complete);
      end loop;
      pragma Assert (Complete and Freed = 1);
      Records.Release (Store, Ref);
   end;
   pragma Assert (Charged = 40 * 512 and not Records.Empty (Orphans));
   Ada.Text_IO.Put_Line ("PASS composed DMA lifecycle:40 allocations/80MiB,64-page steps, grant gate, retained orphan survives PID reuse, ordinary free does not refund retained charge");
end DMA_Lifecycle_Integration_Test;
