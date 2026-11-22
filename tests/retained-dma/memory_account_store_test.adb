with Memory_Account_Store;
with Process_Memory_Budget; use Process_Memory_Budget;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO;
procedure Memory_Account_Store_Test is
   RAM : array (1 .. 65536) of aliased Unsigned_64 with Alignment => 4096;
   Offset : Storage_Count := 0;
   Fail : Boolean := False;
   function Allocate (Bytes, Alignment : Storage_Count) return Address is
      Result : Address;
   begin
      if Fail then return Null_Address; end if;
      Offset := (Offset + Alignment - 1) / Alignment * Alignment;
      if Bytes > RAM'Size / 8 - Offset then return Null_Address; end if;
      Result := RAM'Address + Storage_Offset (Offset);
      Offset := Offset + Bytes;
      return Result;
   end Allocate;
   procedure Release_Index_Page (Page : Address) is
   begin
      null; -- Hosted bump arena; index reclamation is checked via byte counts.
   end Release_Index_Page;
   package S is new Memory_Account_Store (Allocate, Release_Index_Page);
   use type S.Open_Result;
   use type S.Handle;
   Owner, Other, Admission : S.Store;
   Batch : array (1 .. 600) of S.Handle;
   Saved, Replacement, Foreign, Failed : S.Handle;
   Status : S.Open_Result;
   OK, Live : Boolean;
   Pages, Limit, Bytes : Unsigned_64;
   Routed : S.Handle;
   Tags : array (Charge_Kind) of Unsigned_64;
begin
   S.Open (Owner, S.Block_Bytes - 1, Failed, Status);
   pragma Assert (Status = S.Metadata_Limit and Failed = S.No_Account);
   pragma Assert (Offset = 0);
   Fail := True;
   S.Open (Owner, 524288, Failed, Status);
   pragma Assert (Status = S.No_Memory and not S.Valid (Owner, Failed));
   Fail := False;
   -- Record creation succeeds, then index creation fails partway. Its branch
   -- pages roll back, and the unused record is reusable but never resolvable.
   S.Open (Admission, S.Block_Bytes + 6 * 4096, Failed, Status);
   pragma Assert (Status = S.Metadata_Limit and Failed = S.No_Account);
   pragma Assert (S.Metadata_Bytes (Admission) = S.Block_Bytes);
   pragma Assert (S.Resolve (Admission, 1) = S.No_Account);
   Fail := True;
   S.Open (Admission, 65536, Failed, Status);
   pragma Assert (Status = S.No_Memory and Failed = S.No_Account);
   pragma Assert (S.Metadata_Bytes (Admission) = S.Block_Bytes);
   Fail := False;
   S.Open (Admission, 65536, Failed, Status);
   pragma Assert (Status = S.Opened and S.Identity (Admission, Failed) = 1);
   S.Close (Admission, Failed, OK);
   pragma Assert (OK and S.Metadata_Bytes (Admission) = S.Block_Bytes);
   for I in Batch'Range loop
      S.Open (Owner, 524288, Batch (I), Status);
      pragma Assert (Status = S.Opened);
      S.Reserve (Owner, Batch (I), DMA_Backing, Unsigned_64 (I), OK);
      pragma Assert (OK);
      S.Adopt (Owner, Batch (I), Unsigned_64 (I), OK);
      pragma Assert (OK);
      S.Reserve (Owner, Batch (I), Ordinary, 1, OK);
      pragma Assert (not OK);
      S.Close (Owner, Batch (I), OK);
      pragma Assert (OK and S.Valid (Owner, Batch (I)));
      pragma Assert (S.Resolve (Owner, S.Identity (Owner, Batch (I))) = Batch (I));
      S.Inspect (Owner, Batch (1), Live, Pages, Limit, OK);
      pragma Assert (OK and not Live and Pages = 1 and Limit = 1);
   end loop;
   Bytes := S.Metadata_Bytes (Owner);
   pragma Assert (Bytes > 38 * S.Block_Bytes);
   S.Open (Other, 524288, Foreign, Status);
   pragma Assert (Status = S.Opened);
   S.Reserve (Owner, Foreign, Metadata, 1, OK);
   pragma Assert (not OK and not S.Valid (Other, Batch (1)));
   Saved := Batch (1);
   pragma Assert (S.Resolve (Owner, 0) = S.No_Account);
   pragma Assert (S.Resolve (Owner, Unsigned_64'Last) = S.No_Account);
   S.Reserve (Owner, Saved, Ordinary, 1, OK);
   pragma Assert (not OK); -- Closed incarnation cannot acquire new charges.
   S.Refund (Owner, Batch (1), DMA_Backing, 2, OK);
   pragma Assert (not OK and S.Valid (Owner, Batch (1)));
   S.Refund (Owner, Batch (1), DMA_Backing, 1, OK);
   pragma Assert (OK and Batch (1) = S.No_Account);
   pragma Assert (not S.Valid (Owner, Saved));
   Fail := True; -- Reuse requires no backing allocation.
   S.Open (Owner, Bytes, Replacement, Status);
   pragma Assert (Status = S.Opened and S.Metadata_Bytes (Owner) = Bytes);
   S.Reserve (Owner, Replacement, Ordinary, 3, OK);
   pragma Assert (OK);
   S.Refund (Owner, Saved, Ordinary, 1, OK);
   pragma Assert (not OK); -- Stale token cannot refund a reused slot.
   S.Inspect (Owner, Replacement, Live, Pages, Limit, OK);
   pragma Assert (OK and Live and Pages = 3);
   for I in 2 .. Batch'Last loop
      S.Refund (Owner, Batch (I), DMA_Backing, Unsigned_64 (I), OK);
      pragma Assert (OK and Batch (I) = S.No_Account);
   end loop;
   S.Close (Owner, Replacement, OK);
   pragma Assert (OK);
   S.Refund (Owner, Replacement, Ordinary, 3, OK);
   pragma Assert (OK and Replacement = S.No_Account);
   S.Refund (Owner, Replacement, Ordinary, 3, OK);
   pragma Assert (not OK);
   S.Close (Other, Foreign, OK);
   pragma Assert (OK and Foreign = S.No_Account);
   pragma Assert (S.Metadata_Bytes (Owner) = 38 * S.Block_Bytes);
   Fail := False;
   S.Open (Owner, 524288, Routed, Status);
   pragma Assert (Status = S.Opened);
   for Kind in Charge_Kind loop
      S.Reserve (Owner, Routed, Kind, Unsigned_64 (Charge_Kind'Pos (Kind) + 1), OK);
      pragma Assert (OK);
      Tags (Kind) := S.Charge_Identity (Owner, Routed, Kind);
      pragma Assert (Tags (Kind) / 4 = S.Identity (Owner, Routed));
      pragma Assert (Tags (Kind) mod 4 = Unsigned_64 (Charge_Kind'Pos (Kind)));
   end loop;
   S.Close (Owner, Routed, OK);
   pragma Assert (OK);
   for Bad in Unsigned_64 range 0 .. 3 loop
      S.Refund_Physical (Owner, Bad, 1, OK);
      pragma Assert (not OK);
   end loop;
   -- All four tags now denote buckets. Retained DMA cannot be refunded
   -- through the ordinary DMA tag, even for the same owner incarnation.
   S.Refund_Physical (Owner, Tags (DMA_Backing), 4, OK);
   pragma Assert (not OK);
   S.Refund_Physical (Owner, Unsigned_64'Last, 1, OK);
   pragma Assert (not OK);
   S.Refund_Physical (Owner, Tags (Ordinary), 0, OK);
   pragma Assert (not OK);
   for Kind in Charge_Kind loop
      S.Refund_Physical (Owner, Tags (Kind), Unsigned_64 (Charge_Kind'Pos (Kind) + 2), OK);
      pragma Assert (not OK);
      S.Refund_Physical (Owner, Tags (Kind), Unsigned_64 (Charge_Kind'Pos (Kind) + 1), OK);
      pragma Assert (OK);
      S.Refund_Physical (Owner, Tags (Kind), 1, OK);
      pragma Assert (not OK); -- Must not steal charges from another kind.
   end loop;
   pragma Assert (not S.Valid (Owner, Routed));
   pragma Assert (S.Charge_Identity (Owner, Routed, Ordinary) = 0);
   Ada.Text_IO.Put_Line
     ("PASS dynamic account store: 600 retired owners, stable growth, failed allocation, quota, cross-store and stale-token rejection, slot reuse");
end Memory_Account_Store_Test;
