pragma Ada_2022;
with Interfaces; use Interfaces;
package Retained_DMA_Budget with SPARK_Mode, Pure is
   Page_Bytes : constant Unsigned_64 := 4096;
   -- Global protection, independent of per-client quota and DMA aperture.
   -- Global ceiling is half of buddy-managed RAM; this is admission, not a
   -- reservation. Per-owner quota and device addressability remain separate.
   Managed_Share_Divisor : constant Unsigned_64 := 2;
   function Limit_Pages (Managed_Bytes : Unsigned_64) return Unsigned_64 is
     (Managed_Bytes / Managed_Share_Divisor / Page_Bytes);
   -- Serialize reservations and refunds under the same kernel lock.
   -- Counts include pending, live and orphaned retained physical backing.
   function Can_Reserve (Limit, Charged, Pages : Unsigned_64) return Boolean is
     (Pages /= 0 and then Charged <= Limit and then Pages <= Limit - Charged);
   procedure Reserve
     (Limit : Unsigned_64; Charged : in out Unsigned_64;
      Pages : Unsigned_64; Accepted : out Boolean)
     with Post =>
       Accepted = Can_Reserve (Limit, Charged'Old, Pages) and then
       Charged = (if Accepted then Charged'Old + Pages else Charged'Old);
   -- Only cancel a reservation after failed allocation has fully rolled back.
   -- Owner death, timeout, unmap alone and logical BO release are NOT refunds.
   procedure Cancel_Unpublished
     (Charged : in out Unsigned_64; Pages : Unsigned_64)
     with Pre => Pages <= Charged,
          Post => Charged = Charged'Old - Pages;
end Retained_DMA_Budget;
