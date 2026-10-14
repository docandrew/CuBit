-- The kernel's geometry and element (Test_Tables.Kernel: Capability,
-- 32768 slots, 32-bit generations), built without run-time assertions
-- because the ghost invariant is quadratic in the capacity. Checks the
-- default limit, full capacity, page counts, the element fitting a page,
-- and a differential run against the reference model.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities;
with Checks; use Checks;
with Fake_Pages;
with Test_Tables; use Test_Tables;
with Differential;

procedure Geometry is
   use Kernel;
   use type Fake_Pages.Page_Total;

   function Make (Id : Unsigned_64) return Capabilities.Capability is
     ((capType => Capabilities.CAP_ENDPOINT, rights => Capabilities.READ_WRITE,
       authorityTag => Id, object => (ref => Id, param => 0), gen => 1));
   function Id_Of (C : Capabilities.Capability) return Unsigned_64 is
     (C.authorityTag);

   procedure Kernel_Differential is new Differential (Kernel, Make, Id_Of);

   Owner : constant Unsigned_64 := 7;
   T : Table;
   R : Insert_Result;
   Leaves_For_Default : constant Natural :=
     Natural (Default_Handle_Count) / Natural (Entries_Per_Leaf);
begin
   Section ("kernel geometry");
   Check (Max_Slots = 32_768, "capacity 512 leaves x 64");
   Configure (T, Owner, Default_Limit);
   for I in 1 .. Natural (Default_Handle_Count) loop
      Insert (T, Make (Unsigned_64 (I)), R);
      if R.Status /= Opened then
         Check (False, "default-limit fill refused at" & I'Image & ": " & R.Status'Image);
         exit;
      end if;
   end loop;
   Insert (T, Make (0), R);
   Check (R.Status = Limit_Reached, "4097th handle: Limit_Reached");
   Check (Natural (Pages_Held (T)) = 1 + Leaves_For_Default
          and then Natural (Fake_Pages.Held (Owner)) = 1 + Leaves_For_Default,
          "4096 handles = directory + 64 leaves");
   Release_All (T);
   Check (Fake_Pages.Held (Owner) = 0, "released");

   Configure (T, Owner, Kernel_Limit_Handles);
   for I in 1 .. Natural (Max_Slots) loop
      Insert (T, Make (Unsigned_64 (I)), R);
      if R.Status /= Opened then
         Check (False, "unlimited fill refused at" & I'Image);
         exit;
      end if;
   end loop;
   Insert (T, Make (0), R);
   Check (R.Status = Table_Full, "past 32768: Table_Full");
   Check (Natural (Pages_Held (T)) = 513, "513 pages at capacity");
   Release_All (T);
   Check (Is_Empty (T) and then Fake_Pages.Held (Owner) = 0, "every page returned");

   Kernel_Differential (16#5EED_0000_0000_0001#, 300_000, Owner + 1, "kernel geometry");
   Check (Fake_Pages.Bad_Releases = 0, "no bad release");
   Finish;
end Geometry;
