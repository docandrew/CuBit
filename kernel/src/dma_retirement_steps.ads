pragma Ada_2022;
with Interfaces; use Interfaces;
-- Caller establishes stopped execution, retired CPU translations and no
-- outstanding grant acquisitions before allowing cleanup. GPU safety is NOT
-- inferred: retained allocations are never freed by this operation.
generic
   with procedure Release_Owner (Physical_Page, Owner : Unsigned_64);
   with procedure Free_Block (Physical : Unsigned_64; Order : Natural);
package DMA_Retirement_Steps is
   subtype Allocation_Order is Natural range 0 .. 14;
   type Allocation is record
      Physical, Owner, Generation : Unsigned_64 := 0;
      Order : Allocation_Order := 0;
      Retained : Boolean := False;
   end record;
   type Cursor is private;
   function Rejected (Position : Cursor) return Boolean;
   Maximum_Pages_Per_Step : constant Positive := 64;
   procedure Step
     (Item : Allocation; Position : in out Cursor;
      CPU_And_Grants_Retired : Boolean; Complete : out Boolean);
private
   type Cursor is record
      Next_Page : Natural range 0 .. 2 ** Allocation_Order'Last := 0;
      Finished : Boolean := False;
      Started : Boolean := False;
      Poisoned : Boolean := False;
      Identity : Allocation;
   end record;
end DMA_Retirement_Steps;
