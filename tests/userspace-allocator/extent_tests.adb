with Ada.Text_IO;
with Heap_Extents;
procedure Extent_Tests is
   use Heap_Extents;
   Heap : State;
   First, Other : Page_Reference;
   OK : Boolean;
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "extent test failed"; end if;
   end Check;
begin
   Initialize (Heap);
   Allocate (Heap, Page_Count, 1, First);
   Check (First = 1);
   Allocate (Heap, 1, 1, Other);
   Check (Other = No_Page);
   Release (Heap, 2, OK);
   Check (not OK and then Length (Heap, 1) = Page_Count);
   Release (Heap, 1, OK);
   Check (OK);
   Release (Heap, 1, OK);
   Check (not OK);
   -- Every alignment supported by the core, not just powers of two. With
   -- page 1 occupied the first aligned candidate is alignment + 1.
   Allocate (Heap, 1, 1, First);
   Check (First = 1);
   for Alignment in 1 .. Page_Count - 1 loop
      Allocate (Heap, 1, Alignment, Other);
      Check (Other = Alignment + 1);
      Check (Owner (Heap, Other) = Other and then Length (Heap, Other) = 1);
      Release (Heap, Other, OK);
      Check (OK);
   end loop;
   Allocate (Heap, 1, Page_Count, Other);
   Check (Other = No_Page);
   Release (Heap, 1, OK);
   Check (OK);
   Allocate (Heap, Page_Count, Page_Count, First);
   Check (First = 1);
   for P in Page_Id loop Check (Owner (Heap, P) = 1); end loop;
   Release (Heap, 1, OK);
   Check (OK);
   for P in Page_Id loop Check (Owner (Heap, P) = 0 and then Length (Heap, P) = 0); end loop;
   Ada.Text_IO.Put_Line ("PASS extent alignment, exhaustion, interior/double release and reuse");
end Extent_Tests;
