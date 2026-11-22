pragma Ada_2022;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Check_Results; use Check_Results;
with Events; use Events;
with Events.Library;
with Shapes;

package body Finalization_Checks is

   procedure Scope_Exit is
   begin
      Clear;
      declare
         A : Tracked;
         B : Tracked := (Ada.Finalization.Controlled with Id => 2);
         R : Resource;
         pragma Unreferenced (R);
         pragma Warnings (Off, B);  --  its assignment is the point (Finalize, Adjust)
      begin
         A.Id := 1;
         B := A;          --  F2 (old B), then A1 (the copy)
         B.Id := 3;
      end;
      --  A and R initialized (B is built from its aggregate); B := A
      --  finalizes the old B and adjusts the copy; the scope ends with R,
      --  B, A finalized in reverse order.
      Check (Trace = "I0I0F2A1F0F3F1",
             "Initialize/Adjust/Finalize on scope exit, reverse order (" & Trace & ")");
   end Scope_Exit;

   function Make (Id : Object_Id) return Tracked is
     (Ada.Finalization.Controlled with Id => Id);

   procedure Function_Results is
   begin
      Clear;
      declare
         X : constant Tracked := Make (4);
      begin
         Check (X.Id = 4 and then Count ('I') = 0, "controlled function result built in place");
      end;
      Check (Count ('F') = Count ('A') + 1, "every controlled copy finalized (" & Trace & ")");
   end Function_Results;

   procedure Composites is
      type Pair is record
         Left, Right : Tracked;
      end record;
      type Row is array (1 .. 3) of Tracked;
   begin
      Clear;
      declare
         P : Pair;
         Q : Row;
         pragma Unreferenced (P, Q);
      begin
         null;
      end;
      Check (Count ('I') = 5 and then Count ('F') = 5,
             "controlled record and array components (" & Trace & ")");
   end Composites;

   --  A tagged extension with a controlled component, through a class-wide
   --  object: finalized by dispatching on its tag.
   type Holder is new Shapes.Square with record
      Inner : Tracked;
   end record;

   procedure Class_Wide is
   begin
      Clear;
      declare
         S : constant Shapes.Shape'Class := Holder'(Id => 1, Side => 2, Inner => Make (5));
      begin
         Check (S.Area = 4, "class-wide object with a controlled component");
      end;
      Check (Count ('F') >= 1 and then Count ('F') = Count ('A') + 1,
             "class-wide object finalized through its tag (" & Trace & ")");
   end Class_Wide;

   procedure Heap is
      type Tracked_Access is access Tracked;
      type Shape_Access is access Shapes.Shape'Class;
      procedure Free is new Ada.Unchecked_Deallocation (Tracked, Tracked_Access);
      procedure Free is new Ada.Unchecked_Deallocation (Shapes.Shape'Class, Shape_Access);
      T : Tracked_Access;
      S : Shape_Access;
      Kept : Tracked_Access;
   begin
      Clear;
      T := new Tracked;
      T.Id := 6;
      S := new Holder'(Id => 1, Side => 2, Inner => Make (7));
      Free (T);
      Free (S);
      Check (T = null and then S = null and then Trace'Length >= 6
               and then Trace (Trace'First .. Trace'First + 1) = "I0"
               and then Trace (Trace'Last - 3 .. Trace'Last) = "F6F7",
             "Unchecked_Deallocation finalizes heap objects (" & Trace & ")");

      --  Not freed: finalized with its access type's collection when the
      --  collection's scope ends.
      Clear;
      declare
         type Local_Access is access Tracked;
         L : Local_Access;
      begin
         L := new Tracked;
         L.Id := 8;
         Kept := null;
      end;
      Check (Trace = "I0F8", "collection finalizes live objects when its access type ends (" & Trace & ")");
      pragma Unreferenced (Kept);
   end Heap;

   procedure Run is
   begin
      Check (Events.Library.Trace_At_Elaboration = "I0" and then Events.Library.Object.Id = 0,
             "library-level controlled object initialized at elaboration");
      Scope_Exit;
      Function_Results;
      Composites;
      Class_Wide;
      Heap;
   end Run;

end Finalization_Checks;
