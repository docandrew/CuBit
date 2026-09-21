-- Single-owner page runs for the bounded runtime's large/over-aligned path.
-- Offsets and ownership only: mapping and payload accesses belong to adapters.
package Heap_Extents with SPARK_Mode, Pure is
   Page_Bytes : constant := 4_096;
   Page_Count : constant := 4_096;
   Arena_Bytes : constant := Page_Count * Page_Bytes;
   subtype Page_Id is Positive range 1 .. Page_Count;
   subtype Page_Reference is Natural range 0 .. Page_Count;
   subtype Run_Length is Positive range 1 .. Page_Count;
   No_Page : constant Page_Reference := 0;
   type State is private;
   function Valid (Heap : State) return Boolean with Ghost;
   function Owner (Heap : State; Page : Page_Id) return Page_Reference;
   function Length (Heap : State; Page : Page_Id) return Page_Reference;
   function Fits (First : Page_Id; Count : Run_Length) return Boolean is
     (Count <= Page_Count - First + 1);
   function Free_Run (Heap : State; First : Page_Id; Count : Run_Length) return Boolean is
     (Fits (First, Count) and then
       (for all P in First .. First + Count - 1 => Owner (Heap, P) = No_Page));
   function Can_Allocate (Heap : State; Count, Alignment : Run_Length) return Boolean is
     (for some P in Page_Id => (P - 1) mod Alignment = 0 and then Free_Run (Heap, P, Count))
     with Ghost;
   procedure Initialize (Heap : out State) with
     Post => Valid (Heap) and then (for all P in Page_Id => Owner (Heap, P) = No_Page);
   procedure Allocate (Heap : in out State; Count, Alignment : Run_Length;
                       First : out Page_Reference) with
     Pre => Valid (Heap),
     Post => Valid (Heap) and then
       (First /= No_Page) = Can_Allocate (Heap'Old, Count, Alignment) and then
       (if First /= No_Page then Fits (First, Count) and then
          (First - 1) mod Alignment = 0 and then Length (Heap, First) = Count and then
          Free_Run (Heap'Old, First, Count) and then
          (for all P in Page_Id =>
             (if P in First .. First + Count - 1 then Owner (Heap, P) = First
              else Owner (Heap, P) = Owner (Heap'Old, P))) and then
          (for all P in Page_Id => (if P /= First then Length (Heap, P) = Length (Heap'Old, P)))
        else Heap = Heap'Old);
   procedure Release (Heap : in out State; First : Page_Id; Success : out Boolean) with
     Pre => Valid (Heap),
     Post => Valid (Heap) and then Success = (Length (Heap'Old, First) > 0) and then
       (if Success then Length (Heap, First) = 0 and then
          (for all P in Page_Id =>
             (if Owner (Heap'Old, P) = First then Owner (Heap, P) = No_Page
              else Owner (Heap, P) = Owner (Heap'Old, P))) and then
          (for all P in Page_Id => (if P /= First then Length (Heap, P) = Length (Heap'Old, P)))
        else Heap = Heap'Old);
private
   type Page_Map is array (Page_Id) of Page_Reference;
   type State is record
      Owners, Lengths : Page_Map;
   end record;
   function Owner (Heap : State; Page : Page_Id) return Page_Reference is (Heap.Owners (Page));
   function Length (Heap : State; Page : Page_Id) return Page_Reference is (Heap.Lengths (Page));
   function Valid (Heap : State) return Boolean is
     ((for all P in Page_Id =>
        (if Heap.Owners (P) = No_Page then Heap.Lengths (P) = 0
         else Heap.Owners (P) <= P and then
           Heap.Lengths (Heap.Owners (P)) > P - Heap.Owners (P))) and then
      (for all P in Page_Id =>
         (if Heap.Lengths (P) > 0 then Fits (P, Heap.Lengths (P)) and then
            (for all Q in P .. P + Heap.Lengths (P) - 1 => Heap.Owners (Q) = P))));
end Heap_Extents;
