generic
   Capacity : Positive;
package Compositor_Focus with SPARK_Mode, Pure is
   subtype Index is Natural range 0 .. Capacity - 1;
   type Candidates is array (Index) of Boolean;
   type Selection is record
      Found : Boolean := False;
      Slot : Index := Index'First;
   end record;
   -- Entries are ordered back to front. Eligibility is supplied from the
   -- surface table: used, visible and window, after removal has completed.
   function Topmost (Items : Candidates) return Selection
     with Post =>
       Topmost'Result.Found = (for some I in Index => Items (I)) and then
       (if Topmost'Result.Found then Items (Topmost'Result.Slot) and
         (for all I in Index => (if I > Topmost'Result.Slot then not Items (I))));
end Compositor_Focus;
