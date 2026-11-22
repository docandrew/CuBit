package Compositor_Focus with SPARK_Mode, Pure is
   -- Entries are ordered back to front: Desktop's surface table, whose size
   -- grows with use. Eligibility is supplied from the surface table: used,
   -- visible and window, after removal has completed.
   type Candidates is array (Natural range <>) of Boolean;
   type Selection is record
      Found : Boolean := False;
      Slot : Natural := 0;
   end record;
   function Topmost (Items : Candidates) return Selection
     with Post =>
       Topmost'Result.Found = (for some I in Items'Range => Items (I)) and then
       (if Topmost'Result.Found then Topmost'Result.Slot in Items'Range and then
         Items (Topmost'Result.Slot) and then
         (for all I in Items'Range => (if I > Topmost'Result.Slot then not Items (I))));
end Compositor_Focus;
