with Interfaces;
--  Audited pointer bridge: the application icon atlas's pixels, one block
--  from CuAlloc (userspace/allocator), the process's one allocator. The
--  block is sized by the icon package's layout (every icon at every
--  exported edge, Desktop_Icons), reserved once when the atlas is first
--  loaded and kept for the process's life: there is no fixed array in the
--  executable and no limit beyond the package itself. Reserve answers
--  False when the heap is out; the store then draws the fallback glyph.
private package Desktop_Icon_Store.Atlas_Memory with SPARK_Mode,
  Abstract_State => (Block with Part_Of => Desktop_Icon_Store.State),
  Initializes => Block
is
   use type Q.Phase, Q.Failure;
   subtype Atlas_Index is Natural range 0 .. Atlas_Pixels - 1;

   function Reserved return Boolean with Global => Block;
   procedure Reserve (Done : out Boolean)
     with Global => (In_Out => Block), Post => Done = Reserved;
   --  Decode Input into the block (CuBit.QOI.Feed on it).
   procedure Feed (D : in out Q.Decoder; Input : Q.Byte_Array)
     with Global => (In_Out => Block),
          Pre  => Reserved and then Q.Valid (D) and then Q.Limit (D) <= Atlas_Pixels,
          Post => Reserved and then Q.Valid (D) and then Q.Limit (D) = Q.Limit (D'Old) and then
                  Q.Written (D) >= Q.Written (D'Old) and then
                  Q.Written (D) <= Q.Total (D) and then Q.Total (D) <= Q.Limit (D) and then
                  (if Q.Current (D'Old) = Q.Failed then Q.Current (D) = Q.Failed) and then
                  (if Q.Current (D) = Q.Complete then Q.Written (D) = Q.Total (D)) and then
                  (if Q.Current (D) = Q.Failed then Q.Error (D) /= Q.No_Failure);
   --  The pixel at Index; zero (transparent) while nothing is reserved.
   function Read (Index : Atlas_Index) return Interfaces.Unsigned_32
     with Global => Block;
end Desktop_Icon_Store.Atlas_Memory;
