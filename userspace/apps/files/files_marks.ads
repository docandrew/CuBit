with Interfaces; use Interfaces;
with Files_Limits; use Files_Limits;

--  The entries a pane has marked for an operation (docs/files-app.md), by
--  Entry_Id, so marks survive sorting and filtering. The count and the
--  marked bytes are kept with every change: the status line never scans.
--  Pure policy, proved (tests/files-app/files_proof.gpr).
package Files_Marks with SPARK_Mode is
   type Mark_State (Capacity : Entry_Capacity) is private;

   function Marked (M : Mark_State; Id : Entry_Id) return Boolean;
   function Count (M : Mark_State) return Entry_Count;
   --  Bytes of the marked files, saturating.
   function Bytes (M : Mark_State) return Unsigned_64;

   procedure Clear (M : out Mark_State)
     with Post => Count (M) = 0 and then Bytes (M) = 0;
   --  Mark Id or not; Size is its bytes (0 for a folder).
   procedure Set (M : in out Mark_State; Id : Entry_Id; Value : Boolean; Size : Unsigned_64)
     with Post => (if Id <= M.Capacity then Marked (M, Id) = Value) and then Count (M) <= M.Capacity;
private
   type Mark_Bits is array (Entry_Id range <>) of Boolean with Pack;
   type Mark_State (Capacity : Entry_Capacity) is record
      Total : Entry_Count := 0;
      Size : Unsigned_64 := 0;
      Bits : Mark_Bits (1 .. Capacity) := [others => False];
   end record;
   function Marked (M : Mark_State; Id : Entry_Id) return Boolean is
     (Id <= M.Capacity and then M.Bits (Id));
   function Count (M : Mark_State) return Entry_Count is
     (if M.Total <= M.Capacity then M.Total else M.Capacity);
   function Bytes (M : Mark_State) return Unsigned_64 is (M.Size);
end Files_Marks;
