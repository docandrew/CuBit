with Interfaces; use Interfaces;
generic
   First, Last : Unsigned_16;
package Intel_GPU_Fence_Ranges with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   pragma Compile_Time_Error (First = 0 or First > Last, "invalid fence pool");
   -- One serialized ledger per CT transport lifetime. No release/reset:
   -- late replies may still name every previously allocated fence.
   type Ledger is limited private;
   function Cursor (Object : Ledger) return Natural;
   function Remaining (Object : Ledger) return Natural;
   procedure Reserve
     (Object : in out Ledger; Count : Natural;
      Range_First, Range_Last : out Unsigned_16; Accepted : out Boolean)
   with Post =>
     Accepted = (Count >= 4 and Count <= Remaining (Object)'Old) and then
     (if Accepted then
        Natural (Range_First) = Cursor (Object)'Old and
        Natural (Range_Last) = Cursor (Object)'Old + Count - 1 and
        Range_First >= First and Range_Last <= Last and
        Cursor (Object) = Cursor (Object)'Old + Count
      else Range_First = 0 and Range_Last = 0 and
        Cursor (Object) = Cursor (Object)'Old);
private
   type Ledger is limited record
      Next : Natural range Natural (First) .. Natural (Last) + 1 := Natural (First);
   end record;
end Intel_GPU_Fence_Ranges;
