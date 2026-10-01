package body Intel_GPU_Fence_Ranges with SPARK_Mode is
   function Cursor (Object : Ledger) return Natural is (Object.Next);
   function Remaining (Object : Ledger) return Natural is
     (Natural (Last) + 1 - Object.Next);
   procedure Reserve
     (Object : in out Ledger; Count : Natural;
      Range_First, Range_Last : out Unsigned_16; Accepted : out Boolean) is
   begin
      Range_First := 0; Range_Last := 0; Accepted := False;
      if Count < 4 or Count > Remaining (Object) then return; end if;
      Range_First := Unsigned_16 (Object.Next);
      Range_Last := Unsigned_16 (Object.Next + Count - 1);
      Object.Next := Object.Next + Count;
      Accepted := True;
   end Reserve;
end Intel_GPU_Fence_Ranges;
