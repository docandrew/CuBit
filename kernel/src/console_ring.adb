package body Console_Ring with
   SPARK_Mode => On
is
   procedure Put (R : in out Ring; C : Character) is
   begin
      R.Data (Slot (R.First, R.Used)) := C;
      R.Used := R.Used + 1;
   end Put;

   procedure Take (R : in out Ring; Into : out Batch; Count : out Batch_Count) is
   begin
      Into := (others => ASCII.NUL);
      Count := Byte_Count'Min (R.Used, Batch_Capacity);
      for I in 1 .. Natural (Count) loop
         Into (I) := R.Data (Slot (R.First, Byte_Count (I - 1)));
         pragma Loop_Invariant
           (for all J in 1 .. I => Into (J) = Peek (R, Byte_Count (J - 1)));
      end loop;
      R.First := (if Count < R.Used then Slot (R.First, Count) else 0);
      R.Used := R.Used - Count;
   end Take;
end Console_Ring;
