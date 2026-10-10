package body Files_Marks with SPARK_Mode is
   procedure Clear (M : out Mark_State) is
   begin
      M.Bits := [others => False];
      M.Total := 0;
      M.Size := 0;
   end Clear;

   procedure Set (M : in out Mark_State; Id : Entry_Id; Value : Boolean; Size : Unsigned_64) is
   begin
      if Id > M.Capacity or else M.Bits (Id) = Value then
         return;
      end if;
      M.Bits (Id) := Value;
      if Value then
         M.Total := (if M.Total < M.Capacity then M.Total + 1 else M.Capacity);
         M.Size := (if Size > Unsigned_64'Last - M.Size then Unsigned_64'Last else M.Size + Size);
      else
         M.Total := (if M.Total > 0 then M.Total - 1 else 0);
         M.Size := (if Size <= M.Size then M.Size - Size else 0);
      end if;
   end Set;
end Files_Marks;
