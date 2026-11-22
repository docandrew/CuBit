package body Copy_Slices with SPARK_Mode is

   procedure Admit
     (Source_At, Target_At, Length : Unsigned_64; C : out Copy; Admitted : out Boolean) is
   begin
      C := (others => <>);
      Admitted := Source_At <= Maximum_Offset and then Target_At <= Maximum_Offset
        and then Length /= 0
        and then (Length = Copy_To_End or else Length <= Maximum_Offset);
      if Admitted then
         C := (Source_At => Source_At, Target_At => Target_At,
               Wanted => (if Length = Copy_To_End then Maximum_Offset else Length), Done => 0);
      end if;
   end Admit;

   function Next (C : Copy; Source_Size : Unsigned_64; Slice_Limit : Positive) return Unsigned_64 is
      Position : constant Unsigned_64 := C.Source_At + C.Done;
      Length : Unsigned_64 := Unsigned_64'Min (C.Wanted - C.Done, Unsigned_64 (Slice_Limit));
   begin
      if Source_Size <= Position then
         return 0;
      end if;
      Length := Unsigned_64'Min (Length, Source_Size - Position);
      return Length;
   end Next;

   procedure Advance (C : in out Copy; Copied : Unsigned_64) is
   begin
      C.Done := C.Done + Copied;
   end Advance;

end Copy_Slices;
