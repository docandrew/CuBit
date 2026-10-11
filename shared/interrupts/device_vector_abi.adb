package body Device_Vector_ABI with
   SPARK_Mode => On
is
   procedure Allocation_Round_Trip
     (Head : Device_Vector; Count : Block_Size; Destination : Destination_ID)
   is
      H : constant Unsigned_64 := Unsigned_64 (Head);
      C : constant Unsigned_64 := Unsigned_64 (Count);
      D : constant Unsigned_64 := Unsigned_64 (Destination);
      Upper : constant Unsigned_64 := C + D * Field_Radix;
      W : constant Unsigned_64 := Allocation_Word (Head, Count, Destination);
   begin
      pragma Assert (W = H + Upper * Field_Radix);
      pragma Assert (H < Field_Radix);
      pragma Assert (W / Head_Scale = W);
      pragma Assert (W mod Field_Radix = H);
      pragma Assert (W / Count_Scale = Upper);
      pragma Assert (Upper mod Field_Radix = C);
      pragma Assert (W / Destination_Scale = D);
      pragma Assert (D mod Field_Radix = D);
      pragma Assert (Field (W, Head_Scale) = H);
      pragma Assert (Field (W, Count_Scale) = C);
      pragma Assert (Field (W, Destination_Scale) = D);
      pragma Assert (W < Word_Limit);
      pragma Assert (Field (W, Head_Scale) in First_Vector .. Last_Vector);
      pragma Assert (Field (W, Count_Scale) in 1 .. Max_Block_Vectors);
      pragma Assert (Field (W, Destination_Scale) <= Unsigned_64 (Destination_ID'Last));
      pragma Assert (Valid_Allocation (W));
      pragma Assert (Natural (H) = Head);
      pragma Assert (Head_Of (W) = Head);
      pragma Assert (Natural (C) = Count);
      pragma Assert (Count_Of (W) = Count);
      pragma Assert (Natural (D) = Destination);
      pragma Assert (Destination_Of (W) = Destination);
   end Allocation_Round_Trip;

   procedure Pending_Bits_Distinct (A, B : Device_Vector) is null;
end Device_Vector_ABI;
