package body Futex_Keys with SPARK_Mode => On is

    function Bucket_Of (K : Key) return Bucket_Index is
        -- Word-aligned addresses: drop the low bits, then mix in the owner.
        H : constant Unsigned_64 :=
          Shift_Right (K.Address, 2) xor Shift_Right (K.Address, 12) xor
          (Unsigned_64 (K.Owner) * 16#9E37_79B9#);
    begin
        return Bucket_Index (H mod Bucket_Count);
    end Bucket_Of;

end Futex_Keys;
