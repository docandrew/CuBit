package body Futex_Keys with SPARK_Mode => On is

    Mix : constant := 16#9E37_79B9#;

    function Grant_Bucket (Slot : Unsigned_32) return Bucket_Index is
      (Bucket_Index ((Unsigned_64 (Slot) * Mix) mod Bucket_Count));

    function Bucket_Of (K : Key) return Bucket_Index is (Grant_Bucket (K.Owner));

    procedure Prove_Grant_One_Bucket (S, G1, G2 : Unsigned_32; O1, O2 : Unsigned_64) is null;

    procedure Prove_Lives_Distinct (S, G1, G2 : Unsigned_32; O : Unsigned_64) is null;

end Futex_Keys;
