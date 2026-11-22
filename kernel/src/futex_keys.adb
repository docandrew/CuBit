package body Futex_Keys with SPARK_Mode => On is

    Mix : constant := 16#9E37_79B9#;

    function Grant_Bucket (Slot : Unsigned_32) return Bucket_Index is
      (Bucket_Index ((Unsigned_64 (Slot) * Mix) mod Bucket_Count));

    function Bucket_Of (K : Key) return Bucket_Index is
    begin
        case K.Space is
            when Shared_Space =>
                return Grant_Bucket (K.Owner);
            when Private_Space =>
                declare
                    -- Word-aligned addresses: drop the low bits, then mix in
                    -- the owner.
                    H : constant Unsigned_64 :=
                      Shift_Right (K.Address, 2) xor Shift_Right (K.Address, 12) xor
                      (Unsigned_64 (K.Owner) * Mix);
                begin
                    return Bucket_Index (H mod Bucket_Count);
                end;
        end case;
    end Bucket_Of;

    procedure Prove_Spaces_Disjoint (P : Unsigned_32; A : Unsigned_64;
                                     S, G : Unsigned_32; O : Unsigned_64) is null;

    procedure Prove_Grant_One_Bucket (S, G1, G2 : Unsigned_32; O1, O2 : Unsigned_64) is null;

    procedure Prove_Lives_Distinct (S, G1, G2 : Unsigned_32; O : Unsigned_64) is null;

end Futex_Keys;
