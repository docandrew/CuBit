-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Process identities (KERN-003, docs/process-objects.md): the one 64-bit
-- word that names a process wherever it crosses the kernel boundary.
--
-- @description
-- An identity is a slot (the kernel's internal index, low 24 bits) and that
-- slot's generation (high 40 bits). A slot's generation only advances, and
-- a slot at its limit is retired, never reused (Id_Ledger), so an identity
-- is never handed out twice. Lookup is decode, index, compare: constant
-- time, no search.
--
-- It is a private type: equality, encoding and decoding are all the kernel
-- does with one. Only the system-call boundary turns it into a register or
-- message word (To_Word, From_Word). Userspace treats it as opaque
-- (CuBit.Process_IDs). No_Identity names no process.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Process_Identities with
    SPARK_Mode => On,
    Pure
is
    type Identity is private;
    No_Identity : constant Identity;

    Slot_Bits       : constant := 24;
    Generation_Bits : constant := 64 - Slot_Bits;

    subtype Slot is Unsigned_64 range 0 .. 2 ** Slot_Bits - 1;
    subtype Generation is Unsigned_64 range 0 .. 2 ** Generation_Bits - 1;

    function Slot_Of (I : Identity) return Slot;
    function Generation_Of (I : Identity) return Generation;

    function Encode (S : Slot; G : Generation) return Identity
      with Post => Slot_Of (Encode'Result) = S and then
                   Generation_Of (Encode'Result) = G;

    -- The word a register or message carries.
    function To_Word (I : Identity) return Unsigned_64;
    function From_Word (W : Unsigned_64) return Identity
      with Post => To_Word (From_Word'Result) = W;

    -- Distinct (slot, generation) pairs give distinct identities, and a
    -- nonzero slot never gives No_Identity.
    procedure Encode_Injective (S1, S2 : Slot; G1, G2 : Generation)
      with Ghost,
           Post => (if Encode (S1, G1) = Encode (S2, G2) then S1 = S2 and then G1 = G2)
                   and then (if S1 /= 0 then Encode (S1, G1) /= No_Identity);

    -- Every identity is the encoding of its own parts.
    procedure Decode_Encode (I : Identity)
      with Ghost,
           Post => Encode (Slot_Of (I), Generation_Of (I)) = I;

private
    type Identity is new Unsigned_64;
    No_Identity : constant Identity := 0;

    Slot_Mask : constant Unsigned_64 := 2 ** Slot_Bits - 1;

    function Slot_Of (I : Identity) return Slot is
      (Unsigned_64 (I) and Slot_Mask);
    function Generation_Of (I : Identity) return Generation is
      (Shift_Right (Unsigned_64 (I), Slot_Bits));
    function Encode (S : Slot; G : Generation) return Identity is
      (Identity (Shift_Left (G, Slot_Bits) or S));
    function To_Word (I : Identity) return Unsigned_64 is (Unsigned_64 (I));
    function From_Word (W : Unsigned_64) return Identity is (Identity (W));
end Process_Identities;
