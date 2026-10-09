-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Request credits (docs/ipc-delivery.md, "Requests travel in per-capability
-- queues with credits"): one class of a receiver's queue, one small ring
-- per sending process.
--
-- @description
-- Every sending process has its own ring of Each entries at the receiver,
-- indexed directly by its process number (no search):
--   - an admitted message always has a place (nothing accepted is dropped);
--   - a sender under its credit is always admitted, however much the others
--     send (a flooder exhausts only its own ring);
--   - a sender at its credit is told Busy and keeps its message;
--   - Take serves senders round-robin from the one after the last served,
--     so a busy sender delays another by at most one message per turn;
--     each sender's messages stay in order.
-- A ring holds one life of its sender (Generation); a sender that ended
-- is forgotten before its process number is reused.
--
-- This models indices only: the kernel keeps the messages in storage of
-- Each entries per sender, entry (Sender, position). KERN-003 (dynamic
-- processes) replaces the direct index with a per-receiver table.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Kernel_Credits with
    SPARK_Mode => On
is
    Maximum_Senders : constant := 255;
    subtype Sender is Positive range 1 .. Maximum_Senders;

    Maximum_Credit : constant := 64;
    subtype Credit is Positive range 1 .. Maximum_Credit;
    subtype Position is Natural range 0 .. Maximum_Credit - 1;
    subtype Queued_Count is Natural range 0 .. Maximum_Credit;

    type Sender_Ring is record
        Generation : Unsigned_64 := 0;
        Head       : Position := 0;
        Count      : Queued_Count := 0;
    end record;
    type Ring_Array is array (Sender) of Sender_Ring;

    -- Which rings hold anything, one bit per sender (sender S is bit
    -- (S - 1) mod 64 of word (S - 1) / 64): finding the next is a
    -- count-trailing-zeros, not a scan.
    Bits_Per_Word : constant := 64;
    subtype Word_Index is Natural range 0 .. 3;
    subtype Bit_Index is Natural range 0 .. Bits_Per_Word - 1;
    type Sender_Bits is array (Word_Index) of Unsigned_64;

    function Word_Of (S : Sender) return Word_Index is ((S - 1) / Bits_Per_Word);
    function Bit_Of (S : Sender) return Bit_Index is ((S - 1) mod Bits_Per_Word);
    function Mask (B : Bit_Index) return Unsigned_64 is (Shift_Left (1, B));
    function Is_Set (W : Sender_Bits; S : Sender) return Boolean is
      ((W (Word_Of (S)) and Mask (Bit_Of (S))) /= 0);

    -- The index of X's lowest set bit: the processor's count-trailing-
    -- zeros (GCC's __builtin_ctzll). Its contract is assumed, not proved:
    -- it states what the instruction does.
    function Trailing_Zeros (X : Unsigned_64) return Integer
      with Import, Convention => Intrinsic, External_Name => "__builtin_ctzll",
           Global => null,
           Pre  => X /= 0,
           Post => Trailing_Zeros'Result in Bit_Index and then
                   (X and Mask (Trailing_Zeros'Result)) /= 0 and then
                   (for all B in Bit_Index =>
                      (if B < Trailing_Zeros'Result then (X and Mask (B)) = 0));

    type Receiver_State is record
        Each     : Credit := 1;
        Rings    : Ring_Array;
        -- Which rings hold anything (Count > 0).
        Nonempty : Sender_Bits := (others => 0);
        -- Where Take looks first: the sender after the last one served.
        Cursor   : Sender := Sender'First;
    end record;

    -- Bit 63 of word 3 would be sender 256, which does not exist.
    Last_Word_Unused : constant Unsigned_64 := Mask (Bits_Per_Word - 1);

    function Valid (R : Receiver_State) return Boolean is
      ((for all S in Sender =>
          R.Rings (S).Count <= R.Each and then
          R.Rings (S).Head < R.Each and then
          Is_Set (R.Nonempty, S) = (R.Rings (S).Count > 0))
       and then (R.Nonempty (Word_Index'Last) and Last_Word_Unused) = 0);

    function Empty (R : Receiver_State) return Boolean is
      (for all W in Word_Index => R.Nonempty (W) = 0);

    -- Where a sender's message at At_Position lives, in its Each entries.
    function Entry_Of (R : Receiver_State; From : Sender; At_Position : Position)
      return Natural is
      ((From - 1) * R.Each + At_Position)
      with Pre => At_Position < R.Each;

    -- In place (the state is large: no temporary on a kernel stack).
    procedure Initialize (R : in out Receiver_State; Each : Credit)
      with Post => Valid (R) and then R.Each = Each and then Empty (R);

    type Admit_Result is (Admitted, Busy);

    -- From (this life) sends a message. Admitted: keep it at At_Position of
    -- From's ring. Busy: From's ring is full, or holds an earlier life's
    -- messages (it is forgotten when that life ends).
    procedure Admit
      (R : in out Receiver_State; From : Sender; Generation : Unsigned_64;
       At_Position : out Position; Result : out Admit_Result)
      with Pre  => Valid (R),
           Post => Valid (R) and then R.Each = R'Old.Each and then
                   (if Result = Admitted then
                      At_Position < R.Each and then
                      R.Rings (From).Count = R'Old.Rings (From).Count + 1 and then
                      R.Rings (From).Generation = Generation and then
                      At_Position = (R'Old.Rings (From).Head + R'Old.Rings (From).Count)
                                      mod R.Each
                    else R = R'Old) and then
                   -- Every other sender's ring is untouched.
                   (for all S in Sender => (if S /= From then R.Rings (S) = R'Old.Rings (S))) and then
                   -- Isolation: a sender under its credit is admitted.
                   (if R'Old.Rings (From).Count < R'Old.Each and then
                       (R'Old.Rings (From).Count = 0 or else
                        R'Old.Rings (From).Generation = Generation)
                    then Result = Admitted);

    -- The receiver takes the next message, round-robin across senders.
    procedure Take
      (R : in out Receiver_State; From : out Sender; At_Position : out Position;
       Found : out Boolean)
      with Pre  => Valid (R),
           Post => Valid (R) and then R.Each = R'Old.Each and then
                   Found = not Empty (R'Old) and then
                   (if Found then
                      R'Old.Rings (From).Count > 0 and then
                      At_Position = R'Old.Rings (From).Head and then
                      R.Rings (From).Count = R'Old.Rings (From).Count - 1 and then
                      (for all S in Sender =>
                         (if S /= From then R.Rings (S) = R'Old.Rings (S)))
                    else R.Rings = R'Old.Rings);

    -- From ended: its ring and what it held go (no one can be answered).
    procedure Forget (R : in out Receiver_State; From : Sender)
      with Pre  => Valid (R),
           Post => Valid (R) and then R.Each = R'Old.Each and then
                   R.Rings (From).Count = 0 and then
                   (for all S in Sender => (if S /= From then R.Rings (S) = R'Old.Rings (S)));

end Kernel_Credits;
