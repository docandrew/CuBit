-------------------------------------------------------------------------------
-- CuBit OS
--
-- Console_Ring: the kernel console's bounded byte FIFO
--
-- Prints copy text here instead of writing the UART a byte at a time; an
-- idle CPU (or the stalled-console fallback on CPU 0's timer) drains it in
-- small batches. The FIFO never loses a byte: a writer finding it full drains
-- it synchronously first (docs/development-backlog.md, "Console output is
-- synchronous serial I/O"). Pure, sequential logic; TextIO's output lock
-- serializes every call.
-------------------------------------------------------------------------------
package Console_Ring with
   SPARK_Mode => On
is
   Capacity : constant := 65_536;
   --  Bytes one drain step takes: the 16550A transmit FIFO depth, so a batch
   --  written when the holding register is empty is never overrun.
   Batch_Capacity : constant := 16;

   type Byte_Count is range 0 .. Capacity;
   type Byte_Index is range 0 .. Capacity - 1;
   subtype Batch_Count is Byte_Count range 0 .. Batch_Capacity;
   subtype Batch_Index is Positive range 1 .. Batch_Capacity;
   type Batch is array (Batch_Index) of Character;

   type Storage is array (Byte_Index) of Character;
   type Ring is record
      Data  : Storage := (others => ASCII.NUL);
      First : Byte_Index := 0;   --  the oldest byte, when Used > 0
      Used  : Byte_Count := 0;
   end record;

   function Is_Empty (R : Ring) return Boolean is (R.Used = 0);
   function Is_Full (R : Ring) return Boolean is (R.Used = Capacity);
   function Length (R : Ring) return Byte_Count is (R.Used);

   --  The storage slot Offset places after First, wrapping once.
   function Slot (First : Byte_Index; Offset : Byte_Count) return Byte_Index is
     (if Byte_Count (First) + Offset < Capacity
      then Byte_Index (Byte_Count (First) + Offset)
      else Byte_Index (Byte_Count (First) + Offset - Capacity))
   with Pre => Offset < Capacity;

   --  The byte Offset places after the oldest one.
   function Peek (R : Ring; Offset : Byte_Count) return Character is
     (R.Data (Slot (R.First, Offset)))
   with Pre => Offset < R.Used;

   procedure Put (R : in out Ring; C : Character)
   with Pre  => not Is_Full (R),
        Post => R.Used = R.Used'Old + 1 and then R.First = R.First'Old
                and then Peek (R, R.Used - 1) = C
                and then (for all I in 0 .. R.Used'Old - 1 =>
                            Peek (R, I) = Peek (R'Old, I));

   --  Remove up to Batch_Capacity of the oldest bytes, in order.
   procedure Take (R : in out Ring; Into : out Batch; Count : out Batch_Count)
   with Post => Count = Byte_Count'Min (R.Used'Old, Batch_Capacity)
                and then R.Used = R.Used'Old - Count
                and then (for all I in 1 .. Natural (Count) =>
                            Into (I) = Peek (R'Old, Byte_Count (I - 1)))
                and then (for all I in 0 .. R.Used - 1 =>
                            Peek (R, I) = Peek (R'Old, I + Count));
end Console_Ring;
