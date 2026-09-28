------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A TCP connection's receive queue with out-of-order reassembly.
--
--  A ring the size of the receive window holds bytes by sequence number,
--  from the first byte the application has not read (Read_Start). A
--  presence bit per byte records which have arrived. The first Ready bytes
--  are contiguous (RCV.NXT = Read_Start + Ready); later ones may have gaps
--  (out-of-order data, kept until the gap fills). Arriving text and reads
--  are copied as at most two slices (the ring wraps once); in-order text
--  moves RCV.NXT past itself at once, and only bytes that arrived out of
--  order are scanned, once, when their gap fills. Memory is the window plus
--  one flag per byte.
--
--  Proved (tests/net-tcp): every stored byte is the last that arrived at
--  its sequence number; bytes below RCV.NXT are never rewritten (text is
--  only inserted at or past it) and all are present; bytes outside an
--  insert are unchanged; RCV.NXT only advances; reading returns exactly
--  the in-order bytes and moves the rest along unchanged.
------------------------------------------------------------------------------
with Interfaces;   use Interfaces;
with TCP_Sequence; use TCP_Sequence;

with TCP_Limits;

generic
   Capacity : Positive;
package TCP_Receive_Queue with SPARK_Mode is
   pragma Compile_Time_Error (Capacity > TCP_Limits.Maximum_Scaled_Window,
      "a receive window must stay below a quarter of the sequence space");

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   --  Bytes held, and a byte's offset from Read_Start.
   subtype Byte_Count  is Natural range 0 .. Capacity;
   subtype Byte_Offset is Byte_Count range 0 .. Capacity - 1;

   type Queue is private;

   function Read_Start (Q : Queue) return Seq;
   function Ready (Q : Queue) return Byte_Count;      --  contiguous bytes
   function Rcv_Nxt (Q : Queue) return Seq is (Read_Start (Q) + Seq (Ready (Q)));
   function Window (Q : Queue) return Byte_Count is (Capacity - Ready (Q));

   --  Offsets count from Read_Start.
   function Present (Q : Queue; Off : Byte_Offset) return Boolean;
   function Element (Q : Queue; Off : Byte_Offset) return Unsigned_8;

   --  Everything below RCV.NXT has arrived.
   function Contiguous (Q : Queue) return Boolean is
     (for all Off in 0 .. Ready (Q) - 1 => Present (Q, Off));

   procedure Initialize (Q : out Queue; Start : Seq) with
     Post => Read_Start (Q) = Start and then Ready (Q) = 0 and then
             (for all Off in 0 .. Capacity - 1 => not Present (Q, Off));

   --  How many of Length bytes at Offset fit in the window.
   function Fitting (Offset : Byte_Offset; Length : Natural) return Byte_Count is
     (Natural'Min (Length, Capacity - Offset));

   --  Bytes that arrived at sequence number First (Data (Data'First) is at
   --  First). Offset is where First falls from Read_Start (computed by the
   --  caller as Distance (Read_Start, First) when that is below Capacity),
   --  never below RCV.NXT: delivered-in-order bytes are never rewritten.
   procedure Insert (Q : in out Queue; Offset : Byte_Offset; Data : Byte_Array)
   with
     Pre  => Contiguous (Q) and then Offset >= Ready (Q) and then
             Data'Length <= Capacity and then Data'First = 1,
     Post => Contiguous (Q) and then
             Read_Start (Q) = Read_Start (Q'Old) and then
             Ready (Q) >= Ready (Q'Old) and then
             --  Bytes outside the insert are unchanged ...
             (for all Off in 0 .. Capacity - 1 =>
                (if Off < Offset or else Off - Offset >= Fitting (Offset, Data'Length) then
                   Present (Q, Off) = Present (Q'Old, Off) and then
                   (if Present (Q, Off) then Element (Q, Off) = Element (Q'Old, Off)))) and then
             --  ... and those inside are what arrived, at their positions.
             (for all K in 1 .. Fitting (Offset, Data'Length) =>
                Present (Q, Offset + K - 1) and then Element (Q, Offset + K - 1) = Data (K)) and then
             --  In-order text moves RCV.NXT past itself.
             (if Offset = Ready (Q'Old) then Ready (Q) >= Offset + Fitting (Offset, Data'Length));

   --  The application reads up to Out'Length in-order bytes.
   procedure Read (Q : in out Queue; Output : out Byte_Array; Got : out Byte_Count)
   with
     Pre  => Contiguous (Q) and then Output'First = 1,
     Post => Contiguous (Q) and then
             Got = Natural'Min (Output'Length, Ready (Q'Old)) and then
             Ready (Q) = Ready (Q'Old) - Got and then
             Read_Start (Q) = Read_Start (Q'Old) + Seq (Got) and then
             (for all K in 1 .. Got => Output (K) = Element (Q'Old, K - 1)) and then
             --  What remains moves along unchanged ...
             (for all Off in 0 .. Capacity - 1 - Got =>
                Present (Q, Off) = Present (Q'Old, Off + Got) and then
                (if Present (Q, Off) then Element (Q, Off) = Element (Q'Old, Off + Got))) and then
             --  ... and the freed slots, now the far end of the window, are
             --  empty (a stale bit would make new data look like a duplicate).
             (for all Off in Capacity - Got .. Capacity - 1 => not Present (Q, Off));

private
   subtype Index is Byte_Offset;
   type Raw is array (Natural range <>) of Unsigned_8;
   subtype Storage is Raw (Index);
   --  One byte per flag, not packed bits: faster to test and set, and
   --  comparing whole arrays needs no bit-operation support from the
   --  run-time library.
   type Bits is array (Index) of Boolean;

   type Queue is record
      Data  : Storage := [others => 0];
      Have  : Bits := [others => False];
      Head  : Index := 0;                        --  ring index of Read_Start
      Count : Byte_Count := 0;  --  Ready
      Start : Seq := 0;                          --  Read_Start
   end record;

   --  The ring position of an offset: one conditional subtraction (Head
   --  and Off are both below Capacity), keeping proofs linear.
   function Slot (Q : Queue; Off : Byte_Offset) return Index is
     (if Q.Head + Off < Capacity then Q.Head + Off else Q.Head + Off - Capacity);

   function Read_Start (Q : Queue) return Seq is (Q.Start);
   function Ready (Q : Queue) return Byte_Count is (Q.Count);
   function Present (Q : Queue; Off : Byte_Offset) return Boolean is (Q.Have (Slot (Q, Off)));
   function Element (Q : Queue; Off : Byte_Offset) return Unsigned_8 is (Q.Data (Slot (Q, Off)));
end TCP_Receive_Queue;
