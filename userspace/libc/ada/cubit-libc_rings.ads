------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The byte ring behind a pipe or one direction of a socket pair
--  (docs/c-removal.md): in-process, shared by its read and write ends.
--
--  @description
--  Proved (tests/libc-ada): every index stays in the ring, Put adds as
--  many bytes as fit (a short write is allowed), Take removes as many as
--  are wanted and present, the bytes come out in the order they went in,
--  and the end counts never underflow.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Libc_Rings with Pure, SPARK_Mode is

   Ring_Bytes : constant := 65_536;
   subtype Ring_Index is Natural range 0 .. Ring_Bytes - 1;
   subtype Ring_Count is Natural range 0 .. Ring_Bytes;
   type Ring_Data is array (Ring_Index) of Unsigned_8;

   --  Ends: how many descriptors read and write the ring (dup adds ends).
   Maximum_Ends : constant := 1_024;
   subtype End_Count is Natural range 0 .. Maximum_Ends;

   type Ring is record
      Data    : Ring_Data;
      Head    : Ring_Index := 0;    --  the oldest byte
      Length  : Ring_Count := 0;
      Readers : End_Count := 0;
      Writers : End_Count := 0;
   end record;

   type Bytes is array (Positive range <>) of Unsigned_8;

   --  The K'th byte waiting (0: the oldest).
   function Waiting (R : Ring; K : Ring_Index) return Unsigned_8 is
     (R.Data ((R.Head + K) mod Ring_Bytes));

   function Room (R : Ring) return Ring_Count is (Ring_Bytes - R.Length);

   --  Add Source's first Done bytes, as many as fit.
   procedure Put (R : in out Ring; Source : Bytes; Done : out Ring_Count)
   with Pre => Source'Length <= Natural'Last - Ring_Bytes,
        Post => Done = Natural'Min (Source'Length, Room (R'Old))
                and then R.Length = R'Old.Length + Done
                and then R.Head = R'Old.Head
                and then R.Readers = R'Old.Readers
                and then R.Writers = R'Old.Writers
                and then (for all K in 0 .. R'Old.Length - 1 =>
                            Waiting (R, K) = Waiting (R'Old, K))
                and then (for all K in 0 .. Done - 1 =>
                            Waiting (R, R'Old.Length + K) = Source (Source'First + K));

   --  Remove the oldest Count bytes into Target, as many as wanted and
   --  waiting.
   procedure Take (R : in out Ring; Target : out Bytes; Count : out Ring_Count)
   with Pre => Target'Length <= Natural'Last - Ring_Bytes,
        Post => Count = Natural'Min (Target'Length, R'Old.Length)
                and then R.Length = R'Old.Length - Count
                and then R.Readers = R'Old.Readers
                and then R.Writers = R'Old.Writers
                and then (for all K in 0 .. Count - 1 =>
                            Target (Target'First + K) = Waiting (R'Old, K));

end CuBit.Libc_Rings;
