------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  SameBoy's audio batch (docs/c-removal.md): stereo frames from the
--  core's per-sample callback, written to the mixer stream in order. The
--  stream may accept a prefix; the rest stays for the next write.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.SameBoy_Batches with SPARK_Mode, Pure is

   Capacity : constant := 2048;
   subtype Frame_Count is Natural range 0 .. Capacity;
   subtype Frame_Index is Natural range 0 .. Capacity - 1;

   --  GB_sample_t: signed 16-bit left then right, as one 32-bit word.
   type Stereo_Frame is new Unsigned_32;
   type Frame_Array is array (Frame_Index) of Stereo_Frame
     with Convention => C;

   --  Frames First .. Count - 1 are waiting to be written.
   type Batch is record
      Frames   : Frame_Array;
      First    : Frame_Count := 0;
      Count    : Frame_Count := 0;
      Overflow : Boolean := False;
   end record
     with Predicate => First <= Count;

   function Waiting (B : Batch) return Frame_Count is (B.Count - B.First);

   procedure Clear (B : in out Batch)
     with Post => Waiting (B) = 0 and then not B.Overflow;

   --  A frame arriving while the batch is full is lost and noted.
   procedure Append (B : in out Batch; Frame : Stereo_Frame)
     with Post => (if B'Old.Count < Capacity
                   then B.Count = B'Old.Count + 1 and then
                        B.Overflow = B'Old.Overflow
                   else B.Overflow and then B.Count = Capacity);

   --  The stream took the first Written waiting frames; an emptied batch
   --  starts over from the front.
   procedure Accept_Written (B : in out Batch; Written : Frame_Count)
     with Pre  => Written <= Waiting (B),
          Post => Waiting (B) = Waiting (B'Old) - Written and then
                  (if Waiting (B) = 0 then B.Count = 0);

end CuBit.SameBoy_Batches;
