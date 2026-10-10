------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Server-side copy (docs/filesystem-protocol-v2.md step 5): the bounded
--  slices filesystem.svc copies a range in, between its other work, and how
--  a copy ends.
--
--  @description
--  A copy is admitted with its source and target offsets and a length (or
--  Copy_To_End: up to the source's end, as it is when each slice is cut).
--  Next cuts the next slice: no longer than the slice limit, the bytes
--  still wanted, or what the source still holds; an empty slice ends the
--  copy (Complete). Offsets stay below Maximum_Offset, so no sum of an
--  offset and a byte count overflows (-gnatp safe). Done only grows and
--  never passes Wanted: the copied bytes are always a prefix of the range.
--  Proved (tests/filesystem-copy, level 2): no run-time errors, and the
--  contracts below.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Copy_Slices with SPARK_Mode is

   --  Larger than any ext2 file (2 TiB with 4 KiB blocks), far below 2**64.
   Maximum_Offset : constant := 2 ** 48;
   subtype File_Offset is Unsigned_64 range 0 .. Maximum_Offset;
   --  The request's Length meaning "to the source's end".
   Copy_To_End : constant Unsigned_64 := Unsigned_64'Last;

   type Copy is record
      Source_At : File_Offset := 0;
      Target_At : File_Offset := 0;
      Wanted    : File_Offset := 0;   --  Maximum_Offset for Copy_To_End
      Done      : File_Offset := 0;
   end record;

   function Valid (C : Copy) return Boolean is (C.Done <= C.Wanted);

   --  Refused: an offset past Maximum_Offset, a zero length, or a length
   --  (other than Copy_To_End) past Maximum_Offset.
   procedure Admit
     (Source_At, Target_At, Length : Unsigned_64; C : out Copy; Admitted : out Boolean)
   with Post => (if Admitted then Valid (C) and then C.Done = 0
                   and then C.Source_At = Source_At and then C.Target_At = Target_At
                   and then C.Wanted = (if Length = Copy_To_End then Maximum_Offset else Length)
                   and then C.Wanted > 0);

   --  The next slice's length; 0 when the copy is complete.
   function Next (C : Copy; Source_Size : Unsigned_64; Slice_Limit : Positive) return Unsigned_64
   with Pre  => Valid (C),
        Post => Next'Result <= Unsigned_64 (Slice_Limit)
                and then Next'Result <= C.Wanted - C.Done
                and then (if Next'Result > 0 then Source_Size > C.Source_At + C.Done
                            and then Next'Result <= Source_Size - (C.Source_At + C.Done));

   --  Copied bytes of the slice were written.
   procedure Advance (C : in out Copy; Copied : Unsigned_64)
   with Pre  => Valid (C) and then Copied <= C.Wanted - C.Done,
        Post => Valid (C) and then C.Done = C.Done'Old + Copied
                and then C.Wanted = C.Wanted'Old and then C.Source_At = C.Source_At'Old
                and then C.Target_At = C.Target_At'Old;

   --  The current slice's offsets (no overflow: both below 2 * Maximum_Offset).
   function Source_Position (C : Copy) return Unsigned_64 is (C.Source_At + C.Done);
   function Target_Position (C : Copy) return Unsigned_64 is (C.Target_At + C.Done);

   --  How a copy ends, decided after each slice: a failed slice ends it
   --  (with the prefix copied); else a cancel; else completion (all done
   --  wins over a deadline reached in the same pass); else the deadline.
   type Ending is (Going, Complete, Cancelled, Deadline_Reached, Failed);
   function Decide
     (Slice_Failed, Cancel_Asked, Finished, Deadline_Passed : Boolean) return Ending is
     (if Slice_Failed then Failed
      elsif Cancel_Asked then Cancelled
      elsif Finished then Complete
      elsif Deadline_Passed then Deadline_Reached
      else Going);

end Copy_Slices;
