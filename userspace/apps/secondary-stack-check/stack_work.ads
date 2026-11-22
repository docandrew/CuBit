------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The work secondary-stack-check runs in its main thread and in its extra
--  threads at once: deep primary-stack use, large unconstrained results on
--  the secondary stack, growth across many chunks and back, and a request
--  past the ceiling refused and handled; every byte checked.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;

package Stack_Work is

   KiB : constant := 1024;
   MiB : constant := 1024 * KiB;

   --  Primary stack each thread uses: past the 16 MiB at which the old
   --  fixed secondary stack sat below the main stack's top.
   Primary_Depth_Bytes : constant := 20 * MiB;
   Frame_Bytes : constant := 256 * KiB;
   Frames : constant := Primary_Depth_Bytes / Frame_Bytes;

   --  One unconstrained result; two of them concatenated make a third
   --  twice as large, all on the secondary stack at once.
   Result_Bytes : constant := 3 * MiB;

   --  Growth across chunks: each item larger than half a 64 KiB chunk, so
   --  every item takes a chunk of its own.
   Growth_Items : constant := 160;
   Growth_Item_Bytes : constant := 48 * KiB;

   --  What one run found (Full_Run's result).
   type Outcome is record
      --  Run: the results intact, both stacks.
      Passed : Boolean := False;
      --  Where its secondary-stack results were, and its stack object.
      Result_Address : Unsigned_64 := 0;
      Stack : Unsigned_64 := 0;
      --  Grow_Across_Chunks: every item intact, the chunks it took, and a
      --  second growth after the release reused them (the same addresses,
      --  no more chunks), the mark restored and the high-water mark seen.
      Grown : Boolean := False;
      Chunks : Natural := 0;
      Reused : Boolean := False;
      --  Refuse_Past_Ceiling: Storage_Error with the CAPACITY message,
      --  handled; the stack unchanged and usable after it.
      Refused : Boolean := False;
      Continued : Boolean := False;
   end record;

   --  Use Frames frames of Frame_Bytes of the primary stack, each filled
   --  with a pattern from Seed; at the deepest, build results on the
   --  secondary stack and check them; on the way back, check every frame.
   --  Then Refuse_Past_Ceiling and Grow_Across_Chunks.
   function Full_Run (Seed : Unsigned_64) return Outcome;

   --  The deep run alone.
   function Run (Seed : Unsigned_64) return Outcome;

   --  The calling thread's stack: how many chunks it holds now
   --  (System.Secondary_Stack.SS_Info's count).
   function Chunk_Count return Natural;

   --  Whether Where lies in [First, First + Bytes).
   function Inside (Where, First, Bytes : Unsigned_64) return Boolean is
     (Where >= First and then Where - First < Bytes);

   function Value_Of (Where : System.Address) return Unsigned_64;

   --  Whether Message is the runtime's CAPACITY message for Reason (its
   --  image, "CEILING_REACHED" or "HEAP_REFUSED").
   function Is_Capacity (Message, Reason : String) return Boolean;

   --  A result of Length bytes, built on the secondary stack (visible so
   --  the main program's last step can ask for one past the ceiling).
   type Byte_Array is array (Positive range <>) of Unsigned_8;
   function Pattern (Seed : Unsigned_64; Length : Natural) return Byte_Array
   with No_Inline;

end Stack_Work;
