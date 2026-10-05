------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  DOOM's sound effect mixing: unsigned 8-bit PCM at the lump's rate,
--  resampled (nearest sample) to 48 kHz stereo signed 16-bit, panned by
--  DOOM's volume and separation (docs/c-removal.md).
--
--  All arithmetic is proved free of overflow; the position is 16.16 fixed
--  point in 64 bits, so long sounds no longer wrap back to their start.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Doom_Mixer with SPARK_Mode, Pure is

   Output_Rate : constant := 48_000;
   --  One batch: a DOOM tic (1/35 s) is about 1371 frames at 48 kHz.
   Mix_Frames  : constant := 1536;
   Channel_Count : constant := 8;

   type Channel_Index is range 0 .. Channel_Count - 1;

   --  DOOM's volume (0 .. 127) and separation (0 left .. 128 centre .. 254
   --  right); out-of-range values from the game are clamped.
   Maximum_Volume     : constant := 127;
   Maximum_Separation : constant := 254;
   subtype Volume is Natural range 0 .. Maximum_Volume;

   subtype Sample_Count is Natural;
   subtype Sample_Rate is Positive range 1 .. 2 ** 16 - 1;

   Fraction_Bits : constant := 16;
   One           : constant := 2 ** Fraction_Bits;
   --  The position never passes one step beyond the last sample.
   type Position is range 0 .. 2 ** 48;
   subtype Step is Position range 0 .. 2 ** 17;

   type Channel is record
      Active : Boolean := False;
      Length : Sample_Count := 0;
      At_Sample : Position := 0;
      Advance : Step := 0;
      Left, Right : Volume := 0;
   end record;

   Silent : constant Channel :=
     (Active => False, Length => 0, At_Sample => 0, Advance => 0,
      Left => 0, Right => 0);

   procedure Pan (Vol, Sep : Integer; Left, Right : out Volume);

   function Started (Length : Sample_Count; Rate : Sample_Rate;
                     Vol, Sep : Integer) return Channel
     with Post => Started'Result.Active and then
                  Started'Result.Length = Length and then
                  Started'Result.At_Sample = 0;

   type Byte_Array is array (Natural range <>) of Unsigned_8;

   --  Running sums, saturated far beyond what 16-bit output can hold.
   subtype Mix_Value is Integer_32 range -2 ** 24 .. 2 ** 24;
   type Mix_Buffer is array (0 .. 2 * Mix_Frames - 1) of Mix_Value;

   --  Add one batch of the channel into Into (left, right interleaved).
   --  The channel goes inactive when it runs out of samples.
   procedure Mix (Samples : Byte_Array; Item : in out Channel;
                  Into : in out Mix_Buffer)
     with Pre => Samples'First = 0 and then
                 Samples'Last = Item.Length - 1;

   type Output_Buffer is array (0 .. 2 * Mix_Frames - 1) of Integer_16
     with Convention => C;

   function Clamped (Sum : Mix_Value) return Integer_16;

end CuBit.Doom_Mixer;
