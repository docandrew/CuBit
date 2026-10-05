------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Stream_Rings with SPARK_Mode is

   procedure Place
     (Producer : Unsigned_32; Size : Capacity; Length : Unsigned_32;
      Offset : out Unsigned_32; Sentinel : out Boolean; Sentinel_Offset : out Unsigned_32;
      Start : out Unsigned_32)
   is
      Ring : constant Unsigned_32 := Unsigned_32 (Size);
      --  Below Ring by mod's definition; the clamp states it for the
      --  prover (it never changes the value).
      Remainder : constant Unsigned_32 :=
        (if Producer mod Ring < Ring then Producer mod Ring else 0);
   begin
      Sentinel_Offset := 0;
      if Remainder + Length > Ring then
         --  The entry would straddle the end: a sentinel skips to offset 0.
         Sentinel := True;
         Sentinel_Offset := (if Remainder + 2 <= Ring then Remainder else 0);
         Start := Producer + (Ring - Remainder);
         Offset := 0;
      else
         Sentinel := False;
         Start := Producer;
         Offset := Remainder;
      end if;
   end Place;

end CuBit.Libc_Stream_Rings;
