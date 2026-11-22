-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Which user ranges the kernel may access through the current process's
-- own mapping (KERN-014 slice 2, Process.User_Memory).
--
-- @description
-- A range is admitted when it lies wholly in the user half and, unless the
-- caller says otherwise, misses the received-grant aperture. The hardware
-- then decides the rest (present, user, writable). Proved (gnatprove level
-- 2, tests/user-access-ranges): an admitted range neither wraps nor
-- reaches the kernel half, and no byte of it lies in the aperture.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package User_Access_Ranges with SPARK_Mode, Pure is
   -- The lower canonical half (User_Page_Walk.User_Limit).
   User_Limit : constant Unsigned_64 := 16#0000_8000_0000_0000#;

   function Within_User (Address, Bytes : Unsigned_64) return Boolean is
     (Address < User_Limit and then Bytes <= User_Limit - Address)
     with Post => (if Within_User'Result then
                     Address + Bytes <= User_Limit and then Address + Bytes >= Address);

   -- Within_User and disjoint from [Aperture_First, Aperture_End).
   function Admitted (Address, Bytes, Aperture_First, Aperture_End : Unsigned_64)
     return Boolean is
     (Within_User (Address, Bytes)
      and then (Bytes = 0 or else Address + Bytes <= Aperture_First
                or else Address >= Aperture_End))
     with Pre  => Aperture_First <= Aperture_End,
          Post => (if Admitted'Result then Address + Bytes <= User_Limit);

   -- No byte of an admitted range lies in the aperture, nor at or above
   -- User_Limit (one byte at a time, so hosted tests can run it).
   procedure Prove_Outside_Aperture
     (Address, Bytes, Aperture_First, Aperture_End, Byte : Unsigned_64) with
     Ghost,
     Pre  => Aperture_First <= Aperture_End
             and then Admitted (Address, Bytes, Aperture_First, Aperture_End)
             and then Byte >= Address and then Byte - Address < Bytes,
     Post => Byte < User_Limit
             and then (Byte < Aperture_First or else Byte >= Aperture_End);
end User_Access_Ranges;
