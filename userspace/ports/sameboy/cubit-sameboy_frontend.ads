------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The CuBit frontend for the SameBoy Game Boy core (docs/c-removal.md,
--  userspace/ports/sameboy/README.md). The core is unmodified upstream C on
--  the CuBit libc.
--
--  A desktop window showing a lent buffer (CuBit.Desktop_Protocol), the
--  core's 48 kHz stereo output on one mixer stream (CuBit.Audio), and
--  cartridges read from sameboy/NN.gb. Bindings, scaling, pacing and the
--  audio batch are SPARK (CuBit.SameBoy_Keys, _Frames, _Batches).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;

package CuBit.SameBoy_Frontend is

   function Main return Interfaces.C.int
     with Export, Convention => C, External_Name => "main";

end CuBit.SameBoy_Frontend;
