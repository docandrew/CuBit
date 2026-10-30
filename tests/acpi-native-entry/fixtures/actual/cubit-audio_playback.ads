with Interfaces; use Interfaces;
package CuBit.Audio_Playback with SPARK_Mode, Pure is
   --  Reply bounds for owner-scoped 48 kHz stereo playback accounting.
   function Valid
     (Ring_Frames, Device_Frames, Device_Capacity, Ring_Capacity, Reserved : Unsigned_64)
      return Boolean is
     (Ring_Frames <= Ring_Capacity and then
      Device_Capacity in 512 .. 8192 and then Device_Capacity mod 256 = 0 and then
      Device_Frames <= Device_Capacity and then Reserved = 0);
end CuBit.Audio_Playback;
