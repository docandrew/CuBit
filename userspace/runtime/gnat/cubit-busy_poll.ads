with Interfaces; use Interfaces;

--  Polling while traffic flows (NAPI/busy-poll style): a side that just
--  did work keeps checking its rings for a short window, measured with
--  the TSC, before it arms a doorbell and sleeps. Calibrate once at
--  startup: immediate with the clock publication's TSC rate, otherwise
--  about 10 ms against the millisecond clock.
package CuBit.Busy_Poll is

   --  Linux's default busy-poll window.
   Default_Window_Microseconds : constant := 50;

   procedure Calibrate;

   function Now return Unsigned_64 with Inline;

   --  Less than Micros microseconds have passed since Since (a Now value).
   function Within (Since, Micros : Unsigned_64) return Boolean
     with Inline;

   --  A spin-loop hint to the CPU (PAUSE).
   procedure Relax with Inline;

end CuBit.Busy_Poll;
