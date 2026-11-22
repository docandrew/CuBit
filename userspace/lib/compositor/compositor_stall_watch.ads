with Interfaces;
-- Renderer stall judgement by elapsed time and progress, never by counting
-- frames. Progress is a published frame or an accepted upload transfer: a
-- renderer that is copying cold sources one at a time is progressing, not
-- stalled. A stall needs failing captures to continue for an explicit
-- deadline, measured from the first failure after progress, with no
-- progress and no quiet gap of a whole deadline between failures (idle time
-- is not evidence of failure).
package Compositor_Stall_Watch with SPARK_Mode, Pure is
   subtype Millis is Interfaces.Unsigned_64;
   subtype Count is Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_64;
   type State is record
      Armed : Boolean := False; -- a failing window is open
      Since : Millis := 0;      -- first failure of the window
      Last : Millis := 0;       -- latest failure
      Uploads : Count := 0;     -- upload transfer count when it opened
   end record;
   -- The window must (re)open: none open, upload progress, a clock that went
   -- backwards, or no failure for a whole deadline.
   function Reopens (S : State; Now : Millis; Uploads : Count; Deadline : Millis) return Boolean is
     (not S.Armed or else Uploads /= S.Uploads or else Now < S.Last or else
      Now - S.Last >= Deadline);
   -- A frame was published: progress; closes any failing window.
   procedure Completed (S : in out State)
     with Post => not S.Armed;
   -- A capture ended without a frame.
   procedure Retried (S : in out State; Now : Millis; Uploads : Count;
      Deadline : Millis; Stalled : out Boolean)
     with Post =>
       (if Reopens (S'Old, Now, Uploads, Deadline) then
          not Stalled and S = (True, Now, Now, Uploads)
        else Stalled = (Now >= S'Old.Since and then Now - S'Old.Since >= Deadline) and
          S = (True, S'Old.Since, Now, S'Old.Uploads)) and
       (if Stalled then Uploads = S'Old.Uploads and S'Old.Armed and
          Now >= S'Old.Since and Now - S'Old.Since >= Deadline);
end Compositor_Stall_Watch;
