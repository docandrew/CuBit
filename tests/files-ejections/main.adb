--  Files_Ejections (FS-034): when Files says a USB stick was pulled out
--  without Eject. Image-based run of 2026-11-23: after a clean Eject, a
--  places list names the stick's root "@usb:0" while Files recorded the
--  ejected stick as "@usb:0/", so they never matched and every Eject read
--  as a surprise removal (Same_Root). A listing taken before the Eject
--  finished may also still name the stick (the phases).
with Ada.Text_IO;
with Files_Ejections; use Files_Ejections;

procedure Main is
   Checks, Failures : Natural := 0;

   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   P : Phase;
begin
   --  The run that found it: Eject answered, then a stale listing still
   --  naming the stick, then one without it. No warning.
   P := Started;
   P := Answered (P, Safe => True);
   P := Listed (P, Present => True);    --  stale: taken before the detach
   Check (P = Ejected, "a stale listing after Safe_To_Remove keeps the Eject");
   Check (not Warn_Removed (P), "no warning when the stale-listed stick then leaves");
   P := Listed (P, Present => False);
   Check (P = Gone, "the ejected stick is seen gone");

   --  A listing during the Eject itself.
   P := Started;
   P := Listed (P, Present => True);
   Check (P = In_Flight, "a listing during the Eject keeps it in flight");
   Check (not Warn_Removed (P), "a stick leaving mid-Eject is not a surprise");
   P := Answered (P, Safe => True);
   P := Listed (P, Present => False);
   Check (P = Gone, "gone after the answer");

   --  Plugged in again after it was seen gone: a later pull is news.
   P := Listed (P, Present => True);
   Check (P = Idle, "back after being seen gone is a new stick");
   Check (Warn_Removed (P), "pulling it now warns");

   --  A refused Eject (open files): the stick stays, a pull warns.
   P := Answered (Started, Safe => False);
   Check (P = Idle, "a refused Eject leaves nothing pending");
   Check (Warn_Removed (P), "a pull after a refused Eject warns");

   --  The answer never changes a phase it does not belong to.
   Check (Answered (Idle, True) = Idle, "a stray answer while idle");
   Check (Answered (Gone, True) = Gone, "a stray answer after gone");
   Check (Listed (Idle, False) = Idle and then Listed (Idle, True) = Idle, "listings while idle");
   Check (Listed (Gone, False) = Gone, "still gone");

   --  The places list writes a stick's root "@usb:0" and Files recorded
   --  the ejected one as "@usb:0/": the real cause in the QEMU run. Before
   --  Same_Root, the two never matched and every Eject warned.
   Check (Same_Root ("@usb:0", "@usb:0/"), "a root with and without its '/'");
   Check (Same_Root ("@usb:0/", "@usb:0"), "the other way round");
   Check (Same_Root ("@usb:0", "@usb:0"), "the same spelling");
   Check (not Same_Root ("@usb:0", "@usb:1"), "another stick");
   Check (not Same_Root ("@usb:1", "@usb:10/"), "a longer number");
   Check (not Same_Root ("@usb:0", ""), "nothing ejecting");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("files-ejections: PASS" & Natural'Image (Checks) & " checks");
   else
      Ada.Text_IO.Put_Line ("files-ejections: FAIL" & Natural'Image (Failures) & " of" & Natural'Image (Checks));
      raise Program_Error;
   end if;
end Main;
