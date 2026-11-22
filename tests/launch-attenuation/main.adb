------------------------------------------------------------------------------
--  Hosted tests of procmgr's launch attenuation (launch_attenuation.ads):
--  every set of held endpoints and every request, against an independent
--  statement of the rule, and the cases CAP-012 named. The rule procmgr had
--  before (one bit per role: holding the role at all covered any rights)
--  is stated too, to show the case it let through.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Launch_Attenuation; use Launch_Attenuation;
with Session_Launch; use Session_Launch;

procedure Main is
   Failures, Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   RO : constant Rights_Mask := Read_Right;
   WO : constant Rights_Mask := Write_Right;
   RW : constant Rights_Mask := Read_Right + Write_Right;

   --  Before CAP-012: any endpoint to the role covered the request.
   function Old_Held (Masks : Held_Masks) return Boolean is (Masks /= None_Held);

   Read_Only_Launcher : constant Held_Masks := With_Endpoint (None_Held, RO);
begin
   --  Exhaustive: every set of held masks (0 .. 15 over masks 0 .. 3) and
   --  every request.
   for Masks in Held_Masks range 0 .. 15 loop
      for Asked in Rights_Mask loop
         declare
            Expected : constant Boolean :=
              (for some M in Rights_Mask =>
                 (Masks and Shift_Left (Unsigned_8'(1), Natural (M))) /= 0
                 and then (Asked and not M) = 0);
         begin
            Check (Endpoint_Covers (Masks, Asked) = Expected, "covers" & Masks'Image & Asked'Image);
            Check (Service_Held (Masks, True, Asked), "a provider holds its role");
            for Optional in Boolean loop
               Check (Decide (Service_Held (Masks, False, Asked), Optional) =
                        (if Expected then Given elsif Optional then Withheld else Refused),
                      "verdict");
            end loop;
         end;
      end loop;
   end loop;

   --  CAP-012 finding P-03: a launcher holding a role read-only cannot
   --  launch a child with write; the old rule let it.
   Check (Decide (Service_Held (Read_Only_Launcher, False, RW), False) = Refused,
          "read-only launcher: a required read-write request is refused");
   Check (Decide (Service_Held (Read_Only_Launcher, False, WO), True) = Withheld,
          "read-only launcher: an optional write request is withheld");
   Check (Decide (Service_Held (Read_Only_Launcher, False, RO), False) = Given,
          "read-only launcher: a read-only request is given");
   Check (Old_Held (Read_Only_Launcher), "the old rule held the role for any rights");
   --  Two narrower endpoints do not make a wider one.
   Check (not Endpoint_Covers (With_Endpoint (Read_Only_Launcher, WO), RW),
          "read-only plus write-only endpoints do not cover read-write");
   Check (Endpoint_Covers (With_Endpoint (None_Held, RW), RO), "read-write covers read");
   Check (not Endpoint_Covers (None_Held, 0) , "nothing held covers nothing");

   --  Launch-into-S: the user session's envelope, every kind and rights.
   for K in Request_Kind loop
      for Asked in Rights_Mask loop
         declare
            Allowed : constant Boolean :=
              K in Service | Observation | Offerable_Self | Render | Outbound_Network;
         begin
            Check (Session_Launch.Within (User_Session, K, Asked) = Allowed,
                   "user session envelope " & K'Image & Asked'Image);
            Check (Decide (Session_Launch.Within (User_Session, K, Asked), True) =
                     (if Allowed then Given else Withheld),
                   "optional request against the envelope");
            Check (Decide (Session_Launch.Within (User_Session, K, Asked), False) =
                     (if Allowed then Given else Refused),
                   "required request against the envelope");
         end;
      end loop;
   end loop;
   --  An envelope narrowed to read-only services covers no write.
   Check (not Session_Launch.Within
            ((User_Session with delta Service_Rights => Read_Right), Service, RW),
          "a read-only envelope gives no write");
   Check (Badge_Of (Session_Launch_Tag) = Session_Badge
          and then Badge_Of (Remote_Launch_Tag) = Remote_Badge
          and then Badge_Of (0) = No_Badge and then Badge_Of (Remote_Launch_Tag + 1) = No_Badge,
          "badges by tag");
   Check (not Needs_Device_Confirmation (Remote_Badge), "no launch needs device confirmation yet");

   Put_Line ("launch-attenuation:" & Checks'Image & " checks," & Failures'Image & " failed");
   if Failures > 0 then
      raise Program_Error;
   end if;
end Main;
