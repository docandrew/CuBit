--  Whether a USB stick that left the drawer was ejected by Files or pulled
--  out (FS-034). Files re-lists its places every 2 s, and a listing taken
--  before an Eject finished can still name the stick after the Eject's
--  answer arrives. So the stick Files ejected is "news again" only once a
--  listing has shown it gone and a later one shows it back.
package Files_Ejections with SPARK_Mode is
   type Phase is
     (Idle,        --  no Eject of this stick pending: a removal is news
      In_Flight,   --  Queue_Eject sent, no answer yet
      Ejected,     --  answered Safe_To_Remove; listings may still name it
      Gone);       --  a listing has shown it gone since the Eject

   --  Queue_Eject sent for the stick.
   function Started return Phase is (In_Flight);

   --  The Eject's answer: Safe_To_Remove, or a refusal (the stick stays).
   function Answered (Current : Phase; Safe : Boolean) return Phase is
     (if Current = In_Flight and then Safe then Ejected
      elsif Current = In_Flight then Idle
      else Current);

   --  A listing of the places: whether it names the stick.
   function Listed (Current : Phase; Present : Boolean) return Phase is
     (if Present then (if Current = Gone then Idle else Current)
      elsif Current in In_Flight | Ejected then Gone
      else Current);

   --  The stick was in the last listing and is not in this one: say that
   --  it was pulled out without Eject only when no Eject of it is pending.
   function Warn_Removed (Current : Phase) return Boolean is (Current = Idle);

   ROOT_END : constant Character := '/';

   --  A locator without the root's closing '/' ("@usb:0/" is "@usb:0").
   function Bare (Locator : String) return String is
     (if Locator'Length > 0 and then Locator (Locator'Last) = ROOT_END
      then Locator (Locator'First .. Locator'Last - 1) else Locator);

   --  A and B name the same volume root, written with or without the
   --  closing '/' (the service's places list and a pane's path differ).
   function Same_Root (A, B : String) return Boolean is (Bare (A) = Bare (B));
end Files_Ejections;
