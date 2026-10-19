------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Why an app launched from the Apps menu did not start, for the person who
--  launched it: the state behind Desktop's launch notice.
--
--  @description
--  Two sources, neither a new call:
--  - procmgr's OP_SPAWN error reply names a typed reason
--    (CuBit.Launch_Arguments.Launch_Failure): Show_Refusal.
--  - A child procmgr started can still end before it is useful. The kernel
--    tells its parent (Desktop) asynchronously (CuBit.Child_Exits); Desktop
--    watches each child it launched until it opens a window (Watch,
--    Window_Opened) and, if it ends first by being stopped or with a
--    nonzero code, shows that (Ended). Ending with code 0 is a normal end
--    for a program without a window, not a failure.
--  Desktop posts each notice into the Deskbar's notification centre
--  (Desktop_Notifications): toasted briefly, then kept in the history until
--  the person dismisses it, so it cannot go unseen.
--  The watch is bounded: past Watch_Capacity children still without a
--  window, the oldest watch is replaced (that child's early end then goes
--  unreported here; procmgr still logs it).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with CuBit.Child_Exits;
with CuBit.Launch_Arguments;
with CuBit.Process_IDs;

package Desktop_Launch_Notice with SPARK_Mode is
   use type CuBit.Process_IDs.Process_ID;
   use type CuBit.Child_Exits.Termination_Kind;
   use type Interfaces.Unsigned_64;

   --  As Desktop_Launch.Maximum_Label: the menu label names the app.
   Maximum_Name : constant := 32;
   subtype Name_Length is Natural range 0 .. Maximum_Name;
   subtype Name_Text is String (1 .. Maximum_Name);

   type Cause is (Refused, Ended_Early);

   type Notice is record
      Name    : Name_Text := [others => ' '];
      Length  : Name_Length := 0;
      Why     : Cause := Refused;
      Failure : CuBit.Launch_Arguments.Launch_Failure :=
        CuBit.Launch_Arguments.Spawn_Failed;
      Ending  : CuBit.Child_Exits.Report := (others => <>);
   end record;

   --  procmgr refused to start Name.
   procedure Show_Refusal
     (N : out Notice; Name : String;
      Failure : CuBit.Launch_Arguments.Launch_Failure)
     with Post => N.Why = Refused;

   --  The app's name (its menu label).
   function Name_Of (N : Notice) return String is (N.Name (1 .. N.Length));

   --  "Penny could not start" / "Penny stopped while starting".
   function Title (N : Notice) return String;

   --  The reason in plain words.
   function Detail (N : Notice) return String;

   Watch_Capacity : constant := 8;
   type Watch_Count is range 0 .. Watch_Capacity;
   subtype Watch_Index is Watch_Count range 1 .. Watch_Capacity;

   type Watched is record
      Used    : Boolean := False;
      Process : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
      Name    : Name_Text := [others => ' '];
      Length  : Name_Length := 0;
   end record;
   type Watch_Slots is array (Watch_Index) of Watched;

   type Watch_Table is record
      Slots : Watch_Slots;
      Next  : Watch_Index := 1;   --  replaced first when every slot is used
   end record;

   --  Process, just launched as Name, has not opened a window yet.
   procedure Watch
     (T : in out Watch_Table; Process : CuBit.Process_IDs.Process_ID;
      Name : String)
     with Pre => Process /= CuBit.Process_IDs.No_Process;

   --  Process opened a window: it started.
   procedure Window_Opened
     (T : in out Watch_Table; Process : CuBit.Process_IDs.Process_ID);

   --  The kernel reported Ending. Shown is True when it was a watched child
   --  that failed to start; N then describes it. Its watch ends either way.
   procedure Ended
     (T : in out Watch_Table; N : in out Notice;
      Ending : CuBit.Child_Exits.Report; Shown : out Boolean)
     with Post => (if Shown then N.Why = Ended_Early);

   --  Whether Ending is a failure to start (not a clean exit with code 0).
   function Failed (Ending : CuBit.Child_Exits.Report) return Boolean is
     (Ending.Kind = CuBit.Child_Exits.Stopped or else Ending.Code /= 0);

end Desktop_Launch_Notice;
