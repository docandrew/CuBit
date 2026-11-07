------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Apps built on this machine and swapped in for the running copy
--  (SELF-002 phase B, cubit-remote app replace | rollback | status): a
--  product of a workspace build becomes the app's next generation and
--  replaces the instance this endpoint started, with the one before kept
--  for a rollback.
--
--  @description
--  Generations live under the work root, outside every host place, so the
--  host cannot change one after it was taken:
--
--    <work>/generations/<app>/<n>.app   generation n, a copy of the product
--    <work>/generations/<app>/state     "latest n", "current n <sha256>",
--                                       "previous n <sha256>" (0: none)
--
--  The work root must be on the system volume ("@system/work"): the
--  manifest's launch table admits programs beneath
--  "@system/work/generations/" only, and procmgr admits them only while
--  the Desktop's "Allow app replacement" switch is on (Switch_Off
--  otherwise, before anything is stopped). A staged app holds at most this
--  endpoint's authority (procmgr attenuates it as for any launch); nothing
--  checks who built it (signed admission is not built).
--
--  A swap, as an explicit sequence: copy the product (SHA-256 of the bytes
--  copied), ask procmgr whether it would admit it (OP_DESCRIBE), stop the
--  running instance (Stop, then End_Child after Stop_Deadline_Ms; Stopping
--  below), start the new generation, record the state. When the new one
--  does not start, the one stopped for it is started again (Start_Failed).
--  A rollback checks the previous generation's SHA-256 against the state
--  file before it starts it.
--
--  Each operation runs to its end within the request: at most
--  Stop_Deadline_Ms + End_Deadline_Ms waiting, well inside the host's
--  exchange deadline. The instances this endpoint started are known only
--  while it runs (a restarted ccl-remote reads the generations back from
--  the state files, but not the instances: LIVE-001 hands those over).
------------------------------------------------------------------------------
pragma Ada_2022;
with Remote_Wire;

package Remote_Apps is
   package W renames Remote_Wire;

   --  How long a running instance has to obey Stop, and then to end once
   --  procmgr was asked to end it (milliseconds).
   Stop_Deadline_Ms : constant := 5_000;
   End_Deadline_Ms  : constant := 5_000;
   --  Named ceiling: apps this endpoint keeps track of (Capacity beyond).
   Maximum_Apps : constant := 8;

   procedure Initialize (Work : String);

   type Report is record
      Result   : W.App_Result := W.Reported;
      Current  : W.Generation := W.No_Generation;
      Previous : W.Generation := W.No_Generation;
      Instance : W.Instance_State := W.Not_Started;
      Stop     : W.Stop_Kind := W.None_Running;
      Current_Hash, Previous_Hash : W.Digest := [others => 0];
   end record;

   --  Status: OK (Item says what happened), or why nothing could be done
   --  (Unavailable: the work root is not on the system volume; Capacity;
   --  Not_Found: no such product, or an app with no generations; a
   --  filesystem failure; Integrity_Mismatch: a generation's file is not
   --  what the state recorded).
   procedure Replace (Product : String; Item : out Report; Status : out W.Status)
     with Pre => Product'Length <= W.Maximum_Name_Bytes and then Product'Last < Positive'Last;
   procedure Rollback (App : String; Item : out Report; Status : out W.Status)
     with Pre => App'Length <= W.Maximum_App_Name_Bytes;
   procedure Query (App : String; Item : out Report; Status : out W.Status)
     with Pre => App'Length <= W.Maximum_App_Name_Bytes;

   --  The response body (W.App_Response_Bytes) for Item.
   procedure Encode (Item : Report; Data : in out W.Byte_Array)
     with Pre => Data'First = 1 and then Data'Length >= W.App_Response_Bytes;

   --  What the person reads in the activity line: "swap-probe generation
   --  2 running (generation 1 stopped)".
   function Image (App : String; Item : Report) return String;
end Remote_Apps;
