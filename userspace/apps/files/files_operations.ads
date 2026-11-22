with Interfaces; use Interfaces;
with Files_Queue;

--  Copy, move, delete, make-folder and rename (docs/files-app.md,
--  "Operations"), run through the filesystem's request queue one step at a
--  time, never waiting: a scan of the source folders builds the plan
--  (Files_Plan, proved), then each item is copied by the service
--  (Queue_Copy, its progress read from the service's region), made,
--  unlinked or removed. Progress is counted in
--  items and bytes. Every operation ends visibly: succeeded, failed (with
--  the service's status and the item), cancelled or timed out (each step
--  has an explicit deadline). Cancel stops issuing steps, waits only for
--  the one in flight, and removes a partly copied file. Name conflicts
--  follow a policy: skip, overwrite, keep both ("name (2)") or ask, once or
--  for all.
--
--  Move tries a rename first (Queue_Rename) and copies then deletes only
--  what the service answers is across volumes.
package Files_Operations is
   type Operation_Kind is (Copy_Operation, Move_Operation, Delete_Operation, Make_Folder_Operation,
                           Rename_Operation);
   type Conflict_Policy is (Ask, Skip_Existing, Overwrite_Existing, Keep_Both);
   type Phase_Kind is (Idle, Scanning, Running, Asking, Cancelling, Finished);
   type Outcome is (No_Outcome, Succeeded, Failed, Cancelled, Timed_Out);

   --  The arena area: two path pages (the second for a queued rename) and
   --  a scan slot of directory pages. File data never passes through it:
   --  the service copies (Queue_Copy).
   PATH_AREA : constant := 4_096;
   --  Directory.Page.V2 pages per folder-scan read.
   SCAN_READ_PAGES : constant := 8;
   SCAN_PAGE_BYTES : constant := 4_096;
   AREA_BYTES : constant := 2 * PATH_AREA + SCAN_READ_PAGES * SCAN_PAGE_BYTES;
   --  How long one step may take before the operation is reported as
   --  timed out (explicit; slow media gets its own value later). A copy
   --  times out only when its progress stops for that long.
   STEP_DEADLINE_US : constant := 10_000_000;
   --  A copy the service has no room for yet (REPLY_BUSY: four run) is
   --  asked again after this.
   BUSY_RETRY_US : constant := 10_000;
   --  While the service copies, its progress is read this often (the bar).
   PROGRESS_REFRESH_US : constant := 100_000;
   --  The most items one operation plans.
   MAXIMUM_ITEMS : constant := 65_536;

   --  Copy and move: sources are names in Source_Dir; Target_Dir gets them.
   --  Delete: Target_Dir is unused.
   procedure Prepare (Kind : Operation_Kind; Source_Dir, Target_Dir : String; Policy : Conflict_Policy)
     with Pre => Kind in Copy_Operation | Move_Operation | Delete_Operation
                 and then Source_Dir'Length in 1 .. 4_096 and then Target_Dir'Length <= 4_096;
   procedure Add_Source (Name : String; Folder : Boolean; Size : Unsigned_64)
     with Pre => Name'Length in 1 .. 255;
   procedure Start (Arena_Base : Unsigned_64; Now_Us : Unsigned_64);
   procedure Make_Folder (Directory, Name : String; Arena_Base, Now_Us : Unsigned_64)
     with Pre => Directory'Length in 1 .. 4_096 and then Name'Length in 1 .. 255;
   procedure Rename (Directory, Old_Name, New_Name : String; Arena_Base, Now_Us : Unsigned_64)
     with Pre => Directory'Length in 1 .. 4_096 and then Old_Name'Length in 1 .. 255
                 and then New_Name'Length in 1 .. 255;

   procedure Take (Answer : Files_Queue.Answer; Owned : out Boolean);
   procedure Pump (Now_Us : Unsigned_64);
   procedure Cancel;
   --  The answer to Asking: how to treat this conflict (and, For_All, the
   --  rest of the operation's).
   procedure Decide (Policy : Conflict_Policy; For_All : Boolean)
     with Pre => Policy /= Ask;
   --  Forget a finished operation (its result was shown).
   procedure Acknowledge;

   function Kind return Operation_Kind;
   function Phase return Phase_Kind;
   function Result return Outcome;
   function Items_Done return Natural;
   function Items_Total return Natural;
   function Bytes_Done return Unsigned_64;
   function Bytes_Total return Unsigned_64;
   --  The item in progress (or that failed, or conflicts), relative to the
   --  source folder.
   function Current return String;
   function Failure return Unsigned_32;
   function Skipped return Natural;
   function Revision return Unsigned_64;
   --  When the step in flight times out; Unsigned_64'Last for none.
   function Deadline return Unsigned_64;
   function Busy return Boolean is (Phase in Scanning | Running | Asking | Cancelling);
end Files_Operations;
