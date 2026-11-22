with Interfaces; use Interfaces;
with CuBit.Filesystem_Sessions;
with CuBit.Handles;
with CuBit.Messages;
with Text_Hosts;
with Text_Limits; use Text_Limits;

--  On CuBit: the editor's host (docs/text-editor.md, "Files, saving and
--  recovery"). Files load and save through the filesystem's request queue
--  (CuBit.Filesystem_Sessions on the endpoint the manifest grants), never
--  blocking: each poll reaps what has arrived and submits what follows.
--  Names are the scoped paths the manifest's filesystem-scope grants; the
--  service checks every open. A save writes a sibling (.name.text-save),
--  flushes and closes it, then renames it over the original (Queue_Rename,
--  POSIX replacement): a crash leaves the old file or the new one. The
--  clipboard is the desktop's clipboard service (CuBit.UI.Clipboard), on
--  the window's surface; a paste runs inside the key's handling, which
--  carries its activation grant.
package Text_Host_Native is
   type Host is limited private;

   --  Open the queue (False: no filesystem; files then fail to open).
   procedure Open (H : in out Host; Endpoint : CuBit.Handles.Endpoint; Ready : out Boolean);
   procedure Close (H : in out Host);
   --  The window's surface, for the clipboard.
   procedure Set_Surface (H : in out Host; Surface : Unsigned_64);
   --  A file name offered for Open (the granted ones the app knows).
   procedure Offer (H : in out Host; Name : String);

   --  The event loop's part: with requests out, arm the wake so the loop
   --  sleeps until answers wait; hand completion entries to Complete.
   procedure Arm_Wake (H : in out Host);
   procedure Complete (H : in out Host; Receipt : CuBit.Messages.CompletionEntry; Consumed : out Boolean);
   function Waiting (H : Host) return Boolean;
   --  The last reply label of a failed request (for the serial marker).
   function Last_Reply (H : Host) return Unsigned_32;
   --  The last load's timeline (microseconds on the monotonic clock):
   --  open submitted, its answer reaped, first read submitted, its answer
   --  reaped (0 where it did not happen). For the serial marker.
   type Load_Times is record
      Open_Sent, Open_Reaped, Read_Sent, Read_Reaped : Unsigned_64 := 0;
   end record;
   function Last_Load (H : Host) return Load_Times;

   function Chunk_Limit (H : Host) return Positive;
   procedure Start_Load (H : in out Host; Name : String; Size : out Byte_Count;
                                    Status : out Text_Hosts.File_Status);
   procedure Poll_Load (H : in out Host; Into : out String; Got : out Natural;
                                   Status : out Text_Hosts.File_Status);
   procedure Start_Save (H : in out Host; Name : String; Status : out Text_Hosts.File_Status);
   procedure Save_Chunk (H : in out Host; Bytes : String; Status : out Text_Hosts.File_Status);
   procedure Finish_Save (H : in out Host; Status : out Text_Hosts.File_Status);
   procedure Poll_Save (H : in out Host; Status : out Text_Hosts.File_Status);
   procedure Copy (H : in out Host; Text : String);
   procedure Request_Paste (H : in out Host);
   procedure Poll_Paste
     (H : in out Host; Into : out String; Length : out Natural; Ready, Truncated : out Boolean);
   function Granted_Count (H : Host) return Natural;
   function Granted_Name (H : Host; Index : Positive) return String;
   function Display_Name (H : Host; Name : String) return String;
private
   package FS renames CuBit.Filesystem_Sessions;
   MAXIMUM_NAME : constant := 4_096;
   MAXIMUM_OFFERED : constant := 16;
   PASTE_BYTES : constant := 1_048_576;

   type Load_Phase is (No_Load, Load_Opening, Load_Reading, Load_Closing);
   type Save_Phase is (No_Save, Save_Opening, Save_Ready, Save_Writing, Save_Flushing, Save_Closing, Save_Renaming,
                       Save_Done, Save_Failed);
   type Name_Text is record
      Text : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Length : Natural := 0;
   end record;
   type Name_Array is array (1 .. MAXIMUM_OFFERED) of Name_Text;
   type Paste_Buffer is access String;
   --  An answer reaped for the load or the save, kept until its poll.
   type Held_Answer is record
      Held : Boolean := False;
      Status : Unsigned_32 := 0;
      Value : Unsigned_64 := 0;
   end record;

   type Host is limited record
      Session : FS.Session;
      Surface : Unsigned_64 := 0;
      Wake_Token : Unsigned_64 := 0;
      Reply : Unsigned_32 := 0;
      --  Loading.
      Load : Load_Phase := No_Load;
      Load_Tag : FS.Token := 0;
      Load_Handle : Unsigned_64 := 0;
      Load_Position : Unsigned_64 := 0;
      Load_Asked : Unsigned_64 := 0;
      Load_Answer : Held_Answer;
      Times : Load_Times;
      --  Saving.
      Save : Save_Phase := No_Save;
      Save_Tag : FS.Token := 0;
      Save_Handle : Unsigned_64 := 0;
      Save_Position : Unsigned_64 := 0;
      Target, Temporary : Name_Text;
      Save_Answer : Held_Answer;
      Finish_Requested : Boolean := False;
      --  The clipboard.
      Paste : Paste_Buffer;
      Paste_Length : Natural := 0;
      Paste_Ready, Paste_Truncated : Boolean := False;
      Offered : Name_Array;
      Offered_Count : Natural := 0;
   end record;
end Text_Host_Native;
