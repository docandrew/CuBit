with Interfaces; use Interfaces;
with Files_Listing;
with Files_Queue;

--  Reading the start of one file for the quick viewer (docs/files-app.md,
--  "Viewer"): open, positioned reads of one chunk at a time, close, all
--  through the request queue and never waiting. Every read ends in a state
--  the person sees: done, failed (with the service's status), timed out
--  (an explicit deadline, given by the caller) or cancelled. Answers that
--  arrive after the end are taken and dropped; a handle opened late is
--  closed.
package Files_Reader is
   --  The most bytes the viewer holds.
   VIEW_LIMIT : constant := 1_048_576;
   subtype View_Length is Natural range 0 .. VIEW_LIMIT;

   type Read_State is (Idle, Opening, Reading, Done, Failed, Timed_Out, Cancelled);
   subtype Finished is Read_State range Done .. Cancelled;

   --  Arena_Base: where this reader's arena area starts (a path page, then
   --  one chunk): AREA_BYTES long.
   CHUNK_BYTES : constant := 65_536;
   AREA_BYTES : constant := 4_096 + CHUNK_BYTES;

   procedure Start (Path : String; Arena_Base : Unsigned_64; Now_Us, Deadline_Us : Unsigned_64)
     with Pre => Path'Length in 1 .. 4_096 and then Deadline_Us > Now_Us;
   procedure Cancel;
   --  Whether Answer was for the reader (it is then consumed).
   procedure Take (Answer : Files_Queue.Answer; Owned : out Boolean);
   --  The deadline, and requests that wait for queue room.
   procedure Pump (Now_Us : Unsigned_64);

   function State return Read_State;
   function Status return Unsigned_32;   --  the service's reply for Failed
   function Length return View_Length;
   function Byte (Position : Positive) return Unsigned_8 with Pre => Position <= Length;
   --  The bytes read, as one copy (for decoding lines).
   function Bytes return Files_Listing.Name_Bytes;
   --  The file went on past VIEW_LIMIT.
   function Truncated return Boolean;
   --  Advances on every change of state or data.
   function Revision return Unsigned_64;
   --  When an unfinished read times out; Unsigned_64'Last when none runs.
   function Deadline return Unsigned_64;
end Files_Reader;
