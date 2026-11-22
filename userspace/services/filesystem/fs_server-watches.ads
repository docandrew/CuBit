------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Change notifications of the filesystem service: watches, event records, notice paths.
------------------------------------------------------------------------------
with CuBit.Filesystem_Events;
with Watch_Reserve;
private package Fs_Server.Watches is

   package FE renames CuBit.Filesystem_Events;
   package WR renames Watch_Reserve;

   --  Records the client has not read yet (its wake answers at once).
   function eventsWaiting (q : Client_Queue_Index) return Boolean;

   --  A change to path (relative to volume) of kind: every watch that sees
   --  it gets a record, if its owner's policy lets it read that path.
   procedure notify
     (volume : Volume_Index; path : String; kind : FE.Event_Kind; isDirectory : Boolean;
      object : Unsigned_64; cookie : Unsigned_64 := 0);

   --  Each pass: watches whose Rescan_Needed was read watch again, and owed
   --  records (Rescan_Needed, Watch_Ended) go in when there is room.
   procedure settleWatches;

   --  The cookie shared by a rename's two records (the next one).
   function nextRenameCookie return Unsigned_64;

   --  Modified events: a handle that may write keeps the path it was opened
   --  by (a bounded table: past it, the change is reported unnamed), and a
   --  written handle reports Modified once at its close, flush, park or the
   --  periodic commit. pending: it reports even if it writes nothing more
   --  (a truncation at open).
   procedure keepNoticePath (slot : Natural; rel : String; pending : Boolean);
   procedure noteModified (slot : Integer);
   procedure reportModified (slot : Natural);
   procedure forgetNoticePath (slot : Natural);

   --  A rename moved old to new: kept paths at or below old follow it.
   procedure renameNoticePaths (before, after : String);

   --  Queue_Watch: a directory handle of the client's; Options: subtree.
   procedure handleWatch (q : Client_Queue_Index; handleWord : Unsigned_64; options : Unsigned_32);

   procedure handleUnwatch (q : Client_Queue_Index; number : Unsigned_64);

   --  Queue q is released: its watches end with it.
   procedure forgetWatches (q : Client_Queue_Index);

end Fs_Server.Watches;
