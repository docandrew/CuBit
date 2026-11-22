with Text_Limits; use Text_Limits;

--  What the editor needs from whoever embeds it (docs/text-editor.md,
--  "Embedding"): the Text app, a RAD tool's code pane, the hosted test
--  harness. A host extends Host; nothing else in the core reaches outside.
--
--  Every operation is asynchronous in shape: a start, then polls that hand
--  over whatever has arrived, so a slow store never blocks a keypress. A
--  name is whatever the host's grants mean by it (on CuBit a scoped path
--  under the manifest's filesystem-scope, or a file handed over by the
--  chooser); the core never builds or interprets paths.
package Text_Hosts is
   type File_Status is
     (Done,            --  complete
      Pending,         --  more to come: poll again
      Busy,            --  nothing taken now (the transfer area is in use): offer it again later
      Not_Found,
      Not_Granted,     --  outside what this program was given
      Read_Only_Place, --  granted for reading only
      Too_Large,       --  beyond the capacity the host allows
      Failed);

   --  A host provides these operations on its own type (Text_Shell is
   --  generic over them; the CuBit user runtime has no tagged types, so the
   --  binding is static):
   --
   --  Chunk_Limit (H) return Positive: the most bytes one Poll_Load hands
   --    over or one Save_Chunk takes (the host's transfer area).
   --  Start_Load (H, Name, Size, Status), then Poll_Load (H, Into, Got,
   --    Status) until Done: Size is the file's length when known (0
   --    otherwise); Got bytes of Into have arrived.
   --  Start_Save (H, Name, Status), Save_Chunk (H, Bytes, Status) in order
   --    (Busy: nothing taken, offer it again), Finish_Save (H, Status), then
   --    Poll_Save (H, Status) until Done: the file is replaced atomically
   --    (a sibling written, flushed and renamed over it), so a crash leaves
   --    the old file or the new one.
   --  Copy (H, Text); Request_Paste (H), then Poll_Paste (H, Into, Length,
   --    Ready, Truncated): the clipboard service (a paste only with the
   --    key's activation grant).
   --  Granted_Count (H), Granted_Name (H, Index): the names to offer for
   --    Open; Display_Name (H, Name): how a name is shown (the granted place
   --    and path within it, never an ambient host path).
end Text_Hosts;
