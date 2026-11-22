--  The editor-keys test's stand-in for Desktop: one entry, and tickets
--  issued from each key press exactly as Desktop_Clipboard.Note_Key does
--  (Clipboard_Chords.Ticket_Of). A copy needs a copy ticket, a paste a
--  paste ticket; either spends it. A paste into a sensitive target, or of a
--  sensitive entry, consumes the entry (Clipboard_Store's rule).
with Interfaces; use Interfaces;
with Clipboard_Chords;
with Compositor_Clipboard_Protocol;

package Host_Clipboard is
   package P renames Compositor_Clipboard_Protocol;
   CAPACITY : constant := 4_096;
   subtype Entry_Length is Natural range 0 .. CAPACITY;

   Ticket : Clipboard_Chords.Ticket := Clipboard_Chords.No_Ticket;
   Text : String (1 .. CAPACITY) := [others => ' '];
   Length : Entry_Length := 0;
   Type_Name : String (1 .. 256) := [others => ' '];
   Type_Length : Natural := 0;
   Literal : String (1 .. CAPACITY) := [others => ' '];
   Literal_Length : Entry_Length := 0;
   Kind : P.Sensitivity := P.Ordinary;
   Copies, Pastes : Natural := 0;

   --  Desktop routing a key press to the focused surface.
   procedure Key (Code, Modifiers : Unsigned_64);
   --  Put an entry on the clipboard (another app's copy).
   procedure Put (Item : String; Item_Kind : P.Sensitivity := P.Ordinary);
   function Current return String is (Text (1 .. Length));
end Host_Clipboard;
