with CuBit.UI.Editor;
with CCL_Desktop_Platform;

--  Copy, cut and paste on a CuBit.UI.Editor field through the platform
--  clipboard (CCL_Desktop_Platform: Desktop natively, an in-process
--  clipboard on Linux). Call it only while handling the Copy_Selection,
--  Cut_Selection or Paste_Clipboard event the user's chord produced.
--  A Sensitive field never copies through here: the platform has no
--  paste-once secret path, and a secret never becomes ordinary text.
package CCL_Field_Clipboard is
   subtype Clipboard_Event is CCL_Desktop_Platform.Window_Event range
     CCL_Desktop_Platform.Copy_Selection .. CCL_Desktop_Platform.Paste_Clipboard;

   --  Single_Line: line breaks and tabs become spaces, other control bytes
   --  are dropped (Clipboard_Text.Single_Line). Copied: the field had a
   --  selection and the platform took it.
   procedure Apply
     (Field : in out CuBit.UI.Editor.Edit_State; Event : Clipboard_Event;
      Single_Line : Boolean; Changed, Copied : out Boolean);

   --  Paste the clipboard's text into Buffer (Buffer'First .. Last), the
   --  typed literal when the entry has one.
   procedure Paste_Into (Buffer : out String; Last : out Natural; Pasted : out Boolean);
end CCL_Field_Clipboard;
