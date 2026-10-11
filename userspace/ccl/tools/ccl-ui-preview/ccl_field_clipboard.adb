with Clipboard_Text;

package body CCL_Field_Clipboard is
   package Ed renames CuBit.UI.Editor;
   use type Ed.Sensitivity;
   use type CCL_Desktop_Platform.Window_Event;

   --  The clipboard's own bound on one form.
   Paste_Buffer : String (1 .. Clipboard_Text.Maximum_Form);

   procedure Paste_Into (Buffer : out String; Last : out Natural; Pasted : out Boolean) is
      Typed : Boolean;
   begin
      CCL_Desktop_Platform.Paste (Buffer, Last, Typed, Pasted);
   end Paste_Into;

   procedure Apply
     (Field : in out Ed.Edit_State; Event : Clipboard_Event;
      Single_Line : Boolean; Changed, Copied : out Boolean)
   is
      Last, Kept : Natural;
      Pasted : Boolean;
   begin
      Changed := False;
      Copied := False;
      case Event is
         when CCL_Desktop_Platform.Copy_Selection | CCL_Desktop_Platform.Cut_Selection =>
            if Ed.Kind_Of (Field) = Ed.Sensitive or else
              Ed.Selection_First (Field) = Ed.Selection_Last (Field)
            then
               return;
            end if;
            Copied := CCL_Desktop_Platform.Copy_Text (Ed.Selected_Text (Field));
            if Copied and then Event = CCL_Desktop_Platform.Cut_Selection then
               Ed.Backspace (Field, Changed);
            end if;
         when CCL_Desktop_Platform.Paste_Clipboard =>
            Paste_Into (Paste_Buffer, Last, Pasted);
            if Pasted and then Last >= Paste_Buffer'First then
               if Single_Line then
                  Clipboard_Text.Single_Line (Paste_Buffer (1 .. Last), Kept);
               else
                  Kept := Last;
               end if;
               Ed.Insert (Field, Paste_Buffer (1 .. Kept), Changed);
            end if;
            Paste_Buffer (1 .. Last) := [others => ' '];
      end case;
   end Apply;
end CCL_Field_Clipboard;
