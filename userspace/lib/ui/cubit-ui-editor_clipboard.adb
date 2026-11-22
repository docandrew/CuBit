with Clipboard_Chords;
with Clipboard_Text;

package body CuBit.UI.Editor_Clipboard is
   package E renames CuBit.UI.Editor;
   package C renames CuBit.UI.Clipboard;
   package K renames Clipboard_Chords;
   use type E.Sensitivity;
   use type C.Outcome;
   use type K.Command;

   --  Overwrite through a volatile view so the compiler cannot drop the
   --  store as dead: the buffer may hold a secret.
   procedure Scrub (Text : in out String) is
      View : String (Text'Range)
        with Import, Volatile, Address => Text'Address;
   begin
      for I in View'Range loop
         View (I) := ' ';
      end loop;
   end Scrub;

   function Kind (State : E.Edit_State) return C.Sensitivity is
     (if E.Kind_Of (State) = E.Sensitive then C.Sensitive else C.Ordinary);

   --  Paste one line of text into Buffer (Buffer'First .. Kept).
   procedure Paste_Line
     (Surface : Unsigned_64; Target : C.Sensitivity; Buffer : out String;
      Kept : out Natural; Outcome : out C.Outcome)
   is
      Last : Natural;
      Entry_Kind : C.Sensitivity;
      Count : Natural;
   begin
      Kept := 0;
      C.Paste_Text (Surface, Target, Buffer, Last, Entry_Kind, Outcome);
      if Outcome = C.Done and then Last >= Buffer'First then
         Clipboard_Text.Single_Line (Buffer (Buffer'First .. Last), Count);
         Kept := Count;
      end if;
   end Paste_Line;

   function Handle_Key
     (State : in out E.Edit_State; Surface : Unsigned_64;
      Code, Modifiers : Unsigned_64) return Result
   is
      Command : constant K.Command := C.Command_Of (Code, Modifiers);
      R : Result;
   begin
      if Command = K.No_Command then
         return R;
      end if;
      R.Handled := True;
      case Command is
         when K.Select_All_Command =>
            E.Select_All (State);
         when K.Copy_Command | K.Cut_Command =>
            if E.Selection_First (State) = E.Selection_Last (State) then
               --  Nothing selected: nothing to copy, the chord is spent.
               R.Outcome := C.Empty;
               return R;
            end if;
            C.Copy_Text (Surface, E.Selected_Text (State), Kind (State),
                         R.Outcome);
            if Command = K.Cut_Command and then R.Outcome = C.Done then
               E.Backspace (State, R.Changed);
            end if;
         when K.Paste_Command =>
            declare
               Buffer : String (1 .. E.MAX_TEXT_LENGTH);
               Kept : Natural;
            begin
               Paste_Line (Surface, Kind (State), Buffer, Kept, R.Outcome);
               if R.Outcome = C.Done then
                  E.Insert (State, Buffer (1 .. Kept), R.Changed);
               end if;
               --  The local copy may hold a secret.
               Scrub (Buffer);
            end;
         when K.No_Command =>
            null;
      end case;
      return R;
   end Handle_Key;

   function Handle_Line_Key
     (Text : in out String; Length : in out Natural; Surface : Unsigned_64;
      Code, Modifiers : Unsigned_64; Kind : C.Sensitivity := C.Ordinary)
      return Result
   is
      Command : constant K.Command := C.Command_Of (Code, Modifiers);
      R : Result;
   begin
      if Command in K.No_Command | K.Select_All_Command then
         return R;
      end if;
      R.Handled := True;
      case Command is
         when K.Copy_Command | K.Cut_Command =>
            if Length = 0 then
               R.Outcome := C.Empty;
               return R;
            end if;
            C.Copy_Text (Surface, Text (Text'First .. Text'First + Length - 1),
                         Kind, R.Outcome);
            if Command = K.Cut_Command and then R.Outcome = C.Done then
               Scrub (Text (Text'First .. Text'First + Length - 1));
               Length := 0;
               R.Changed := True;
            end if;
         when K.Paste_Command =>
            if Length = Text'Length then
               R.Outcome := C.Too_Large;
               return R;
            end if;
            declare
               Buffer : String (1 .. Text'Length - Length);
               Kept : Natural;
            begin
               Paste_Line (Surface, Kind, Buffer, Kept, R.Outcome);
               if R.Outcome = C.Done and then Kept > 0 then
                  Text (Text'First + Length .. Text'First + Length + Kept - 1) :=
                    Buffer (1 .. Kept);
                  Length := Length + Kept;
                  R.Changed := True;
               end if;
               Scrub (Buffer);
            end;
         when K.No_Command | K.Select_All_Command =>
            null;
      end case;
      return R;
   end Handle_Line_Key;
end CuBit.UI.Editor_Clipboard;
