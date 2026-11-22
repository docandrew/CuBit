with CuBit.Settings;
with Startup;

package body Bible_Preferences is
   use all type CuBit.Settings.Outcome;
   package Face_Setting renames Startup.Settings.Bible_Reading_Face;
   package Translation_Setting renames Startup.Settings.Bible_Translation;
   package Red_Setting renames Startup.Settings.Bible_Red_Letters;

   function Saved (Item : Bible_Translations.Reader_Preference) return String is
   begin
      case Item is
         when Bible_Translations.Reading_Face_Preference =>
            declare
               Stored : constant Face_Setting.Reading := Face_Setting.Read;
            begin
               return (if Stored.Kind = Present then CuBit.Settings.Image (Stored.Item) else "");
            end;
         when Bible_Translations.Translation_Preference =>
            declare
               Stored : constant Translation_Setting.Reading := Translation_Setting.Read;
            begin
               return (if Stored.Kind = Present then CuBit.Settings.Image (Stored.Item) else "");
            end;
         when Bible_Translations.Red_Letters_Preference =>
            declare
               Stored : constant Red_Setting.Reading := Red_Setting.Read;
            begin
               return (if Stored.Kind = Present then CuBit.Settings.Image (Stored.Item) else "");
            end;
      end case;
   end Saved;

   procedure Save (Item : Bible_Translations.Reader_Preference; Value : String) is
      Ignore : CuBit.Settings.Change_Outcome;
      Text : constant CuBit.Settings.Text := CuBit.Settings.To_Text (Value);
   begin
      --  A choice that cannot be saved still applies for this session.
      case Item is
         when Bible_Translations.Reading_Face_Preference => Face_Setting.Write (Text, Ignore);
         when Bible_Translations.Translation_Preference => Translation_Setting.Write (Text, Ignore);
         when Bible_Translations.Red_Letters_Preference => Red_Setting.Write (Text, Ignore);
      end case;
   end Save;
end Bible_Preferences;
