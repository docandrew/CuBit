with CCL.Interfaces.Desktop_Launch.Icons;
with CCL.Setting_Codecs.Launch_Entry;
with CuBit.Canonical_Readers;

package body CCL.Typed_Settings is
   procedure Check (Kind : Setting_Kind; Source : String; Result : out Check_Result) is
   begin
      CCL.Typed_Values.Check (CCL.Setting_Kinds.Schema (Kind), CCL.Setting_Kinds.Root_Name (Kind), Source, Result);
      --  What the type cannot say: a Launch_Icon names an icon the atlas
      --  holds (CCL.Interfaces.Desktop_Launch.Icons, generated with it).
      if Result.Success and then Kind = Launch_Entry_Setting then
         declare
            package Icons renames CCL.Interfaces.Desktop_Launch.Icons;
            Item : CCL.Setting_Codecs.Launch_Entry.Launch_Entry_Value;
            Decoded : Boolean;
         begin
            CCL.Setting_Codecs.Launch_Entry.Decode (Result.Canonical (1 .. Result.Length), Item, Decoded);
            if not Decoded or else not Icons.Known (CuBit.Canonical_Readers.Image (Item.icon.name)) then
               declare
                  Text : constant String :=
                    "not a Launch_Entry: " & Icons.ICON_PACKAGE & " has no icon named """ &
                    CuBit.Canonical_Readers.Image (Item.icon.name) & """";
               begin
                  Result := (others => <>);
                  Result.Message_Length := Natural'Min (Text'Length, MAXIMUM_MESSAGE);
                  Result.Message (1 .. Result.Message_Length) :=
                    Text (Text'First .. Text'First + Result.Message_Length - 1);
               end;
            end if;
         end;
      end if;
   end Check;
end CCL.Typed_Settings;
