with Calc_Settings;

package body Calc_Host_Memory is
   use Ada.Strings.Unbounded;

   procedure Load_Options (H : in out Host; Item : out CCL_Calc_Options.Choice; Found : out Boolean) is
   begin
      Calc_Settings.Decode (To_String (H.Options), Item, Found);
   end Load_Options;
   procedure Save_Options (H : in out Host; Item : CCL_Calc_Options.Choice) is
   begin
      H.Options := To_Unbounded_String (Calc_Settings.Source (Item));
   end Save_Options;
   function Stored_Options (H : Host) return String is (To_String (H.Options));
   procedure Set_Stored_Options (H : in out Host; Source : String) is
   begin
      H.Options := To_Unbounded_String (Source);
   end Set_Stored_Options;

   function Clipboard (H : Host) return String is (To_String (H.Text));
   procedure Set_Clipboard (H : in out Host; Text : String) is
   begin
      H.Text := To_Unbounded_String (Text);
   end Set_Clipboard;
   procedure Copy (H : in out Host; Text : String) is
   begin
      H.Text := To_Unbounded_String (Text);
   end Copy;
   procedure Request_Paste (H : in out Host) is
   begin
      H.Pending := True;
   end Request_Paste;
   procedure Poll_Paste (H : in out Host; Into : out String; Length : out Natural; Ready : out Boolean) is
      Text : constant String := To_String (H.Text);
   begin
      Ready := H.Pending;
      H.Pending := False;
      Length := Natural'Min (Text'Length, Into'Length);
      Into := [others => ' '];
      Into (Into'First .. Into'First + Length - 1) := Text (Text'First .. Text'First + Length - 1);
   end Poll_Paste;
end Calc_Host_Memory;
