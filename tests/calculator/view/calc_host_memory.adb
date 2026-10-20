package body Calc_Host_Memory is
   use Ada.Strings.Unbounded;

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
