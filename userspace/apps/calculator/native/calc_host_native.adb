with CuBit.Settings;
with CuBit.UI.Clipboard;
with Startup;

package body Calc_Host_Native is
   use type CuBit.UI.Clipboard.Outcome;

   procedure Load_Options (H : in out Host; Item : out CCL_Calc_Options.Choice; Found : out Boolean) is
      pragma Unreferenced (H);
      use all type CuBit.Settings.Outcome;
      Stored : constant Startup.Settings.Calculator_Options.Reading := Startup.Settings.Calculator_Options.Read;
   begin
      Found := Stored.Kind = Present;
      Item := (if Found then Stored.Item else (others => <>));
   end Load_Options;

   procedure Save_Options (H : in out Host; Item : CCL_Calc_Options.Choice) is
      pragma Unreferenced (H);
      Ignore : CuBit.Settings.Change_Outcome;
   begin
      Startup.Settings.Calculator_Options.Write (Item, Ignore);
   end Save_Options;

   procedure Set_Surface (H : in out Host; Surface : Interfaces.Unsigned_64) is
   begin
      H.Surface := Surface;
   end Set_Surface;

   procedure Copy (H : in out Host; Text : String) is
      Result : CuBit.UI.Clipboard.Outcome;
   begin
      CuBit.UI.Clipboard.Copy_Text (H.Surface, Text, CuBit.UI.Clipboard.Ordinary, Result);
   end Copy;

   procedure Request_Paste (H : in out Host) is
      Last : Natural;
      Kind : CuBit.UI.Clipboard.Sensitivity;
      Result : CuBit.UI.Clipboard.Outcome;
   begin
      CuBit.UI.Clipboard.Paste_Text (H.Surface, CuBit.UI.Clipboard.Ordinary, H.Paste, Last, Kind, Result);
      H.Paste_Ready := True;
      H.Paste_Length := (if Result = CuBit.UI.Clipboard.Done then Last - H.Paste'First + 1 else 0);
   end Request_Paste;

   procedure Poll_Paste (H : in out Host; Into : out String; Length : out Natural; Ready : out Boolean) is
   begin
      Ready := H.Paste_Ready;
      H.Paste_Ready := False;
      Length := Natural'Min (H.Paste_Length, Into'Length);
      Into := [others => ' '];
      Into (Into'First .. Into'First + Length - 1) := H.Paste (1 .. Length);
   end Poll_Paste;
end Calc_Host_Native;
