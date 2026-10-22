with Interfaces;
with CCL_Calc_Options;

--  The Calculator's platform services on CuBit: the desktop clipboard
--  (CuBit.UI.Clipboard) for the window's surface, and its remembered mode
--  and options in Config (the typed setting calculator.options, within
--  the manifest's config scope "calculator."). Nothing else: the
--  calculator needs no files and no other authority.
package Calc_Host_Native is
   type Host is limited private;
   procedure Set_Surface (H : in out Host; Surface : Interfaces.Unsigned_64);
   procedure Copy (H : in out Host; Text : String);
   procedure Request_Paste (H : in out Host);
   procedure Poll_Paste (H : in out Host; Into : out String; Length : out Natural; Ready : out Boolean);
   procedure Load_Options (H : in out Host; Item : out CCL_Calc_Options.Choice; Found : out Boolean);
   procedure Save_Options (H : in out Host; Item : CCL_Calc_Options.Choice);
private
   PASTE_BYTES : constant := 1_024;
   type Host is limited record
      Surface : Interfaces.Unsigned_64 := 0;
      Paste : String (1 .. PASTE_BYTES) := [others => ' '];
      Paste_Length : Natural := 0;
      Paste_Ready : Boolean := False;
   end record;
end Calc_Host_Native;
