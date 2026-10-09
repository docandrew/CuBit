with Desktop_GPU_Scene.Drawing; with Compositor_Text; with Desktop_Vulkan_Startup;
with Vulkan_Glyph_Sources; with Vulkan_Device_Owner; with Vulkan_Scene;
package body Desktop_GPU_Scene_Real_Bridge is
   package D renames Desktop_Vulkan_Startup;
   package G renames Desktop_GPU_Scene;
   Scene : G.State;
   Key : Vulkan_Glyph_Sources.Key;
   use type G.Outcome, G.Phase, Vulkan_Device_Owner.Phase;
   procedure Capture (OK : out Boolean) is
      Items : Compositor_Text.Glyphs := (others => (65, (1000, 1000, 1032, 1017)));
   begin
      Items (16) := (Key.Code, (2, 3, 34, 20));
      G.Drawing.Fill (Scene, (0, 0, 65535, 65535), 0, OK); if not OK then return; end if;
      G.Drawing.Text (Scene, Items, 32, (0, 0, 96, 64), 16#FFFFFFFF#, OK, Key.Face);
   end Capture;
   function Open return Interfaces.C.int is
      OK : Boolean;
   begin
      D.Initialize (25); if D.Current /= Vulkan_Device_Owner.Ready then return 1; end if;
      D.Configure_Targets (96, 64, 1, 1024 * 1024, OK); if not OK then return 2; end if;
      D.Prepare_Pipeline (OK); if not OK then return 3; end if;
      D.Configure_Upload (4096, OK); return (if OK then 0 else 4);
   end Open;
   function Start (Version : Interfaces.C.int) return Interfaces.C.int is
      V : constant Natural := Natural (Version);
      Scales : constant array (0 .. 3) of Vulkan_Scene.A.G.UI_Scale := ((1, 1), (5, 4), (3, 2), (2, 1));
      Codes : constant String := "AgW?jQm@";
      Result : G.Outcome; OK : Boolean;
   begin
      Key := (V mod 2, Character'Pos (Codes (V + 1)), Scales (V / 2));
      D.Damage_Output ((0, 0, 96, 64), OK); if not OK then return 1; end if;
      G.Begin_Frame (Scene, (96, 64, Vulkan_Scene.A.G.Unrotated, Key.Scale, 0, 0), 0, OK);
      if not OK then return 1; end if;
      Capture (OK);
      if OK then return 2; end if;
      G.Finish (Scene, Result); return (if Result = G.Pending and G.Current (Scene) = G.Uploading then 0 else 3);
   end Start;
   function Import_Image return Interfaces.C.int is
      OK : Boolean;
   begin
      if G.Current (Scene) /= G.Idle then return 1; end if;
      G.Begin_Frame (Scene, (96, 64, Vulkan_Scene.A.G.Unrotated, Key.Scale, 0, 0), 0, OK);
      if not OK then return 2; end if;
      Capture (OK);
      return (if OK and G.Reader_Count (Scene) = 1 then 0 else 3);
   end Import_Image;
   function Render return Interfaces.C.int is
      OK : Boolean; Result : G.Outcome;
   begin
      G.Finish (Scene, Result); return (if Result = G.Pending and G.Current (Scene) = G.Submitted then 0 else 2);
   end Render;
   function Poll_Upload return Interfaces.C.int is
      Result : G.Outcome;
   begin G.Poll (Scene, Result); return (if Result = G.Retry then 0 else 1); end Poll_Upload;
   function Poll_Frame return Interfaces.C.int is
      Result : G.Outcome;
   begin G.Poll (Scene, Result); return (if Result = G.Complete and G.Reader_Count (Scene) = 0 then 0 else 1); end Poll_Frame;
   function Close return Interfaces.C.int is
      Safe : Boolean;
   begin
      G.Close (Scene, Safe); if not Safe then return 1; end if;
      D.Stop; return (if D.Current = Vulkan_Device_Owner.Retired and D.Charged_Bytes = 0 then 0 else 2);
   end Close;
end Desktop_GPU_Scene_Real_Bridge;
