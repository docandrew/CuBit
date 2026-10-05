with Desktop_Glyph_Upload; with Desktop_Vulkan_Startup;
with Vulkan_Glyph_Sources; with Vulkan_Owned_Targets; with Vulkan_Submission;
with Vulkan_Device_Owner; with Vulkan_Scene; with Compositor_Glyph_Layout;
with Interfaces; with System;
package body Desktop_Glyph_Real_Bridge is
   package D renames Desktop_Vulkan_Startup;
   Lease : Vulkan_Owned_Targets.A.Ticket;
   Source : Vulkan_Submission.Source_Ticket;
   Key : Vulkan_Glyph_Sources.Key;
   use type Interfaces.C.int, D.Source_Result, D.Frame_Result, D.Poll_Result,
     Vulkan_Device_Owner.Phase, System.Address;
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
      Layout : Compositor_Glyph_Layout.Layout;
      Result : D.Source_Result; OK : Boolean; Retired : System.Address;
      Advance : Natural;
   begin
      Key := (V mod 2, Character'Pos (Codes (V + 1)), Scales (V / 2));
      Layout := Compositor_Glyph_Layout.Plan (Key.Scale);
      if V > 0 then
         D.Release_Source (Source, Retired); if Retired = System.Null_Address then return -1; end if;
         if V mod 2 = 0 then D.Release_Backing (0, Lease, OK);
         else D.Restart_Content (0, Lease, OK); end if;
         if not OK then return -2; end if;
      end if;
      if V mod 2 = 0 then
         D.Allocate_Backing (0, Interfaces.Unsigned_32 (Layout.Width), Interfaces.Unsigned_32 (Layout.Height), True, Lease, Result);
         if Result /= D.Source_Accepted then return -3; end if;
      end if;
      Desktop_Glyph_Upload.Start (0, Lease, Key, Advance, Result);
      if Result /= D.Source_Accepted or else not D.Upload_Pending then return -4; end if;
      return Interfaces.C.int (Advance);
   end Start;
   function Import_Image return Interfaces.C.int is
      Result : D.Source_Result; OK : Boolean;
   begin
      D.Import_Backing (0, Lease, Source, Result);
      if Result /= D.Source_Accepted then return 1; end if;
      D.Bind_Glyph (0, Lease, Key, Source, OK);
      return (if OK then 0 else 2);
   end Import_Image;
   function Render return Interfaces.C.int is
      Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((96, 64, Vulkan_Scene.A.G.Unrotated, Key.Scale, 0, 0));
      OK : Boolean; Result : D.Frame_Result;
   begin
      D.Capture_Glyph (Scene, Key, (2, 3, 34, 20), 16#FFFFFFFF#, OK);
      if not OK then return 1; end if;
      Vulkan_Scene.Seal (Scene, OK); if not OK then return 2; end if;
      D.Damage_Output ((0, 0, 96, 64), OK); if not OK then return 3; end if;
      D.Render (Scene, Result); return (if Result = D.Submitted then 0 else 4);
   end Render;
   function Poll_Upload return Interfaces.C.int is
      Result : D.Poll_Result;
   begin D.Poll_Upload (Result); return (if Result = D.Completed then 0 else 1); end Poll_Upload;
   function Poll_Frame return Interfaces.C.int is
      Result : D.Poll_Result;
   begin D.Poll_Frame (Result); return (if Result = D.Completed then 0 else 1); end Poll_Frame;
   function Close return Interfaces.C.int is
      Retired : System.Address;
   begin
      D.Release_Source (Source, Retired); if Retired = System.Null_Address then return 1; end if;
      D.Stop;
      return (if D.Current = Vulkan_Device_Owner.Retired and D.Charged_Bytes = 0 then 0 else 2);
   end Close;
end Desktop_Glyph_Real_Bridge;
