with Desktop_GPU_Scene.Backdrop; with Desktop_Vulkan_Startup;
with Desktop_Backdrop_Owner; with Desktop_Wallpaper;
with CuBit.Appearance; with Vulkan_Submission; with System;
with Vulkan_Device_Owner; with Vulkan_Scene;
package body Desktop_Backdrop_Real_Bridge is
   package D renames Desktop_Vulkan_Startup;
   package G renames Desktop_GPU_Scene;
   package O renames Desktop_Backdrop_Owner;
   package A renames CuBit.Appearance;
   use type System.Address;
   Front, Pending : D.Presentation_Ticket := D.No_Presentation;
   function Held_Front return Interfaces.C.int is (Interfaces.C.int (D.Presentation_Front.Buffer));
   function Held_Pending return Interfaces.C.int is (Interfaces.C.int (D.Presentation_Pending.Buffer));
   Scene : G.State;
   Owners : array (0 .. 1) of O.State;
   Style : A.Preferences;
   Version, Selected : Natural := 0;
   use type Interfaces.C.int, Vulkan_Scene.A.G.Logical_Coordinate, A.Background, G.Outcome, G.Phase, O.Outcome, Vulkan_Device_Owner.Phase;
   function Open return Interfaces.C.int is
      OK : Boolean;
   begin
      D.Initialize (25); if D.Current /= Vulkan_Device_Owner.Ready then return 1; end if;
      D.Configure_Targets (96, 64, 1, 32 * 1024 * 1024, OK); if not OK then return 2; end if;
      D.Prepare_Pipeline (OK); if not OK then return 3; end if;
      D.Configure_Upload (65536, OK); return (if OK then 0 else 4);
   end Open;
   function Start (Version : Interfaces.C.int) return Interfaces.C.int is
      N : constant Natural := Natural (Version) mod 24;
      Source : Vulkan_Submission.Source_Ticket; Result : O.Outcome;
   begin
      Desktop_Backdrop_Real_Bridge.Version := Natural (Version);
      Style := (A.Color_Scheme'Val (N / 12), A.Background'Val ((N / 3) mod 4), A.Placement'Val (N mod 3));
      if Style.Backdrop not in A.Wallpaper | A.Cubie then return 0; end if;
      Selected := (if Style.Backdrop = A.Cubie then 1 else 0);
      O.Acquire (Owners (Selected), O.Slot (128 + Selected), Style.Backdrop, Source, Result);
      return (case Result is when O.Available => 0, when O.Pending => 1, when others => -1);
   end Start;
   function Clip return Vulkan_Scene.A.G.Physical_Rectangle is
     (case (Version / 24) mod 3 is
        when 0 => (0, 0, 96, 64), when 1 => (3, 5, 79, 49), when others => (0, 0, 0, 0));
   function Import_Image return Interfaces.C.int is
      Scales : constant array (0 .. 3) of Vulkan_Scene.A.G.UI_Scale := ((1, 1), (5, 4), (3, 2), (2, 1));
      Source : Vulkan_Submission.Source_Ticket := Vulkan_Submission.No_Source;
      Result : O.Outcome; OK : Boolean; Released : System.Address;
   begin
      if Style.Backdrop in A.Wallpaper | A.Cubie then
         O.Acquire (Owners (Selected), O.Slot (128 + Selected), Style.Backdrop, Source, Result);
         if Result /= O.Available then return 1; end if;
      end if;
      G.Begin_Frame (Scene, (96, 64, Vulkan_Scene.A.G.Orientation'Val (Version / 96),
        Scales ((Version / 24) mod 4), -20, 10), 0, OK); if not OK then return 2; end if;
      G.Set_Clip (Scene, Clip, OK); if not OK then return 7; end if;
      G.Backdrop.Capture (Scene, Style, Source, OK); if not OK then return 3; end if;
      if Style.Backdrop in A.Wallpaper | A.Cubie then
         if G.Image_Reader_Count (Scene) /= 1 or else not D.Source_Pinned (Source) then return 4; end if;
         -- Actual provider release must be blocked before submission, while
         -- only the CPU snapshot (not a GPU fence) protects this image.
         D.Release_Source (Source, Released);
         if Released /= System.Null_Address or else not D.Source_Held (Source) then return 5; end if;
      elsif G.Image_Reader_Count (Scene) /= 0 then return 6;
      end if;
      return 0;
   end Import_Image;
   function Render return Interfaces.C.int is
      OK : Boolean; Result : G.Outcome;
   begin
      D.Damage_Output ((0, 0, 96, 64), OK); if not OK then return 1; end if;
      G.Finish (Scene, Result); return (if Result = G.Pending and G.Current (Scene) = G.Submitted then 0 else 2);
   end Render;
   function Poll_Upload return Interfaces.C.int is
      Result : O.Outcome;
   begin
      O.Poll (Owners (Selected), Result);
      return (case Result is when O.Available => 0, when O.Pending => 1, when others => -1);
   end Poll_Upload;
   function Poll_Frame return Interfaces.C.int is
      Result : G.Outcome; OK : Boolean;
   begin
      G.Poll (Scene, Result); if Result /= G.Complete then return 1; end if;
      if G.Image_Reader_Count (Scene) /= 0 then return 4; end if;
      if Version = 0 then
         D.Take_Presentation (Front);
         D.Confirm_Presentation (Front, D.No_Presentation, True, OK); if not OK then return 2; end if;
      elsif Version = 1 then
         D.Take_Presentation (Pending); if Pending.Buffer = 0 then return 3; end if;
      end if;
      return 0;
   end Poll_Frame;
   procedure Reference (Target : System.Address) is
      Pixels : array (0 .. 96 * 64 - 1) of Interfaces.Unsigned_32 with Import, Address => Target;
      Area : constant Vulkan_Scene.A.G.Physical_Rectangle := Clip;
   begin
      -- Scene background fills are opaque RGB, including color zero.
      Pixels := (others => 16#FF000000#);
      Desktop_Wallpaper.Paint (Target, 96, 64, 96 * 4,
        Natural (Area.Left), Natural (Area.Top), Natural (Area.Right) - Natural (Area.Left),
        Natural (Area.Bottom) - Natural (Area.Top), Style);
   end Reference;
   function Close return Interfaces.C.int is
      Safe : Boolean; Latest : D.Presentation_Ticket;
   begin
      G.Close (Scene, Safe); if not Safe then return 1; end if;
      for I in Owners'Range loop O.Close (Owners (I), True, Safe); if not Safe then return 2; end if; end loop;
      D.Cancel_Presentation (Pending, True, Safe); if not Safe then return 4; end if;
      D.Take_Presentation (Latest); if Latest.Buffer = 0 then return 5; end if;
      D.Confirm_Presentation (Latest, Front, True, Safe); if not Safe then return 6; end if;
      D.Retire_Presentation (Latest, True, Safe); if not Safe then return 7; end if;
      D.Stop; return (if D.Current = Vulkan_Device_Owner.Retired and D.Charged_Bytes = 0 then 0 else 3);
   end Close;
end Desktop_Backdrop_Real_Bridge;
