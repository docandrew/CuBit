with Ada.Text_IO;
with Interfaces;
with System.Storage_Elements;
with Vulkan_Scene;
with Vulkan_Submission;
procedure Region_Scene_Tests is
   package S renames Vulkan_Scene;
   package V renames Vulkan_Submission;
   use type Interfaces.Unsigned_32, S.Phase, S.Layer_Kind, S.R.Rectangle, System.Address;
   use type S.A.G.Logical_Coordinate;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Region_Set (Status : Interfaces.Unsigned_32) with Import, Convention => C, External_Name => "region_mock_reset";
   function Calls return Interfaces.Unsigned_32 with Import, Convention => C, External_Name => "region_mock_calls";
   Context : constant System.Address := System.Storage_Elements.To_Address (1);
   Screen : constant S.A.G.Output := (32, 24, S.A.G.Unrotated, (5, 4), -20, 10);
   Surface : constant S.A.G.Logical_Rectangle := (-18, 12, -10, 19);
   Region : constant S.R.Rectangle := (2, 3, 7, 9, 32, 24);
   Submission : V.State;
   Scene : S.State;
   Damage : S.D.State;
   Ticket : V.Source_Ticket;
   Released : System.Address;
   OK : Boolean;
   procedure Fresh is
   begin
      Reset; Region_Set (0); Submission := V.Open (Context);
      V.Install_Source (Submission, 0, Context, Ticket);
      Scene := S.Open (Screen); Damage := S.D.Open (32, 24); S.D.Begin_Paint (Damage, 1);
   end Fresh;
   procedure Replay is
   begin
      V.Begin_Record (Submission, OK); pragma Assert (OK);
      V.Begin_Scene (Submission, Context, 32, 24, OK); pragma Assert (OK);
      S.Replay (Scene, Submission, Damage, OK);
   end Replay;
begin
   Fresh;
   S.Append_Region (Scene, Ticket, Surface, Region, True, True, OK);
   pragma Assert (OK and S.Count (Scene) = 1 and S.Region_At (Scene, 1) = Region and
     S.Item (Scene, 1).Kind = S.Straight_Region);
   S.Append_Physical_Clip (Scene, (0, 0, 32, 24), OK); pragma Assert (OK);
   S.Append_Gradient (Scene, Surface, 0, 0, OK);
   pragma Assert (OK and S.Region_At (Scene, 1) = Region);
   S.Seal (Scene, OK); pragma Assert (OK); Replay;
   pragma Assert (OK and Calls = 1); V.Cancel (Submission, OK); pragma Assert (OK);
   Fresh; S.Append_Region (Scene, Ticket, Surface, Region, True, True, OK); pragma Assert (OK);
   V.Remove_Source (Submission, Ticket, Released); pragma Assert (Released = Context);
   S.Seal (Scene, OK); pragma Assert (OK); Replay;
   pragma Assert (not OK and Calls = 0 and V.Draws (Submission) = 0);
   V.Cancel (Submission, OK); pragma Assert (OK);
   Fresh; S.Append (Scene, (Ticket, Surface, True, False, 0, S.Region_Textured), OK);
   pragma Assert (not OK and S.Current (Scene) = S.Rejected and S.Count (Scene) = 0);
   Fresh; S.Append_Region (Scene, Ticket, Surface, (0, 0, 0, 9, 32, 24), True, True, OK);
   pragma Assert (not OK and S.Count (Scene) = 0);
   Fresh;
   for I in 1 .. S.Maximum_Layers loop
      S.Append_Region (Scene, Ticket, Surface, Region, True, True, OK); pragma Assert (OK);
   end loop;
   S.Append_Region (Scene, Ticket, Surface, Region, True, True, OK);
   pragma Assert (not OK and S.Current (Scene) = S.Rejected and S.Count (Scene) = S.Maximum_Layers);
   for I in 1 .. S.Maximum_Layers loop pragma Assert (S.Region_At (Scene, I) = Region); end loop;
   Ada.Text_IO.Put_Line ("PASS region scene: retained geometry, replay, stale-source preflight, direct-kind/invalid-window rejection, bounded overflow");
end Region_Scene_Tests;
