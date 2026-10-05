with Ada.Text_IO;
with System.Storage_Elements;
with Interfaces;
with Vulkan_Submission.Regions;
with Vulkan_Affine_Binding;
procedure Region_Submission_Tests is
   package V renames Vulkan_Submission;
   package B renames Vulkan_Affine_Binding;
   subtype Word is Interfaces.Unsigned_32;
   use type Word, B.Outcome, V.Phase, V.Observation, System.Address;
   use type B.G.Logical_Coordinate;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : Word) with Import, Convention => C, External_Name => "submission_mock_set";
   procedure Region_Set (Status : Word) with Import, Convention => C, External_Name => "region_mock_reset";
   function Calls return Word with Import, Convention => C, External_Name => "region_mock_calls";
   Context : constant System.Address := System.Storage_Elements.To_Address (1);
   S : V.State;
   Ticket : V.Source_Ticket;
   Released : System.Address;
   OK : Boolean;
   Result : B.Outcome;
   Seen : V.Observation;
   Screen : B.G.Output := (32, 24, B.G.Unrotated, (5, 4), -20, 10);
   procedure Fresh is
   begin
      Reset; Region_Set (0); S := V.Open (Context);
      V.Install_Source (S, 0, Context, Ticket);
      V.Begin_Record (S, OK); pragma Assert (OK);
      V.Begin_Scene (S, Context, 32, 24, OK); pragma Assert (OK);
   end Fresh;
   procedure Draw (Source : V.Source_Ticket) is
   begin
      V.Regions.Draw_Output (S, Source, Screen, (-18, 12, -10, 19), (0, 0, 32, 24),
        (2, 3, 7, 9, 32, 24), True, True, Result);
   end Draw;
begin
   Fresh; Draw (Ticket); pragma Assert (Result = B.Recorded and Calls = 1 and V.Draws (S) = 1);
   V.End_Scene (S, OK); pragma Assert (OK); V.Seal (S, OK); pragma Assert (OK);
   V.Submit (S, OK); pragma Assert (OK); Set (3, 1);
   for I in 1 .. 10_000 loop
      V.Poll (S, Seen); pragma Assert (Seen = V.Still_Pending);
      V.Remove_Source (S, Ticket, Released);
      pragma Assert (Released = System.Null_Address and V.Source_Valid (S, Ticket) and not V.Can_Destroy (S));
   end loop;
   Set (3, 0); V.Poll (S, Seen); pragma Assert (Seen = V.Finished);
   V.Remove_Source (S, Ticket, Released); pragma Assert (Released = Context);
   Fresh;
   for I in 1 .. V.Maximum_Draws loop Draw (Ticket); pragma Assert (Result = B.Recorded); end loop;
   Draw (Ticket); pragma Assert (Result = B.Rejected and Calls = Word (V.Maximum_Draws) and not V.Complete_Frame (S));
   V.End_Scene (S, OK); pragma Assert (OK and not V.Complete_Frame (S));
   V.Cancel (S, OK); pragma Assert (OK);
   Fresh; Draw (V.No_Source); pragma Assert (Result = B.Rejected and Calls = 0);
   V.Cancel (S, OK); pragma Assert (OK);
   Fresh; Set (5, 1); Draw (Ticket); pragma Assert (Result = B.Rejected and Calls = 0);
   V.Cancel (S, OK); pragma Assert (OK);
   Fresh; Screen.Width := 31; Draw (Ticket); pragma Assert (Result = B.Rejected and Calls = 0);
   V.Cancel (S, OK); pragma Assert (OK); Screen.Width := 32;
   Fresh; Region_Set (1); Draw (Ticket); pragma Assert (Result = B.Rejected and Calls = 1);
   Set (4, 2); V.Cancel (S, OK); pragma Assert (not OK and V.Current (S) = V.Quarantined);
   V.Remove_Source (S, Ticket, Released); pragma Assert (Released = System.Null_Address);
   Ada.Text_IO.Put_Line ("PASS region submission: command exhaustion, source/context/target rejection, 10000 pending polls retain atlas, failed cancellation quarantines");
end Region_Submission_Tests;
