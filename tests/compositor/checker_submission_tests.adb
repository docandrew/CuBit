with Ada.Text_IO; with System.Storage_Elements; with Interfaces;
with Vulkan_Submission.Checkers;
procedure Checker_Submission_Tests is
   package V renames Vulkan_Submission; package C renames V.Checkers; package G renames C.G;
   subtype Word is Interfaces.Unsigned_32;
   use type Word, Interfaces.Integer_64, V.Phase, V.Observation, System.Address;
   use type G.Logical_Coordinate;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : Word) with Import, Convention => C, External_Name => "submission_mock_set";
   procedure Checker_Set (Code : Word) with Import, Convention => C, External_Name => "checker_mock_set";
   procedure Fail_At (Index : Word) with Import, Convention => C, External_Name => "checker_mock_fail_at";
   function Last_Context return System.Address with Import, Convention => C, External_Name => "checker_mock_context";
   function Calls return Word with Import, Convention => C, External_Name => "checker_mock_calls";
   function Field (Index : Word) return Interfaces.Integer_64 with Import, Convention => C, External_Name => "checker_mock_field";
   Context : constant System.Address := System.Storage_Elements.To_Address (1);
   S : V.State; OK : Boolean; Ticket : V.Source_Ticket; Released : System.Address;
   Seen : V.Observation;
   Screen : constant G.Output := (32, 24, G.Unrotated, (5, 4), -20, 10);
   Surface : constant G.Logical_Rectangle := (-19, 12, -11, 17);
   Damage : constant G.Physical_Rectangle := (3, 4, 29, 21);
   Plan : C.D.State;
   procedure Fresh is
   begin
      -- Independent mock device lifetime, never recovery of a real context.
      Reset; Checker_Set (0); S := V.Open (Context);
      V.Install_Source (S, 0, Context, Ticket);
      V.Begin_Record (S, OK); pragma Assert (OK);
      V.Begin_Scene (S, Context, 32, 24, OK); pragma Assert (OK);
   end Fresh;
   procedure Draw is
   begin C.Draw_Output (S, Screen, Surface, Damage, 16#2468AC#, OK); end Draw;
   procedure Retained is
   begin
      V.Remove_Source (S, Ticket, Released);
      pragma Assert (Released = System.Null_Address and V.Source_Valid (S, Ticket) and not V.Can_Destroy (S));
   end Retained;
begin
   Fresh; Draw; pragma Assert (OK and V.Draws (S) = 1 and Calls = 1 and Last_Context = Context);
   declare
      type Values is array (0 .. 15) of Interfaces.Integer_64;
      Expected : constant Values := (-19, 12, -11, 17, -20, 10, 5, 4, 32, 24, 0, 3, 4, 9, 5, 16#2468AC#);
   begin
      for I in Expected'Range loop pragma Assert (Field (Word (I)) = Expected (I)); end loop;
   end;
   Retained;
   C.Draw_Output (S, Screen, (1, 1, 0, 0), Damage, 0, OK);
   pragma Assert (OK and V.Draws (S) = 2 and Calls = 1);
   C.Draw_Output (S, Screen, Surface, (9, 9, 1, 1), 0, OK);
   pragma Assert (OK and V.Draws (S) = 3 and Calls = 1);
   C.Draw_Output (S, (31, 24, G.Unrotated, (1, 1), 0, 0), Surface, Damage, 0, OK);
   pragma Assert (not OK and V.Draws (S) = 4 and Calls = 1);
   Draw; pragma Assert (not OK and V.Draws (S) = 4 and Calls = 1); Retained;
   V.Cancel (S, OK); pragma Assert (OK and V.Quiescent (S));
   for Fault in 1 .. 3 loop
      Fresh; Checker_Set ((case Fault is when 1 => 1, when 2 => 2, when others => Word'Last));
      Draw; pragma Assert (not OK and V.Draws (S) = 1 and Calls = 1); Retained;
      if Fault = 3 then Set (4, 2); end if;
      V.Cancel (S, OK);
      pragma Assert (OK = (Fault /= 3));
      if not OK then pragma Assert (V.Current (S) = V.Quarantined); Retained; end if;
   end loop;
   Fresh;
   for I in 1 .. V.Maximum_Draws loop Draw; pragma Assert (OK); end loop;
   Draw; pragma Assert (not OK and V.Draws (S) = V.Maximum_Draws and Calls = Word (V.Maximum_Draws));
   Retained; V.Cancel (S, OK); pragma Assert (OK);
   Fresh; Draw; V.End_Scene (S, OK); pragma Assert (OK);
   V.Seal (S, OK); pragma Assert (OK); V.Submit (S, OK); pragma Assert (OK);
   Set (3, 1);
   for I in 1 .. 10_000 loop V.Poll (S, Seen); pragma Assert (Seen = V.Still_Pending); Retained; end loop;
   Set (3, 0); V.Poll (S, Seen); pragma Assert (Seen = V.Finished);
   V.Remove_Source (S, Ticket, Released); pragma Assert (Released = Context and V.Can_Destroy (S));
   -- A procedural-only frame has no image tickets to accidentally mask a
   -- broken quiescence gate. The context child must still remain alive.
   V.Begin_Record (S, OK); pragma Assert (OK);
   V.Begin_Scene (S, Context, 32, 24, OK); pragma Assert (OK);
   Draw; pragma Assert (OK); V.End_Scene (S, OK); pragma Assert (OK);
   V.Seal (S, OK); pragma Assert (OK); V.Submit (S, OK); pragma Assert (OK);
   Set (3, 1);
   for I in 1 .. 10_000 loop
      V.Poll (S, Seen); pragma Assert (Seen = V.Still_Pending and not V.Can_Destroy (S));
   end loop;
   Set (3, 0); V.Poll (S, Seen); pragma Assert (Seen = V.Finished and V.Can_Destroy (S));
   Fresh; Plan := C.D.Open (32, 24); C.D.Begin_Paint (Plan, 1);
   C.Replay (S, Plan, Screen, Surface, Damage, 0, OK);
   pragma Assert (OK and Calls = 1 and V.Draws (S) = 1);
   V.Cancel (S, OK); pragma Assert (OK);
   Fresh; Plan := C.D.Open (31, 24); C.D.Begin_Paint (Plan, 1);
   C.Replay (S, Plan, Screen, Surface, Damage, 0, OK);
   pragma Assert (not OK and Calls = 0 and V.Draws (S) = 0);
   V.Cancel (S, OK); pragma Assert (OK);
   Plan := C.D.Open (32, 24); C.D.Begin_Paint (Plan, 1); C.D.Finish (Plan, C.D.Completed);
   for I in 0 .. 7 loop C.D.Change (Plan, (I * 4, 0, I * 4 + 1, 1)); end loop;
   C.D.Begin_Paint (Plan, 1);
   pragma Assert (C.D.D.Count (C.D.Painting (Plan)) = 8);
   Fresh;
   C.Replay (S, Plan, Screen, (-20, 10, 12, 34), (0, 0, 32, 24), 0, OK);
   pragma Assert (OK and Calls = 8 and V.Draws (S) = 8);
   V.Cancel (S, OK); pragma Assert (OK);
   Fresh; Checker_Set (2); Fail_At (3);
   C.Replay (S, Plan, Screen, (-20, 10, 12, 34), (0, 0, 32, 24), 0, OK);
   pragma Assert (not OK and Calls = 3 and V.Draws (S) = 3);
   Retained; V.Cancel (S, OK); pragma Assert (OK);
   Ada.Text_IO.Put_Line ("PASS checker submission: exact signed ABI/clipping, empty/mismatched geometry, status faults, command exhaustion, cancellation quarantine, 20000 held polls with/without images, eight-region replay and mid-replay failure");
end Checker_Submission_Tests;
