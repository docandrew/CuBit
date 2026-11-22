with Ada.Text_IO;
with Interfaces;
with System.Storage_Elements;
with Vulkan_Submission.Backdrops;
procedure Backdrop_Submission_Tests is
   package V renames Vulkan_Submission;
   package W renames V.Backdrops;
   package B renames W.B;
   use type B.Outcome, V.Source_Ticket, V.Phase, V.Observation, Interfaces.Unsigned_32, System.Address;
   subtype Word is Interfaces.Unsigned_32;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (I, Value : Word) with Import, Convention => C, External_Name => "submission_mock_set";
   function Calls (I : Word) return Word with Import, Convention => C, External_Name => "submission_mock_calls";
   procedure Reset_Draw (Value : Word) with Import, Convention => C, External_Name => "backdrop_submission_reset";
   function Draw_Calls return Word with Import, Convention => C, External_Name => "backdrop_submission_calls";
   Context : constant System.Address := System.Storage_Elements.To_Address (1);
   S : V.State;
   Source, Stale : V.Source_Ticket;
   Released : System.Address;
   Result : B.Outcome;
   Completion : V.Observation;
   OK : Boolean;
   procedure Fresh is
   begin
      Reset; Reset_Draw (0);
      S := V.Open (Context);
      V.Install_Source (S, 0, Context, Stale);
      V.Remove_Source (S, Stale, Released);
      pragma Assert (Released = Context);
      V.Install_Source (S, 0, Context, Source);
      pragma Assert (Source /= Stale);
      V.Begin_Record (S, OK); pragma Assert (OK);
      V.Begin_Scene (S, Context, 32, 24, OK); pragma Assert (OK);
   end Fresh;
   procedure Draw (Ticket : V.Source_Ticket; Width : B.B.G.Physical_Extent := 32;
                   Empty : Boolean := False) is
   begin
      W.Draw_Output (S, Ticket, Width, 24, 17, 5, B.B.S.Fit,
        (0, 0, (if Empty then 0 else Width), 24), Result);
   end Draw;
   procedure Held is
      Before : constant V.State := S;
   begin
      V.Remove_Source (S, Source, Released);
      pragma Assert (Released = System.Null_Address and V.Same_Sources (S, Before));
   end Held;
begin
   Fresh; Draw (Source);
   pragma Assert (Result = B.Recorded and Draw_Calls = 1 and V.Draws (S) = 1);
   Held;
   V.End_Scene (S, OK); pragma Assert (OK);
   V.Seal (S, OK); pragma Assert (OK);
   V.Submit (S, OK); pragma Assert (OK);
   Set (3, 1); V.Poll (S, Completion); pragma Assert (Completion = V.Still_Pending); Held;
   Set (3, 0); V.Poll (S, Completion); pragma Assert (Completion = V.Finished);
   V.Remove_Source (S, Source, Released); pragma Assert (Released = Context);
   Fresh; Draw (Source, Empty => True);
   pragma Assert (Result = B.Empty and Draw_Calls = 0 and V.Draws (S) = 1 and V.Complete_Frame (S));
   for Fault in 1 .. 5 loop
      Fresh;
      case Fault is
         when 1 => Draw (V.No_Source);
         when 2 => Draw (Stale);
         when 3 => Draw (Source, Width => 31);
         when 4 => Set (5, 1); Draw (Source);
         when others => Reset_Draw (1); Draw (Source);
      end case;
      pragma Assert (Result = B.Rejected and not V.Complete_Frame (S) and V.Draws (S) = 1);
      pragma Assert (Draw_Calls = (if Fault = 5 then 1 else 0));
      pragma Assert (Calls (5) = (if Fault >= 4 then 1 else 0));
      Held;
      Draw (Source); -- A rejected candidate cannot record a later wallpaper.
      pragma Assert (Result = B.Rejected and V.Draws (S) = 1);
      pragma Assert (Draw_Calls = (if Fault = 5 then 1 else 0));
      V.Cancel (S, OK); pragma Assert (OK);
      V.Remove_Source (S, Source, Released); pragma Assert (Released = Context);
   end loop;
   Fresh;
   for I in 1 .. V.Maximum_Draws loop
      Draw (Source); pragma Assert (Result = B.Recorded);
   end loop;
   Draw (Source);
   pragma Assert (Result = B.Rejected and V.Draws (S) = V.Maximum_Draws and
     Draw_Calls = Word (V.Maximum_Draws) and not V.Complete_Frame (S));
   Set (4, 1); V.Cancel (S, OK);
   pragma Assert (not OK and V.Current (S) = V.Quarantined); Held;
   Ada.Text_IO.Put_Line ("BACKDROP SUBMISSION: PASS source generation, bounds, empty, failure, pending retirement, 4096 draw cap and quarantine");
end Backdrop_Submission_Tests;
