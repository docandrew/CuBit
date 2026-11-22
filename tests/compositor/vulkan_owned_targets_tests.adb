with Ada.Text_IO; with Interfaces; with System.Storage_Elements;
with Vulkan_Owned_Targets; with Vulkan_Frame;
with Vulkan_Scene; with Vulkan_Scene_Recording;
procedure Vulkan_Owned_Targets_Tests is
   package O renames Vulkan_Owned_Targets;
   package A renames O.A;
   package V renames O.V;
   package P renames O.P;
   use type O.Phase, Interfaces.Unsigned_32, P.Ticket, V.Observation;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   function Addr (N : Natural) return System.Address is (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   Requests : constant Vulkan_Frame.Targets := (Addr (1), Addr (2), Addr (3));
   procedure Mock (Bytes : U64; Mask, Prep, Bind, Release : U32)
     with Import, Convention => C, External_Name => "image_mock";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   function Calls (Index : U32) return U32 with Import, Convention => C, External_Name => "submission_mock_calls";
   procedure Bind_Fault (Value : U32) with Import, Convention => C, External_Name => "owned_target_mock_bind";
   procedure Prepare_Fault (Value : U32) with Import, Convention => C, External_Name => "owned_target_mock_prepare";
   function Releases return U32 with Import, Convention => C, External_Name => "image_mock_releases";
   procedure Check_Recording is
      package R renames Vulkan_Scene_Recording;
      package F renames Vulkan_Frame;
      package C renames Vulkan_Scene;
      package D renames F.D;
      use type R.Outcome, F.Admission, P.Slot;
   begin
      for Failure in 0 .. 9 loop
         declare
            Owner : O.State; Budget : A.State := A.Open (16384);
            Submission : V.State := V.Open (Addr (99));
            Pool : P.State := P.Open (if Failure = 6 then 2 else 1);
            Damage : D.State := D.Open (8, 6);
            Scene : C.State := C.Open ((8, 6, C.A.G.Unrotated, (1, 1), 0, 0));
            Accepted, Released : Boolean;
            Admission : F.Admission; Result : R.Outcome;
            Source : V.Source_Ticket; Returned : System.Address;
         begin
            Reset; Bind_Fault (0); Mock (4096, 1, 0, 0, 0);
            O.Allocate (Owner, Requests, Budget, 1);
            O.Attach (Owner, Addr (44), 1, Submission, Budget);
            pragma Assert (O.Ready (Owner));
            if Failure = 8 then
               V.Install_Source (Submission, 0, Addr (55), Source);
               C.Append (Scene, (Source, (0, 0, 8, 6), False, False, 0, C.Textured), Accepted);
               V.Remove_Source (Submission, Source, Returned);
            else C.Append_Physical_Fill (Scene, (1, 1, 7, 5), 16#123456#, Accepted);
            end if;
            pragma Assert (Accepted);
            if Failure not in 1 | 5 then C.Seal (Scene, Accepted); pragma Assert (Accepted); end if;
            if Failure = 9 then Prepare_Fault (2); end if;
            if Failure = 2 then Set (7, 2); end if;
            if Failure = 3 then Set (13, 2); end if;
            if Failure = 4 then Set (8, 2); end if;
            if Failure = 5 then Set (4, 2); end if;
            if Failure = 7 then Submission := V.Open (Addr (100)); end if;
            F.Begin_Record (Submission, Pool, Damage, Admission);
            pragma Assert (Admission = F.Started);
            R.Record_Scene (Scene, Owner, Submission, Pool, Damage, Result);
            pragma Assert (Calls (2) = 0 and A.Charged (Budget) = 12288);
            if Failure in 1 | 5 | 6 | 7 | 8 | 9 then pragma Assert (Calls (7) = 0); end if;
            if Failure = 0 then
               pragma Assert (Result = R.Recorded and not V.Quiescent (Submission) and
                 P.Ready (Pool) = P.None and P.Rendering (Pool) and D.Active (Damage) /= 0);
               O.Close (Owner, Submission, Pool, Budget, Released); pragma Assert (not Released and Releases = 0);
               F.Cancel (Submission, Pool, Damage, Released); pragma Assert (Released);
            elsif Failure in 2 | 4 | 5 then
               pragma Assert (Result = R.Quarantined and P.Faulted (Pool) and D.Faulted (Damage));
               O.Close (Owner, Submission, Pool, Budget, Released); pragma Assert (not Released and Releases = 0);
            else
               pragma Assert (Result = R.Cancelled and V.Quiescent (Submission) and
                 P.Writer (Pool) = P.None and D.Active (Damage) = 0 and not D.Faulted (Damage));
            end if;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("SCENE-RECORDING: PASS recorded-not-ready, no implicit queue, source/context/epoch preflight, clean cancellation and unknown retention");
   end Check_Recording;
begin
   Check_Recording;
   for Failure in 0 .. 5 loop
      declare
         S : O.State;
         Budget : A.State := A.Open (if Failure = 1 then 8192 else 16384);
         Submission : V.State := V.Open (Addr (99));
         Pool : P.State := P.Open (1);
         Released, Accepted : Boolean;
         Observation : V.Observation;
         Ticket, Previous : P.Ticket;
      begin
         Reset; Bind_Fault (0); Mock (4096, 1, 0, 0, 0);
         O.Allocate (S, Requests, Budget, 1);
         if Failure = 1 then
            pragma Assert (O.Current (S) = O.Closed and A.Charged (Budget) = 0 and Releases = 3);
         else
            pragma Assert (O.Current (S) = O.Backed and A.Charged (Budget) = 12288);
            if Failure = 2 then Set (11, 1); end if;
            if Failure = 3 then Set (11, 2); end if;
            if Failure = 4 then Bind_Fault (2); end if;
            if Failure = 5 then Set (12, 2); end if;
            O.Attach (S, Addr (44), 1, Submission, Budget);
            if Failure = 2 then
               pragma Assert (O.Current (S) = O.Closed and A.Charged (Budget) = 0 and Releases = 3);
            elsif Failure = 3 or Failure = 4 then
               pragma Assert (O.Current (S) = O.Quarantined and A.Charged (Budget) = 12288 and Releases = 0);
            else
               pragma Assert (O.Ready (S));
               O.Close (S, V.Open (Addr (100)), Pool, Budget, Released);
               pragma Assert (not Released and Releases = 0 and Calls (12) = 0 and A.Charged (Budget) = 12288);
               V.Begin_Record (Submission, Accepted); pragma Assert (Accepted);
               O.Close (S, Submission, Pool, Budget, Released);
               pragma Assert (not Released and Releases = 0 and Calls (12) = 0);
               V.Cancel (Submission, Accepted); pragma Assert (Accepted);
               V.Begin_Record (Submission, Accepted); pragma Assert (Accepted);
               V.Begin_Scene (Submission, O.Bindings (S) (1), 32, 24, Accepted); pragma Assert (Accepted);
               V.End_Scene (Submission, Accepted); pragma Assert (Accepted);
               V.Seal (Submission, Accepted); pragma Assert (Accepted);
               V.Submit (Submission, Accepted); pragma Assert (Accepted);
               O.Close (S, Submission, Pool, Budget, Released);
               pragma Assert (not Released and Releases = 0 and Calls (12) = 0);
               Set (3, 1); V.Poll (Submission, Observation);
               pragma Assert (Observation = V.Still_Pending);
               O.Close (S, Submission, Pool, Budget, Released);
               pragma Assert (not Released and Releases = 0 and Calls (12) = 0);
               Set (3, 0); V.Poll (Submission, Observation); pragma Assert (Observation = V.Finished);
               -- Wrong output epoch cannot retire any backing.
               O.Close (S, Submission, P.Open (2), Budget, Released);
               pragma Assert (not Released and Releases = 0 and Calls (12) = 0);
               P.Acquire (Pool, Ticket);
               O.Close (S, Submission, Pool, Budget, Released);
               pragma Assert (not Released and Releases = 0);
               P.Start_Render (Pool, Ticket); P.Finish_Render (Pool, Ticket, P.Completed);
               O.Close (S, Submission, Pool, Budget, Released);
               pragma Assert (not Released and Releases = 0);
               P.Present (Pool, Ticket);
               O.Close (S, Submission, Pool, Budget, Released);
               pragma Assert (not Released and Releases = 0);
               Previous := P.Front (Pool);P.Latch_Display (Pool, Ticket, Previous, True);
               O.Close (S, Submission, Pool, Budget, Released);
               pragma Assert (not Released and Releases = 0);
               P.Retire_Front (Pool, Ticket, True);
               O.Close (S, Submission, Pool, Budget, Released);
               if Failure = 5 then
                  pragma Assert (not Released and O.Current (S) = O.Quarantined and A.Charged (Budget) = 12288 and Releases = 0);
               else pragma Assert (Released and O.Current (S) = O.Closed and A.Charged (Budget) = 0 and Releases = 3);
               end if;
            end if;
         end if;
      end;
   end loop;
   for Kind in 0 .. 2 loop
      declare
         S : O.State; Budget : A.State := A.Open (16384);
         R : Vulkan_Frame.Targets := Requests; Released : Boolean;
      begin
         Reset; Bind_Fault (0); Mock (4096, 1, 0, 0, 0);
         if Kind = 1 then R (2) := R (1); end if;
         if Kind = 2 then R (2) := System.Null_Address; end if;
         O.Allocate (S, R, Budget, 1);
         if Kind = 0 then
            O.Close (S, V.Open (Addr (99)), P.Open (1), Budget, Released);
            pragma Assert (Released and Releases = 3);
         else pragma Assert (Releases = 0);
         end if;
         pragma Assert (O.Current (S) = O.Closed and A.Charged (Budget) = 0);
      end;
   end loop;
   declare
      S : O.State; Budget : A.State := A.Open (16384);
   begin
      Reset; Bind_Fault (0); Mock (4096, 1, 0, 0, 0);
      O.Allocate (S, Requests, Budget, 1);
      O.Attach (S, Addr (44), 1, V.Open (System.Null_Address), Budget);
      pragma Assert (O.Current (S) = O.Closed and A.Charged (Budget) = 0 and Releases = 3 and Calls (11) = 0);
   end;
   Ada.Text_IO.Put_Line ("Owned targets: budget rollback, uncertain view/bind/release retention, submission identity, null context, epoch and writer/ready/display/front gates PASS");
end Vulkan_Owned_Targets_Tests;
