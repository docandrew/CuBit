with Ada.Text_IO; with Interfaces; with System; with System.Storage_Elements;
with Vulkan_Upload_Recording;
procedure Vulkan_Upload_Recording_Tests is
   package R renames Vulkan_Upload_Recording; package G renames R.G;
   package U renames R.U; package S renames R.S; package C renames R.C; package V renames R.V;
   package A renames U.A;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type V.Phase, V.Observation, U32, System.Address, U.Phase, S.I.Phase;
   function Addr (N : Natural) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Upload_Set (Bytes : U64; Types, Prep, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32)
     with Import, Convention => C, External_Name => "image_mock";
   procedure Record_Set (Value : U32) with Import, Convention => C, External_Name => "upload_record_mock_set";
   function Record_Calls return U32 with Import, Convention => C, External_Name => "upload_record_mock_calls";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
begin
   for Failure in 0 .. 9 loop
      declare
         Context, Foreign : C.State; Submission : V.State; Upload : U.State; Source : S.State;
         Budget : A.State := A.Open (8192); Plan : G.Plan;
         OK, Released : Boolean; Ticket : V.Source_Ticket; Key : System.Address;
         Observed : V.Observation;
      begin
         Reset; Context_Set (0, 0); Upload_Set (4096, 1, 0, 0, 0, 0); Image_Set (4096, 1, 0, 0, 0);
         Record_Set (if Failure = 5 then 1 else 0);
         C.Initialize (Context, Addr (31), OK); pragma Assert (OK); Submission := V.Open (C.Context (Context));
         U.Initialize (Upload, Context, Submission, Addr (44), 128, Budget, OK); pragma Assert (OK);
         S.Initialize (Source, Context, Submission, Addr (55), Budget, 1, OK); pragma Assert (OK);
         G.Make (8, 4, (if Failure = 1 then 127 elsif Failure = 4 then 256 else 128),
                 (0, 0, 8, 4), 0, 0, G.BGRA8, Plan, OK);
         if Failure = 2 then V.Install_Source (Submission, 0, Addr (66), Ticket); end if;
         C.Initialize (Foreign, Addr (32), OK); pragma Assert (OK);
         V.Begin_Record (Submission, OK); pragma Assert (OK);
         if Failure = 6 then
            for N in 1 .. V.Maximum_Draws loop V.Admit_Draw (Submission, OK); pragma Assert (OK); end loop;
         end if;
         R.Record_Transfer (Submission, (if Failure = 3 then Foreign else Context), Upload, Source, 0, Plan, True, OK);
         if Failure in 1 .. 6 then
            pragma Assert (not OK and not V.Complete_Frame (Submission));
            pragma Assert (Record_Calls = (if Failure = 5 then 1 else 0));
            V.Cancel (Submission, OK); pragma Assert (OK);
            if Failure = 2 then V.Remove_Source (Submission, Ticket, Key); pragma Assert (Key = Addr (66)); end if;
         else
            pragma Assert (OK and V.Draws (Submission) = 1 and Record_Calls = 1);
            if Failure = 7 then Set (1, 2); end if;
            V.Seal_Transfer (Submission, OK);
            if Failure /= 7 then
               pragma Assert (OK); if Failure = 8 then Set (2, 2); end if;
               V.Submit (Submission, OK);
               if Failure /= 8 then
                  pragma Assert (OK and V.Current (Submission) = V.Pending);
                  U.Close (Upload, Context, Budget, V.Quiescent (Submission), Released); pragma Assert (not Released);
                  Set (3, 1); V.Poll (Submission, Observed); pragma Assert (Observed = V.Still_Pending);
                  Set (3, (if Failure = 9 then 2 else 0)); V.Poll (Submission, Observed);
               end if;
            end if;
         end if;
         U.Close (Upload, Context, Budget, V.Quiescent (Submission), Released);
         pragma Assert (Released = (Failure < 7));
         S.Close (Source, Context, Budget, V.Quiescent (Submission), Released);
         pragma Assert (Released = (Failure < 7));
         if Failure >= 7 then
            pragma Assert (V.Current (Submission) = V.Quarantined and A.Charged (Budget) = 8192);
            pragma Assert (U.Parent_Held (Upload, Context) and S.Parent_Held (Source, Context));
         else pragma Assert (A.Charged (Budget) = 0 and C.Empty (Context));
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS transfer recording: bounds/parent/descriptor/admission rejection, cancellation, pending, seal/submit/poll uncertainty retention");
end Vulkan_Upload_Recording_Tests;
