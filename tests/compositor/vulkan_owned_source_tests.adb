with Ada.Text_IO; with Interfaces; with System; with System.Storage_Elements;
with Vulkan_Owned_Source;
procedure Vulkan_Owned_Source_Tests is
   package O renames Vulkan_Owned_Source;
   package I renames O.I; package C renames O.C; package V renames O.V; package A renames O.A;
   use type I.Phase, C.Phase, C.Serial, C.Child, O.State, A.State, A.Ticket, Interfaces.Unsigned_32;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   function Addr (N : Natural) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   procedure Context_Set (Create, Release : U32)
     with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32)
     with Import, Convention => C, External_Name => "image_mock";
   function Binds return U32 with Import, Convention => C, External_Name => "image_mock_binds";
   function Releases return U32 with Import, Convention => C, External_Name => "image_mock_releases";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
begin
   for Failure in 0 .. 8 loop
      declare
         Source : O.State; Context, Foreign : C.State; Submission : V.State;
         Budget : A.State := A.Open (if Failure = 1 then 4095 else 4096);
         Children : array (1 .. C.Maximum_Children) of C.Child;
         OK, Released : Boolean;
      begin
         Reset; Context_Set (0, 0);
         Image_Set (4096, 1, (if Failure = 2 then 2 elsif Failure = 3 then 1 else 0),
            (if Failure = 4 then 2 elsif Failure = 5 then 1 else 0),
            (if Failure = 6 then 2 else 0));
         C.Initialize (Context, Addr (31), OK); pragma Assert (OK);
         Submission := V.Open (if Failure = 7 then Addr (32) else C.Context (Context));
         if Failure = 8 then
            for Child of Children loop C.Register_Child (Context, Child); end loop;
         end if;
         O.Initialize (Source, Context, Submission, Addr (44), Budget, 1, OK);
         if Failure in 1 | 3 | 5 | 7 | 8 then
            pragma Assert (not OK and not O.Parent_Held (Source, Context) and A.Charged (Budget) = 0);
            if Failure in 7 | 8 then pragma Assert (Binds = 0); end if;
            if Failure /= 8 then pragma Assert (C.Empty (Context)); end if;
         elsif Failure in 2 | 4 then
            pragma Assert (not OK and O.Current (Source) = I.Quarantined and O.Parent_Held (Source, Context));
            C.Close (Context, Submission, Released); pragma Assert (not Released);
            O.Initialize (Source, Context, Submission, Addr (45), Budget, 1, OK);
            pragma Assert (not OK and O.Parent_Held (Source, Context));
         else
            pragma Assert (OK and O.Current (Source) = I.Live and O.Parent_Held (Source, Context));
            -- No descriptor exists yet; backing alone must retain the context.
            C.Close (Context, Submission, Released); pragma Assert (not Released);
            C.Initialize (Foreign, Addr (32), OK); pragma Assert (OK);
            O.Close (Source, Foreign, Budget, True, Released);
            pragma Assert (not Released and Releases = 0 and O.Parent_Held (Source, Context));
            O.Close (Source, Context, Budget, False, Released);
            pragma Assert (not Released and Releases = 0 and A.Charged (Budget) = 4096);
            O.Close (Source, Context, Budget, True, Released);
            if Failure = 6 then
               pragma Assert (not Released and O.Current (Source) = I.Quarantined);
               pragma Assert (O.Parent_Held (Source, Context) and A.Charged (Budget) = 4096);
               C.Close (Context, Submission, Released); pragma Assert (not Released);
            else
               pragma Assert (Released and C.Empty (Context) and A.Charged (Budget) = 0);
               C.Close (Context, Submission, Released); pragma Assert (Released);
            end if;
         end if;
      end;
   end loop;
   declare
      Source : O.State; Context : C.State; Submission : V.State;
      Budget : A.State := A.Open (4096); Old : A.Ticket := A.No_Ticket;
      Other : C.Child; OK, Released : Boolean;
   begin
      Reset; Context_Set (0, 0); Image_Set (4096, 1, 0, 0, 0);
      C.Initialize (Context, Addr (31), OK); pragma Assert (OK);
      Submission := V.Open (C.Context (Context)); C.Register_Child (Context, Other);
      for Cycle in 1 .. 32 loop
         O.Initialize (Source, Context, Submission, Addr (44), Budget, 1, OK);
         pragma Assert (OK and O.Lease (Source) /= Old and not A.Current (Budget, Old));
         Old := O.Lease (Source);
         declare Before : constant O.State := Source; begin
            O.Initialize (Source, Context, Submission, Addr (45), Budget, 1, OK);
            pragma Assert (not OK and Source = Before);
         end;
         O.Close (Source, Context, Budget, True, Released);
         pragma Assert (Released and A.Charged (Budget) = 0 and C.Held (Context, Other));
         pragma Assert (A.Issued (Budget) = Cycle and C.Sequence (Context) = C.Serial (Cycle + 1));
      end loop;
      pragma Assert (Binds = 32 and Releases = 32);
   end;
   Ada.Text_IO.Put_Line ("PASS source/context allocation, 9 failure cases, unimported retention, foreign parent, held readers, quarantine and 32 safe reuses");
end Vulkan_Owned_Source_Tests;
