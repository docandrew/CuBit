with Ada.Text_IO; with Interfaces; with System; with System.Storage_Elements;
with Vulkan_Upload_Owner;
procedure Vulkan_Upload_Owner_Tests is
   package O renames Vulkan_Upload_Owner; package A renames O.A; package C renames O.C; package V renames O.V;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type O.Phase, A.Ticket, A.Phase, System.Address, U32, C.Child;
   function Addr (N : Natural) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Mock (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   function Binds return U32 with Import, Convention => C, External_Name => "upload_mock_binds";
   function Releases return U32 with Import, Convention => C, External_Name => "upload_mock_releases";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
begin
   for Failure in 0 .. 12 loop
      declare
         S : O.State; Context, Foreign : C.State; Submission : V.State;
         Budget : A.State := A.Open (if Failure = 1 then 4095 else 4096);
         Children : array (1 .. C.Maximum_Children) of C.Child;
         OK, Released : Boolean;
      begin
         Reset; Context_Set (0, 0);
         Mock ((if Failure = 7 then 127 elsif Failure = 8 then U64'Last else 4096),
           (if Failure = 10 then 0 else 1), (if Failure = 2 then 1 elsif Failure = 3 then 2 else 0),
           (if Failure = 4 then 1 elsif Failure = 5 then 2 else 0),
           (if Failure = 6 then 2 else 0), (if Failure = 9 then 1 else 0));
         C.Initialize (Context, Addr (31), OK); pragma Assert (OK);
         Submission := V.Open (if Failure = 11 then Addr (32) else C.Context (Context));
         if Failure = 12 then for Child of Children loop C.Register_Child (Context, Child); end loop; end if;
         O.Initialize (S, Context, Submission, Addr (44), 128, Budget, OK);
         if Failure in 0 | 6 then
            pragma Assert (OK and O.Current (S) = O.Live and O.Capacity (S) = 128 and O.Mapping (S) /= System.Null_Address);
            pragma Assert (A.Charged (Budget) = 4096 and O.Parent_Held (S, Context));
            C.Close (Context, Submission, Released); pragma Assert (not Released);
            C.Initialize (Foreign, Addr (32), Released); pragma Assert (Released);
            O.Close (S, Foreign, Budget, True, Released); pragma Assert (not Released and Releases = 0);
            O.Close (S, Context, Budget, False, Released); pragma Assert (not Released and Releases = 0);
            O.Close (S, Context, Budget, True, Released);
            if Failure = 0 then pragma Assert (Released and C.Empty (Context) and A.Charged (Budget) = 0);
            else pragma Assert (not Released and O.Current (S) = O.Quarantined and A.Charged (Budget) = 4096);
            end if;
         else pragma Assert (not OK); end if;
         if Failure in 3 | 5 | 6 | 9 then
            pragma Assert (O.Current (S) = O.Quarantined and O.Parent_Held (S, Context));
            pragma Assert (O.Mapping (S) = System.Null_Address);
            C.Close (Context, Submission, Released); pragma Assert (not Released);
         elsif Failure not in 11 | 12 then
            pragma Assert (O.Current (S) = O.Closed and A.Charged (Budget) = 0 and C.Empty (Context));
         else pragma Assert (Binds = 0 and O.Current (S) = O.Fresh);
         end if;
      end;
   end loop;
   declare
      S : O.State; Context : C.State; Submission : V.State;
      Budget : A.State := A.Open (40960); Held : array (1 .. 8) of A.Ticket;
      Old, Extra : A.Ticket := A.No_Ticket; OK, Released : Boolean;
   begin
      Reset; Context_Set (0, 0); Mock (4096, 1, 0, 0, 0, 0);
      C.Initialize (Context, Addr (31), OK); pragma Assert (OK); Submission := V.Open (C.Context (Context));
      for Ticket of Held loop A.Reserve (Budget, 4096, Ticket); A.Allocated (Budget, Ticket, True); end loop;
      for Cycle in 1 .. 32 loop
         O.Initialize (S, Context, Submission, Addr (44), 128, Budget, OK);
         pragma Assert (OK and O.Lease (S) /= Old and not A.Current (Budget, Old));
         pragma Assert (A.Charged (Budget) = 36864);
         A.Reserve (Budget, 1, Extra); pragma Assert (Extra = A.No_Ticket);
         Old := O.Lease (S);
         O.Close (S, Context, Budget, True, Released);
         pragma Assert (Released and A.Charged (Budget) = 32768);
         for Ticket of Held loop pragma Assert (A.Current (Budget, Ticket) and A.Status (Budget, Ticket) = A.Live); end loop;
      end loop;
      pragma Assert (Binds = 32 and Releases = 32);
   end;
   Ada.Text_IO.Put_Line ("PASS upload owner: 13 fault/retirement cases, real-size budget, ninth slot, 32 reuses, other allocations retained");
end Vulkan_Upload_Owner_Tests;
