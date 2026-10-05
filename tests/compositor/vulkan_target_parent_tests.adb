with Ada.Text_IO;
with Interfaces;
with System;
with System.Storage_Elements;
with Vulkan_Owned_Targets;
with Vulkan_Frame;
procedure Vulkan_Target_Parent_Tests is
   package O renames Vulkan_Owned_Targets;
   package C renames O.C;
   package V renames O.V;
   package P renames O.P;
   package A renames O.A;
   use type O.Phase, C.Phase, C.Child, Interfaces.Unsigned_32;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   function Addr (N : Natural) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   Requests : constant Vulkan_Frame.Targets := (Addr (1), Addr (2), Addr (3));
   procedure Context_Set (Create, Release : U32)
     with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32)
     with Import, Convention => C, External_Name => "image_mock";
   function Binds return U32 with Import, Convention => C, External_Name => "image_mock_binds";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   procedure Bind_Fault (Value : U32) with Import, Convention => C, External_Name => "owned_target_mock_bind";
begin
   for Failure in 0 .. 8 loop
      declare
         Owner : O.State;
         Context, Foreign : C.State;
         Submission : V.State;
         Pool : P.State := P.Open (1);
         Budget : A.State := A.Open (if Failure = 1 then 8192 else 16384);
         Children : array (1 .. C.Maximum_Children) of C.Child;
         OK, Released : Boolean;
         Ticket, Previous : P.Ticket;
      begin
         Reset; Bind_Fault (if Failure = 3 then 2 else 0); Context_Set (0, 0);
         Image_Set (4096, 1, (if Failure = 2 then 2 else 0), 0,
                    (if Failure = 6 then 2 else 0));
         C.Initialize (Context, Addr (31), OK); pragma Assert (OK);
         Submission := V.Open (C.Context (Context));
         if Failure = 4 then Set (11, 1); end if;
         if Failure = 5 then Set (12, 2); end if;
         if Failure = 7 then Submission := V.Open (Addr (32)); end if;
         if Failure = 8 then
            for Child of Children loop
               C.Register_Child (Context, Child); pragma Assert (Child /= C.No_Child);
            end loop;
         end if;
         O.Initialize (Owner, Context, Requests, Addr (44), 1, Submission, Budget, 1);
         if Failure in 1 | 4 | 7 | 8 then
            pragma Assert (O.Current (Owner) = O.Closed and not O.Parent_Held (Owner, Context));
            pragma Assert (A.Charged (Budget) = 0);
            if Failure in 7 | 8 then pragma Assert (Binds = 0); end if;
            if Failure = 8 then
               for Child of Children loop pragma Assert (C.Held (Context, Child)); end loop;
            else pragma Assert (C.Empty (Context));
            end if;
         elsif Failure in 2 | 3 then
            pragma Assert (O.Current (Owner) = O.Quarantined and O.Parent_Held (Owner, Context));
            C.Close (Context, Submission, Released); pragma Assert (not Released);
         else
            pragma Assert (O.Ready (Owner) and O.Parent_Held (Owner, Context));
            C.Close (Context, Submission, Released);
            pragma Assert (not Released and C.Current (Context) = C.Live);
            C.Initialize (Foreign, Addr (32), OK); pragma Assert (OK);
            O.Close (Owner, Foreign, Submission, Pool, Budget, Released);
            pragma Assert (not Released and O.Parent_Held (Owner, Context));
            -- A completed GPU draw does not permit destruction of an output
            -- that still has a ready, submitted or latched display buffer.
            P.Acquire (Pool, Ticket);
            P.Start_Render (Pool, Ticket); P.Finish_Render (Pool, Ticket, P.Completed);
            O.Close (Owner, Context, Submission, Pool, Budget, Released);
            pragma Assert (not Released and O.Parent_Held (Owner, Context));
            P.Present (Pool, Ticket);
            O.Close (Owner, Context, Submission, Pool, Budget, Released);
            pragma Assert (not Released and O.Parent_Held (Owner, Context));
            Previous := P.Front (Pool); P.Latch_Display (Pool, Ticket, Previous, True);
            O.Close (Owner, Context, Submission, Pool, Budget, Released);
            pragma Assert (not Released and O.Parent_Held (Owner, Context));
            P.Retire_Front (Pool, Ticket, True);
            O.Close (Owner, Context, Submission, Pool, Budget, Released);
            if Failure = 0 then
               pragma Assert (Released and O.Current (Owner) = O.Closed and C.Empty (Context));
               pragma Assert (A.Charged (Budget) = 0);
               C.Close (Context, Submission, Released); pragma Assert (Released);
            else
               pragma Assert (not Released and O.Current (Owner) = O.Quarantined);
               pragma Assert (O.Parent_Held (Owner, Context));
               C.Close (Context, Submission, Released); pragma Assert (not Released);
            end if;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS target/context registration, clean rollback, capacity, foreign parent, display retention, quarantine");
end Vulkan_Target_Parent_Tests;
