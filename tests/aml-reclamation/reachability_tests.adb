with Ada.Text_IO;
with AML_Identity.Issuer;
with AML_Object_Identifiers;
with AML_Objects.Reclamation;
with AML_Objects.Reachability;
with AML_References;
procedure Reachability_Tests is
   package O renames AML_Objects;
   package R renames O.Reclamation;
   package T renames O.Reachability;
   package IDs renames AML_Object_Identifiers;
   use type O.Allocation_Status;
   use type O.State;
   use type R.Reclaim_Status;
   use type R.Keep_Set;
   use type T.Trace_Status;
   Store : O.State := O.Empty;
   Scratch : T.Workspace;
   Reclaim_Scratch : R.Workspace;
   Seeds, Keep, Expected : T.Keep_Set := [others => False];
   Targets : T.Reference_Targets := [others => IDs.No_Address];
   Status : T.Trace_Status;
   Alloc : O.Allocation_Status;
   ID, Wrapper, Replacement, Second_Wrapper : O.Object_ID;
   Old : IDs.Object_Address;
   Owner : AML_Identity.Identity;
   OK : Boolean;
   Reclaimed : R.Reclaim_Status;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Trace is
      Before : constant O.State := Store;
   begin
      T.Trace (Store, Seeds, Targets, Scratch, Keep, Status);
      Check (Status = T.Traced and then Keep = Expected and then Store = Before);
      pragma Assert (T.Exact_Closure (Store, Seeds, Targets, Keep, Scratch));
   end Trace;
begin
   Trace;
   Seeds (O.Max_Objects) := True;
   T.Trace (Store, Seeds, Targets, Scratch, Keep, Status);
   Check (Status = T.Invalid_Seed and then Keep = Expected);
   Seeds := [others => False];
   -- Exhaust all directed graphs on three vertices, including cycles, sharing,
   -- self-edges and null slots, and all eight possible root sets. The independent
   -- model computes bounded repeated set expansion without a discovery queue.
   for Vertex in 1 .. 3 loop
      O.New_Package (Store, 3, ID, Alloc); Check (Alloc = O.Allocated and then ID = Vertex);
   end loop;
   for Graph in 0 .. 2 ** 9 - 1 loop
      for From in 1 .. 3 loop
         for Into in 1 .. 3 loop
            O.Set_Element (Store, From, Into - 1,
              (if (Graph / 2 ** ((From - 1) * 3 + Into - 1)) mod 2 = 1 then Into else O.No_Object));
         end loop;
      end loop;
      for Roots in 0 .. 7 loop
         for Vertex in 1 .. 3 loop Seeds (Vertex) := (Roots / 2 ** (Vertex - 1)) mod 2 = 1; end loop;
         Expected := Seeds;
         for Step in 1 .. 3 loop
            declare Prior : constant T.Keep_Set := Expected; begin
               for From in 1 .. 3 loop
                  if Prior (From) then
                     for Into in 1 .. 3 loop
                        if (Graph / 2 ** ((From - 1) * 3 + Into - 1)) mod 2 = 1 then
                           Expected (Into) := True;
                        end if;
                     end loop;
                  end if;
               end loop;
            end;
         end loop;
         Trace;
      end loop;
   end loop;
   -- Real sparse reuse: an old reference target must not root a replacement.
   AML_Identity.Issuer.Issue (Owner, OK); Check (OK);
   Store := O.Empty; Seeds := [others => False]; Expected := Seeds;
   O.New_Integer (Store, 7, ID, Alloc); Check (Alloc = O.Allocated);
   Old := O.Address_Of (Store, ID);
   O.New_Reference (Store, AML_References.Bind_Named (Owner, 1, 1), Wrapper, Alloc);
   Check (Alloc = O.Allocated);
   Seeds (Wrapper) := True;
   R.Reclaim (Store, Owner, Seeds, Reclaim_Scratch, Reclaimed); Check (Reclaimed = R.Reclaimed);
   Targets (Wrapper) := Old;
   Expected := Seeds; Trace;
   O.New_Integer (Store, 9, Replacement, Alloc);
   Check (Alloc = O.Allocated and then Replacement = ID);
   Trace;
   -- A supplied current target creates an edge; this is a graph fixture, not
   -- an assertion that the fabricated named descriptor passed owner validation.
   Targets (Wrapper) := O.Address_Of (Store, Replacement);
   Expected (Replacement) := True; Trace;
   Targets (Wrapper) := IDs.No_Address; Expected (Replacement) := False; Trace;
   Keep (Replacement) := True;
   pragma Assert (not T.Exact_Closure (Store, Seeds, Targets, Keep, Scratch));
   Check (Keep /= Expected);
   Targets (Replacement) := O.Address_Of (Store, Wrapper); -- non-reference ignored
   Seeds := [others => False]; Seeds (Replacement) := True; Expected := Seeds; Trace;
   O.New_Reference (Store, AML_References.Bind_Named (Owner, 2, 1), Second_Wrapper, Alloc);
   Check (Alloc = O.Allocated);
   Targets (Wrapper) := O.Address_Of (Store, Second_Wrapper);
   Targets (Second_Wrapper) := O.Address_Of (Store, Wrapper);
   Seeds := [others => False]; Seeds (Wrapper) := True;
   Expected := Seeds; Expected (Second_Wrapper) := True; Trace;
   Targets (Second_Wrapper) := O.Address_Of (Store, Replacement);
   Expected (Replacement) := True; Trace;
   Targets (Second_Wrapper) := IDs.Make_Address (O.Max_Objects, IDs.First_Incarnation);
   Expected (Replacement) := False; Trace;
   -- The maximum live object population exercises the exact queue bound.
   Store := O.Empty; Seeds := [others => False]; Targets := [others => IDs.No_Address];
   for Vertex in 1 .. O.Max_Objects loop
      O.New_Package (Store, 1, ID, Alloc); Check (Alloc = O.Allocated and then ID = Vertex);
      if Vertex > 1 then O.Set_Element (Store, Vertex - 1, 0, Vertex); end if;
   end loop;
   O.Set_Element (Store, O.Max_Objects, 0, 1);
   Seeds (1) := True; Expected := [others => True]; Trace;
   Seeds := [others => True]; Trace;
   Seeds := [others => False]; Expected := Seeds; Trace;
   pragma Assert (not T.Exact_Closure (Store, Seeds, Targets, T.Keep_Set'(others => True), Scratch));
   Ada.Text_IO.Put_Line ("REACHABILITY PASS" & Checks'Image);
   Ada.Text_IO.Put_Line ("Hosted workspace bits" & T.Workspace'Size'Image);
   Ada.Text_IO.Put_Line ("Hosted target map bits" & T.Reference_Targets'Size'Image);
end Reachability_Tests;
