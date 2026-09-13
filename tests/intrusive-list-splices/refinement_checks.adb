with Buddy_Blocks; use Buddy_Blocks;
with Buddy_List_Refinement; use Buddy_List_Refinement;

package body Refinement_Checks is
   procedure Run is
      type Capacities is array (Positive range <>) of Positive;
   begin
      for Capacity of Capacities'[1, 2, 3, 7, 32] loop
         for Size in Order loop
            declare
               Next, Previous, Sequence, Rank : References (0 .. Capacity) :=
                 [others => 0];
               Ledger : Descriptors (0 .. Capacity);
               Count : Natural := 0;
               Accepted : Boolean;

               -- Independent forward walk: no Sequence/Rank lookup is used
               -- to decide which node is reachable from the sentinel.
               procedure Check (N, B, S, R : References; L : Descriptors;
                                Length : Natural) is
                  Seen : array (0 .. Capacity) of Boolean := [others => False];
                  Current : ID := N (0);
                  Before : ID := 0;
                  Total : Natural := 0;
                  Traversed : Natural;
                  End_Node : ID;
               begin
                  Prove_Traversal (N, B, S, R, L, Length, Size, Traversed, End_Node);
                  pragma Assert (Traversed = Length and then End_Node = 0);
                  while Current /= 0 loop
                     pragma Assert (Current <= Capacity);
                     pragma Assert (not Seen (Current));
                     Seen (Current) := True;
                     pragma Assert (B (Current) = Before);
                     Before := Current;
                     Current := N (Current);
                     Total := Total + 1;
                  end loop;
                  pragma Assert (Total = Length and then B (0) = Before);
                  for I in 1 .. Capacity loop
                     pragma Assert (Seen (I) = Matches (L (I), Listed, Size));
                     Prove_Membership (N, B, S, R, L, Length, Size, I);
                  end loop;
                  for P in 1 .. Length loop
                     for Q in 1 .. Length loop
                        if P /= Q then
                           Prove_Unique (N, B, S, R, L, Length, Size, P, Q);
                        end if;
                     end loop;
                  end loop;
               end Check;
            begin
               Next := [others => ID'Last];
               Previous := [others => ID'Last];
               Initialize_Empty (Next, Previous, Sequence, Rank, Ledger, Count, Size);
               pragma Assert (Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size));
               for I in 1 .. Capacity loop
                  Admit (Ledger (I), Size, True, Accepted);
                  pragma Assert (Accepted);
               end loop;
               Check (Next, Previous, Sequence, Rank, Ledger, Count);
               for Item in 1 .. Capacity loop
                  Insert_Front (Next, Previous, Sequence, Rank, Ledger, Count, Size, Item);
                  Check (Next, Previous, Sequence, Rank, Ledger, Count);
                  -- Every removal position for every list length; reinsert
                  -- the detached node and check again, including singleton.
                  for Position in 1 .. Count loop
                     declare
                        N : References := Next;
                        B : References := Previous;
                        S : References := Sequence;
                        R : References := Rank;
                        L : Descriptors := Ledger;
                        C : Natural := Count;
                        Removed : constant ID := S (Position);
                     begin
                        Remove_At (N, B, S, R, L, C, Size, Position);
                        Check (N, B, S, R, L, C);
                        Insert_Front (N, B, S, R, L, C, Size, Removed);
                        Check (N, B, S, R, L, C);
                     end;
                  end loop;
               end loop;
               -- Deliberately corrupted witnesses/representations must not
               -- satisfy Valid merely because the descriptor count agrees.
               declare
                  Bad : References := Next;
                  Bad_Ledger : Descriptors := Ledger;
               begin
                  Bad (0) := 0;
                  pragma Assert (not Valid (Bad, Previous, Sequence, Rank, Ledger, Count, Size));
                  Bad := Next;
                  Bad (Sequence (1)) := Sequence (1);
                  pragma Assert (not Valid (Bad, Previous, Sequence, Rank, Ledger, Count, Size));
                  pragma Assert (not Valid (Next, Previous, Sequence, Rank, Ledger, Count - 1, Size));
                  if Count > 1 then
                     Bad := Sequence;
                     Bad (2) := Bad (1);
                     pragma Assert (not Valid (Next, Previous, Bad, Rank, Ledger, Count, Size));
                  end if;
                  Move (Bad_Ledger (Sequence (1)), Size, Remove, Accepted);
                  pragma Assert (Accepted);
                  Move (Bad_Ledger (Sequence (1)), Size, Commit, Accepted);
                  pragma Assert (Accepted);
                  pragma Assert (not Valid (Next, Previous, Sequence, Rank, Bad_Ledger, Count, Size));
                  Move (Bad_Ledger (Sequence (1)), Size, Defer, Accepted);
                  pragma Assert (Accepted);
                  pragma Assert (not Valid (Next, Previous, Sequence, Rank, Bad_Ledger, Count, Size));
               end;
               while Count > 0 loop
                  Remove_At (Next, Previous, Sequence, Rank, Ledger, Count, Size, 1);
                  Check (Next, Previous, Sequence, Rank, Ledger, Count);
               end loop;
               -- A ledger-only publication must be detected as an orphan.
               Move (Ledger (1), Size, Publish, Accepted);
               pragma Assert (Accepted);
               pragma Assert (not Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size));
            end;
         end loop;
      end loop;
   end Run;
end Refinement_Checks;
