with Ada.Text_IO;
with CuBit.Backend_Targets;
procedure Backend_Targets_Tests is
   package B renames CuBit.Backend_Targets;
   use type B.Phase, B.Identifier, B.State;
   S : B.State;
   Accepted : Boolean;
begin
   B.Prepare (S, Accepted); pragma Assert (not Accepted);
   B.Cleared (S);
   for I in B.Identifier range 1 .. 10000 loop
      declare Old : constant B.Buffer := B.Active (S); begin
         B.Prepare (S, Accepted); pragma Assert (Accepted and B.Writable (S, 1 - Old));
         B.Prepare (S, Accepted); pragma Assert (not Accepted);
         B.Seal (S, I);
         pragma Assert (not B.Writable (S, 0) and not B.Writable (S, 1));
         B.Prepare (S, Accepted); pragma Assert (not Accepted);
         B.Complete (S, I, True);
         pragma Assert (B.Active (S) = 1 - Old and B.Current (S) = B.Idle);
      end;
   end loop;
   for Fault in 0 .. 5 loop
      declare Item : B.State; Before : B.State; begin
         B.Cleared (Item); B.Prepare (Item, Accepted); B.Seal (Item, 123);
         case Fault is
            when 0 => B.Complete (Item, 122, True);
            when 1 => B.Complete (Item, 0, True);
            when 2 => B.Complete (Item, 123, False);
            when 3 => B.Cleared (Item);
            when 4 => B.Quarantine (Item);
            when others => B.Seal (Item, 124);
         end case;
         pragma Assert (B.Current (Item) = B.Failed and B.Active (Item) = 0 and B.Token (Item) = 123);
         Before := Item;
         for Repeat in 1 .. 100 loop
            B.Prepare (Item, Accepted); pragma Assert (not Accepted);
            B.Complete (Item, 123, True); B.Cleared (Item);
            pragma Assert (Item = Before);
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("BACKEND-TARGETS: PASS 10000 flips and stale/unknown/faulted lease retention");
end Backend_Targets_Tests;
