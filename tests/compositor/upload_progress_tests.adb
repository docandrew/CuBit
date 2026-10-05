with Ada.Text_IO; with Interfaces; with Compositor_Upload_Progress; with Upload_Progress_Model;
procedure Upload_Progress_Tests is
   package P renames Upload_Progress_Model; package G renames P.G;
   package Small is new Compositor_Upload_Progress (Last_Sequence => 2);
   use type P.Phase, P.Ticket, P.State, P.Serial, Small.Ticket, Small.Phase;
   State : P.State; Plan, Ignored : G.Plan;
   Ticket, Stale, Rejected : P.Ticket := P.No_Ticket;
   OK, Discard, Ignored_Discard : Boolean;
   Before_Rows, Steps : Natural;
begin
   pragma Assert (P.Valid (State) and not P.Publishable (State, 0));
   P.Begin_Image (State, 0, 8, 4, G.BGRA8, OK); pragma Assert (not OK);
   P.Begin_Image (State, 1, 0, 4, G.BGRA8, OK); pragma Assert (not OK);
   for Content in 1 .. 4 loop
      -- Reuse the same backing for two content versions, then a new identity.
      declare Identity : constant Natural := (Content + 1) / 2; begin
         P.Begin_Image (State, Identity, 3840, 2160, G.BGRA8, OK); pragma Assert (OK);
         P.Begin_Write (State, 1, Ignored, Rejected, Ignored_Discard, OK);
         pragma Assert (not OK and Rejected = P.No_Ticket and P.Completed_Rows (State) = 0);
         Steps := 0;
         while not P.Publishable (State, Identity) loop
            Before_Rows := P.Completed_Rows (State);
            P.Begin_Write (State, 65536, Plan, Ticket, Discard, OK); pragma Assert (OK);
            pragma Assert (Discard = (Before_Rows = 0) and P.Can_Write (State, Ticket));
            if Steps mod 7 = 0 then
               Stale := Ticket; P.Cancel (State, Ticket, True);
               pragma Assert (P.Completed_Rows (State) = Before_Rows and not P.Can_Write (State, Ticket));
               P.Begin_Write (State, 65536, Plan, Ticket, Discard, OK); pragma Assert (OK and Ticket /= Stale);
               pragma Assert (Discard = (Before_Rows = 0));
            end if;
            declare Before : constant P.State := State; begin
               P.Begin_Image (State, Identity + 1, 3840, 2160, G.BGRA8, OK); pragma Assert (not OK and State = Before);
               P.Begin_Write (State, 65536, Ignored, Rejected, Ignored_Discard, OK); pragma Assert (not OK and State = Before);
            end;
            P.Submitted (State, Ticket, OK); pragma Assert (OK and not P.Can_Write (State, Ticket));
            declare Before : constant P.State := State; begin
               P.Cancel (State, Ticket, True); pragma Assert (State = Before);
               P.Submitted (State, Ticket, OK); pragma Assert (not OK and State = Before);
               P.Observe (State, Stale, P.Completed); pragma Assert (State = Before);
               for N in 1 .. 10 loop P.Observe (State, Ticket, P.Still_Pending); pragma Assert (State = Before); end loop;
            end;
            P.Observe (State, Ticket, P.Completed);
            pragma Assert (P.Completed_Rows (State) = Before_Rows + G.Area (Plan).Height);
            P.Observe (State, Ticket, P.Completed);
            pragma Assert (P.Completed_Rows (State) = Before_Rows + G.Area (Plan).Height);
            pragma Assert (P.Publishable (State, Identity) = (P.Completed_Rows (State) = 2160));
            Stale := Ticket; Steps := Steps + 1;
         end loop;
         pragma Assert (Steps = 540 and not P.Publishable (State, Identity + 1));
         P.Begin_Image (State, Identity, 1920, 1080, G.BGRA8, OK); pragma Assert (not OK);
      end;
   end loop;
   P.Begin_Image (State, 1, 3840, 2160, G.BGRA8, OK); pragma Assert (not OK);
   for Failure in 0 .. 1 loop
      declare S : P.State; T : P.Ticket; begin
         P.Begin_Image (S, 1, 32, 24, G.R8, OK); pragma Assert (OK);
         P.Begin_Write (S, 128, Plan, T, Discard, OK); pragma Assert (OK and Discard);
         if Failure = 0 then P.Cancel (S, T, False);
         else P.Submitted (S, T, OK); P.Observe (S, T, P.Uncertain);
         end if;
         pragma Assert (P.Current (S) = P.Quarantined and not P.Publishable (S, 1));
         P.Begin_Image (S, 2, 32, 24, G.R8, OK); pragma Assert (not OK);
         P.Begin_Write (S, 128, Plan, T, Discard, OK); pragma Assert (not OK);
      end;
   end loop;
   declare S : Small.State; T : Small.Ticket; begin
      Small.Begin_Image (S, 1, 8, 3, G.BGRA8, OK); pragma Assert (OK);
      for N in 1 .. 2 loop
         Small.Begin_Write (S, 32, Plan, T, Discard, OK); pragma Assert (OK);
         Small.Submitted (S, T, OK); pragma Assert (OK);
         Small.Observe (S, T, Small.Completed);
      end loop;
      Small.Begin_Write (S, 32, Plan, T, Discard, OK); pragma Assert (not OK and T = Small.No_Ticket);
      pragma Assert (Small.Completed_Rows (S) = 2 and not Small.Publishable (S, 1));
      Small.Begin_Image (S, 2, 8, 3, G.BGRA8, OK); pragma Assert (OK);
      Small.Begin_Write (S, 32, Plan, T, Discard, OK); pragma Assert (not OK);
   end;
   Ada.Text_IO.Put_Line ("PASS upload progress: 2160 completed chunks, 21600 pending observations, same-backing content reuse, stale/duplicate/cancel/uncertain/exhaustion publication gates");
end Upload_Progress_Tests;
