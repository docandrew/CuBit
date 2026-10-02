with Ada.Text_IO;
with Surface_State_Model;
with Surface_State_Production;
procedure Surface_State_Tests is
   package P renames Surface_State_Model;
   use P;
   S : State;
   OK : Boolean;
   Front : Slot := 1;
   Next : Slot;
begin
   Stage (S, 1, 0, OK); pragma Assert (not OK);
   for N in 1 .. 4096 loop
      Configure (S, OK); pragma Assert (OK and S.Requested = N);
      Next := (if Front = 1 then 2 else 1);
      Stage (S, Next, S.Requested, OK); pragma Assert (OK);
      Present (S, Next, S.Requested - 1, S.Buffers (Next).Ticket, OK); pragma Assert (not OK);
      Present (S, Next, S.Requested, S.Buffers (Next).Ticket, OK); pragma Assert (OK);
      Retire (S, Front, S.Buffers (Front).Ticket, False);
      if N > 1 then
         pragma Assert (S.Buffers (Front).Status = Retiring);
         Stage (S, Front, S.Requested, OK); pragma Assert (not OK);
      end if;
      Retire (S, Front, S.Buffers (Front).Ticket, True);
      pragma Assert (S.Buffers (Front).Status = Empty);
      Front := Next;
   end loop;
   Configure (S, OK); pragma Assert (not OK and S.Requested = 4096);
   declare
      T : State;
   begin
      Configure (T, OK); Stage (T, 1, 1, OK); Present (T, 1, 1, 1, OK);
      Configure (T, OK); Stage (T, 2, 2, OK);
      Configure (T, OK); Present (T, 2, 2, 2, OK);
      pragma Assert (not OK and T.Buffers (1) = (Visible, 1, 1));
      Stage (T, 2, 3, OK); pragma Assert (not OK);
      Discard (T, 2, 2); Retire (T, 2, T.Buffers (2).Ticket, False);
      pragma Assert (T.Buffers (2) = (Retiring, 2, 2));
      Retire (T, 2, T.Buffers (2).Ticket, True); Stage (T, 2, 3, OK); pragma Assert (OK);
      Present (T, 2, 3, 2, OK); pragma Assert (not OK);
      Discard (T, 2, 2);
      pragma Assert (T.Buffers (2) = (Candidate, 3, 3));
      Retire (T, 2, 2, True);
      pragma Assert (T.Buffers (2) = (Candidate, 3, 3));
      Present (T, 2, 3, 3, OK); pragma Assert (OK);
      pragma Assert (T.Buffers (1) = (Retiring, 1, 1));
   end;
   declare
      T : State;
   begin
      Configure (T, OK);
      Stage (T, 1, 1, OK); Discard (T, 1, 1); Retire (T, 1, 1, True);
      Stage (T, 1, 1, OK); pragma Assert (OK and T.Buffers (1).Ticket = 2);
      Present (T, 1, 1, 1, OK); pragma Assert (not OK);
      Discard (T, 1, 1); pragma Assert (T.Buffers (1).Status = Candidate);
      Discard (T, 1, 2); Retire (T, 1, 1, True);
      pragma Assert (T.Buffers (1) = (Retiring, 1, 2));
      Retire (T, 1, 2, True);
      T.Issued := Natural'Last;
      Stage (T, 1, 1, OK); pragma Assert (not OK and T.Issued = Natural'Last);
   end;
   -- Resize/DPI change invalidates only staged work, retaining the current
   -- visible image and both identities. Admission waits for real retirement.
   for Left in Phase loop
      for Right in Phase loop
         for Exhausted in Boolean loop
            declare
               T : State :=
                 (Closing => False,
                  Requested => (if Exhausted then Generation'Last else 2),
                  Issued => 2,
                  Buffers =>
                    (1 => (if Left = Empty then (Empty, 0, 0) else (Left, 1, 1)),
                     2 => (if Right = Empty then (Empty, 0, 0) else (Right, 2, 2))));
               Before : constant State := T;
            begin
               if Valid (T) then
                  Configure (T, OK);
                  pragma Assert (Valid (T) and OK = not Exhausted);
                  if Exhausted then
                     pragma Assert (T = Before);
                  else
                     pragma Assert (T.Requested = 3 and T.Issued = 2);
                     for I in Slot loop
                        pragma Assert
                          (T.Buffers (I).Epoch = Before.Buffers (I).Epoch and
                           T.Buffers (I).Ticket = Before.Buffers (I).Ticket);
                        if Before.Buffers (I).Status = Candidate then
                           pragma Assert (T.Buffers (I).Status = Retiring);
                           declare
                              Pending : constant State := T;
                           begin
                              Present (T, I, 3, Before.Buffers (I).Ticket, OK);
                              pragma Assert (not OK and T = Pending);
                              Retire (T, I, Before.Buffers (I).Ticket, False);
                              pragma Assert (T = Pending);
                              Stage (T, I, 3, OK);
                              pragma Assert (not OK and T = Pending);
                              Retire (T, I, 3, True);
                              pragma Assert (T = Pending);
                              Retire (T, I, Before.Buffers (I).Ticket, True);
                              Stage (T, I, 3, OK);
                              pragma Assert (OK and T.Buffers (I) = (Candidate, 3, 3));
                           end;
                        else
                           pragma Assert (T.Buffers (I) = Before.Buffers (I));
                        end if;
                     end loop;
                  end if;
               end if;
            end;
         end loop;
      end loop;
   end loop;
   declare
      package Native_Policy renames Surface_State_Production;
      use type Native_Policy.State;
      use type Native_Policy.Buffer_Record;
      T : Native_Policy.State :=
        (Closing => False, Requested => Positive'Last - 1,
         Issued => Natural'Last - 1,
         Buffers => (1 => (Native_Policy.Visible, 1, 1),
                     2 => (Native_Policy.Candidate, Positive'Last - 1,
                           Natural'Last - 1)));
   begin
      Native_Policy.Configure (T, OK);
      pragma Assert (OK and T.Requested = Positive'Last);
      pragma Assert
        (T.Buffers (1) = (Native_Policy.Visible, 1, 1) and
         T.Buffers (2) = (Native_Policy.Retiring, Positive'Last - 1,
                         Natural'Last - 1));
      declare
         Before : constant Native_Policy.State := T;
      begin
         Native_Policy.Configure (T, OK);
         pragma Assert (not OK and T = Before);
      end;
      Native_Policy.Retire (T, 2, Natural'Last - 1, True);
      Native_Policy.Stage (T, 2, Positive'Last, OK);
      pragma Assert (OK and T.Issued = Natural'Last);
      Native_Policy.Present (T, 2, Positive'Last, Natural'Last, OK);
      pragma Assert (OK and Native_Policy.Valid (T));
      Native_Policy.Retire (T, 1, 1, True);
      declare
         Before : constant Native_Policy.State := T;
      begin
         Native_Policy.Stage (T, 1, Positive'Last, OK);
         pragma Assert (not OK and T = Before);
      end;
   end;
   Ada.Text_IO.Put_Line ("SURFACE-CONFIGURE: PASS stale candidate retirement, visible preservation and exhaustion");
   -- Every admitted pair of phases, in both retirement orders. Teardown
   -- must retain identities even for an unpublished candidate or old epoch.
   for Left in Phase loop
      for Right in Phase loop
         for First in Slot loop
            declare
               T : State :=
                 (Closing => False, Requested => 2, Issued => 2,
                  Buffers =>
                    (1 => (if Left = Empty then (Empty, 0, 0) else (Left, 1, 1)),
                     2 => (if Right = Empty then (Empty, 0, 0) else (Right, 2, 2))));
               Before : constant State := T;
               Second : constant Slot := (if First = 1 then 2 else 1);
            begin
               if Valid (T) then
                  Close (T);
                  pragma Assert (T.Closing and Valid (T));
                  for I in Slot loop
                     pragma Assert (T.Buffers (I).Ticket = Before.Buffers (I).Ticket);
                     pragma Assert (T.Buffers (I).Epoch = Before.Buffers (I).Epoch);
                     pragma Assert
                       (T.Buffers (I).Status =
                          (if Before.Buffers (I).Status = Empty then Empty else Retiring));
                  end loop;
                  declare
                     Closed : constant State := T;
                  begin
                     Close (T); pragma Assert (T = Closed);
                     Configure (T, OK); pragma Assert (not OK and T = Closed);
                     for I in Slot loop
                        Stage (T, I, 2, OK); pragma Assert (not OK and T = Closed);
                        Present (T, I, 2, T.Buffers (I).Ticket, OK);
                        pragma Assert (not OK and T = Closed);
                        Retire (T, I, T.Buffers (I).Ticket, False);
                        pragma Assert (T = Closed);
                        Retire (T, I, 3, True); pragma Assert (T = Closed);
                     end loop;
                  end;
                  Retire (T, First, T.Buffers (First).Ticket, True);
                  pragma Assert (T.Buffers (First) = (Empty, 0, 0));
                  Retire (T, Second, T.Buffers (Second).Ticket, True);
                  pragma Assert (T.Buffers (Second) = (Empty, 0, 0));
                  Configure (T, OK); pragma Assert (not OK and T.Closing);
                  Stage (T, First, 2, OK); pragma Assert (not OK);
               end if;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("SURFACE-CLOSE: PASS phase pairs, both retirement orders, stale receipts and terminal admission");
   Ada.Text_IO.Put_Line ("SURFACE-STATE: PASS 4096 replacements, stale epochs, exhaustion and retained readers");
end Surface_State_Tests;
