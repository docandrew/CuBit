with Ada.Text_IO;
with Client_Frame_State;
procedure Client_Frame_Tests is
   use Client_Frame_State;
   S : State;
begin
   for Round in Identity range 1 .. 4096 loop
      Allocate (S);
      pragma Assert (S.Mode = Writable);
      Seal (S, True); Borrow (S, Round, Round);
      declare Before : constant State := S; begin
         Retire (S, Round + 1, Round, True); pragma Assert (S = Before);
         Retire (S, Round, Round + 1, True); pragma Assert (S = Before);
         Retire (S, Round, Round, False); pragma Assert (S = Before);
      end;
      Retire (S, Round, Round, True);
      pragma Assert (S.Mode = Sealed);
      Reopen (S, True); pragma Assert (S.Mode = Writable);
      Seal (S, True); Borrow (S, Round, Round);
      Release (S, True, True); pragma Assert (S.Mode = Empty);
   end loop;
   for P in Phase loop
      for Retired in Boolean loop
         for Freed in Boolean loop
            S := (P, (if P in Held | Uncertain then 1 else 0),
                     (if P in Held | Uncertain then Identity'Last else 0));
            pragma Assert (Valid (S));
            Release (S, Retired, Freed);
            pragma Assert (Valid (S));
            pragma Assert (S.Mode = (if Retired and Freed then Empty else Uncertain));
         end loop;
      end loop;
   end loop;
   S := (Writable, 0, 0); Seal (S, False);
   pragma Assert (S.Mode = Uncertain);
   S := (Sealed, 0, 0); Reopen (S, False);
   pragma Assert (S.Mode = Uncertain);
   S := (Held, Identity'Last, Identity'Last); Quarantine (S);
   Retire (S, Identity'Last, Identity'Last, True);
   pragma Assert (S = (Uncertain, Identity'Last, Identity'Last));
   Ada.Text_IO.Put_Line ("CLIENT-FRAME: PASS 4096 cycles, mismatched receipts, protection/release failure and quarantine");
end Client_Frame_Tests;
