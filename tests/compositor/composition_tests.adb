with Ada.Text_IO;
with Desktop_Composition; use Desktop_Composition;
with Compositor_Policy;
procedure Composition_Tests is
   Cases : Natural := 0;
begin
   declare
      use Compositor_Policy;
      S : State;
   begin
      for Opt_In in Boolean loop
         for Initialized in Boolean loop
            S := Initial (Opt_In, Initialized);
            pragma Assert ((S = Ready) = (Opt_In and Initialized));
         end loop;
      end loop;
      for Result in Completion loop
         S := Initial (True, True);
         Begin_Draw (S);
         pragma Assert (not May_Retire (S) and not May_Fallback (S));
         Finish_Draw (S, Result);
         pragma Assert (May_Retire (S) = (Result /= Access_Unknown));
         pragma Assert (May_Fallback (S) =
           (Result in Rejected | Failed_Quiescent));
      end loop;
   end;
   for X in 0 .. 10 loop
      for Y in 0 .. 10 loop
         for W in 0 .. 10 loop
            for Clipped in Boolean loop
               declare
                  P : constant Blit_Plan := Plan
                    (8, 8, 6, 5, (X, Y, W, 7), Clipped, (2, 3, 4, 4));
               begin
                  for PY in 0 .. 7 loop
                     for PX in 0 .. 7 loop
                        pragma Assert
                          ((PX >= P.Target_X and PX < P.Target_X + P.Width and
                            PY >= P.Target_Y and PY < P.Target_Y + P.Height) =
                           (PX >= X and PX < X + Natural'Min (W, 6) and
                            PY >= Y and PY < Y + 5 and
                            (not Clipped or else
                               (PX >= 2 and PX < 6 and PY >= 3 and PY < 7))));
                     end loop;
                  end loop;
                  Cases := Cases + 1;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   declare
      P : constant Blit_Plan := Plan
        (Natural'Last, Natural'Last, Natural'Last, Natural'Last,
         (Natural'Last - 3, Natural'Last - 2, 10, 10), True,
         (Natural'Last - 1, Natural'Last - 1, 10, 10));
   begin
      pragma Assert (P.Width = 1 and P.Height = 1);
      pragma Assert (P.Source_X = 2 and P.Source_Y = 1);
   end;
   Ada.Text_IO.Put_Line ("COMPOSITION: PASS" & Cases'Image & " pixel-membership cases");
end Composition_Tests;
