with Ada.Text_IO;
with Compositor_Transition;
procedure Transition_Tests is
   package T renames Compositor_Transition;
   package D renames T.D;
   use type D.Box;
   Old, New_Bounds, Preview, Result : D.Box;
begin
   for X in 0 .. 15 loop
      for Y in 0 .. 15 loop
         Old := (X, Y, X + 80, Y + 60);
         for W in 1 .. 100 loop
            New_Bounds := (X, Y, X + W, Y + W);
            Preview := (X + 10, Y + 20, X + 110, Y + 120);
            for Has in Boolean loop
               Result := T.Cover (Old, New_Bounds, Preview, Has);
               pragma Assert (D.Contains (Result, Old));
               pragma Assert (D.Contains (Result, New_Bounds));
               pragma Assert (not Has or else D.Contains (Result, Preview));
               if not Has then
                  pragma Assert (Result = D.Envelope (Old, New_Bounds));
               end if;
            end loop;
         end loop;
      end loop;
   end loop;
   -- The last shrink outline equals the final size, but the old actual
   -- window still occupies the larger area until this release is painted.
   Old := (98,82,960,726); New_Bounds := (98,82,906,716);
   pragma Assert (T.Cover (Old,New_Bounds,New_Bounds,True)=Old);
   Old := (Natural'Last-1,Natural'Last-1,Natural'Last,Natural'Last);
   pragma Assert (T.Cover (Old,Old,Old,True)=Old);
   Ada.Text_IO.Put_Line ("TRANSITION: PASS 51200 shrink/grow/preview combinations and edge coordinates");
end Transition_Tests;
