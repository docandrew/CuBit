--  Hosted checks for Grant_Windows: fill every window, refuse when full,
--  reuse a released window, and leave the others alone.
with Ada.Text_IO; use Ada.Text_IO;
with Grant_Windows; use Grant_Windows;

procedure Main is
   S : Window_Set := Empty;
   W : Window;
   Found : Boolean;
begin
   pragma Assert (Is_Empty (S));
   for Expected in Window loop
      Allocate (S, W, Found);
      pragma Assert (Found and then W = Expected);
   end loop;
   pragma Assert (not Is_Empty (S));
   Allocate (S, W, Found);
   pragma Assert (not Found);

   --  A released window is the one handed out next; the rest stay taken.
   Release (S, 4097);
   pragma Assert (not Contains (S, 4097) and then Contains (S, 4096) and then Contains (S, 4098));
   Allocate (S, W, Found);
   pragma Assert (Found and then W = 4097);

   for V in Window loop
      Release (S, V);
   end loop;
   pragma Assert (Is_Empty (S));
   Put_Line ("GRANT-WINDOWS: PASS");
end Main;
