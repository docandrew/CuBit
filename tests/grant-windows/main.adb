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

   --  Runs: consecutive free windows, the first such run; a hole too small
   --  for the run is skipped; release frees exactly the run.
   Allocate_Run (S, 3, W, Found);
   pragma Assert (Found and then W = 0 and then Contains (S, 2) and then not Contains (S, 3));
   Allocate (S, W, Found);
   pragma Assert (Found and then W = 3);
   Release_Run (S, 0, 3);
   pragma Assert (not Contains (S, 0) and then not Contains (S, 2) and then Contains (S, 3));
   Allocate_Run (S, 4, W, Found);
   pragma Assert (Found and then W = 4);
   Allocate_Run (S, 3, W, Found);
   pragma Assert (Found and then W = 0);
   Allocate_Run (S, 1, W, Found);
   pragma Assert (Found and then W = 8);
   Release_Run (S, 0, 3);
   Release_Run (S, 4, 4);
   Release (S, 3);
   Release (S, 8);
   pragma Assert (Is_Empty (S));
   --  The whole set as one run, then nothing more.
   Allocate_Run (S, Run_Length'Last, W, Found);
   pragma Assert (Found and then W = 0 and then Contains (S, Window'Last));
   Allocate_Run (S, 1, W, Found);
   pragma Assert (not Found);
   Release_Run (S, 0, Run_Length'Last);
   pragma Assert (Is_Empty (S));
   --  A run that would cross the end is refused, even with free windows.
   Allocate (S, W, Found);
   Allocate_Run (S, Run_Length'Last, W, Found);
   pragma Assert (not Found);
   Put_Line ("GRANT-WINDOWS: PASS");
end Main;
