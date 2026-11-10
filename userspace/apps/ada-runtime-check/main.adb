------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ada language-feature regression checks for the user GNAT runtime
--  (RT-008; headless --test ada-runtime): tagged types, finalization and
--  storage pools (RT-008), and exceptions (EXC-001), each as upstream GNAT
--  15 implements them.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Messages;
with Check_Results; use Check_Results;
with Exception_Checks;
with Exception_Threads;
with Finalization_Checks;
with Pool_Checks;
with Tagged_Checks;

procedure Main is
begin
   Tagged_Checks.Run;
   Finalization_Checks.Run;
   Pool_Checks.Run;
   Exception_Checks.Run;
   Exception_Threads.Run;

   if Failures = 0 then
      CuBit.Messages.debugPrint ("TEST: PASS ada-runtime" & ASCII.LF);
   else
      CuBit.Messages.debugPrint ("TEST: FAIL ada-runtime" & ASCII.LF);
   end if;
end Main;
