with Ada.Text_IO;
with Intel_GPU_Application_Lifetime; use Intel_GPU_Application_Lifetime;
procedure Application_Lifetime_Tests is
   Current : Phase := Empty;
begin
   for P in Phase loop
      pragma Assert (Backing_Usable (P) = (P in Offline | Preparing | Published));
      for Owner in Boolean loop
         for Active in Boolean loop
            for Pending in Boolean loop
               pragma Assert (Admission_Ready (P, Owner, Active, Pending) =
                 (P in Offline | Preparing | Published and then
                  Owner and then Active and then not Pending));
            end loop;
         end loop;
      end loop;
      for Success in Boolean loop
         pragma Assert (Allocate (P, Success) =
           (if P = Empty then (if Success then Offline else Retired) else P));
         pragma Assert (Finish_Preparation (P, Success) =
           (if P = Preparing then (if Success then Published else Retired) else P));
      end loop;
      pragma Assert (Begin_Preparation (P) = (if P = Offline then Preparing else P));
   end loop;
   Current := Allocate (Current, True);
   pragma Assert (Current = Offline);
   Current := Begin_Preparation (Current);
   pragma Assert (Current = Preparing and Backing_Usable (Current));
   Current := Finish_Preparation (Current, True);
   pragma Assert (Current = Published and Backing_Usable (Current));
   pragma Assert (Current /= Offline); -- No offline binds or second prepare.
   pragma Assert (Begin_Preparation (Current) = Published);
   Current := Retired;
   for Success in Boolean loop
      Current := Allocate (Current, Success);
      Current := Begin_Preparation (Current);
      Current := Finish_Preparation (Current, Success);
      pragma Assert (Current = Retired and not Backing_Usable (Current));
   end loop;
   pragma Assert (Finish_Preparation (Begin_Preparation (Allocate (Empty, True)), False) = Retired);
   Ada.Text_IO.Put_Line ("Application lifetime PASS: all phases/transitions, publication remains usable, retirement terminal");
end Application_Lifetime_Tests;
