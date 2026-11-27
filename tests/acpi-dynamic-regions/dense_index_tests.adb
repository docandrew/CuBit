pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with AML_Dynamic_Region_Connections;
procedure Dense_Index_Tests is
   package C is new AML_Dynamic_Region_Connections (4);
   use C;
   Run : State;
   Accepted, Found : Boolean;
   Item : Region_Identity;
   Space : Interfaces.Unsigned_8;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "dense index check" & Checks'Image; end if;
   end Check;
begin
   Check (Active_Region_Count (Run) = 0);
   Check (Pending_Region_Count (Run) = 0);
   for N in Region_Node range 1 .. 3 loop
      Publish (Run, (N, 7), Interfaces.Unsigned_8 (N), Accepted);
      Check (Accepted and then Active_Region_Count (Run) = N);
      Check (Active_Region_Node (Run, N) = N);
      Check (Pending_Region_Count (Run) = N
        and then Pending_Region_Node (Run, N) = N);
   end loop;
   Begin_Turn (Run, 1, 1, Accepted); Check (Accepted);
   Next_Candidate (Run, Found, Item, Space);
   Check (Found and then Item = Region_Identity'(1, 7));
   Claim_Candidate (Run, Item, Accepted); Check (Accepted);
   Finish_Connect (Run, Item, 1, True, Accepted); Check (Accepted);
   Check (Pending_Region_Count (Run) = 2);
   Retire (Run, (2, 7), Accepted);
   Check (Accepted and then Active_Region_Count (Run) = 2);
   Check (Active_Region_Node (Run, 1) = 1 and then Active_Region_Node (Run, 2) = 3);
   Check (Pending_Region_Count (Run) = 1
     and then Pending_Region_Node (Run, 1) = 3);
   Next_Candidate (Run, Found, Item, Space);
   Check (Found and then Item = Region_Identity'(3, 7));
   Claim_Candidate (Run, Item, Accepted); Check (Accepted);
   Finish_Connect (Run, Item, 1, True, Accepted); Check (Accepted);
   Check (Pending_Region_Count (Run) = 0);
   Retire_Method_Local (Run, Item, Accepted);
   Check (Accepted and then Active_Region_Count (Run) = 1);
   Publish (Run, (3, 8), 3, Accepted);
   Check (Accepted and then Active_Region_Count (Run) = 2);
   Check (Pending_Region_Count (Run) = 1
     and then Pending_Region_Node (Run, 1) = 3);
   Next_Candidate (Run, Found, Item, Space);
   Check (Found and then Item = Region_Identity'(3, 8));
   Claim_Candidate (Run, Item, Accepted); Check (Accepted);
   Finish_Connect (Run, Item, 1, True, Accepted); Check (Accepted);
   Check (Pending_Region_Count (Run) = 0);
   Retire_Method_Local (Run, Item, Accepted);
   Check (Accepted and then Active_Region_Count (Run) = 1);
   Begin_Disconnect (Run, (1, 7), Accepted); Check (Accepted);
   Finish_Disconnect (Run, (1, 7), True, Accepted); Check (Accepted);
   Check (Pending_Region_Count (Run) = 1
     and then Pending_Region_Node (Run, 1) = 1);
   Retire (Run, (1, 7), Accepted);
   Check (Accepted and then Active_Region_Count (Run) = 0);
   Check (Pending_Region_Count (Run) = 0);
   Begin_Turn (Run, 2, 2, Accepted); Check (Accepted);
   Next_Candidate (Run, Found, Item, Space); Check (not Found);
   Ada.Text_IO.Put_Line ("dense index checks" & Checks'Image);
end Dense_Index_Tests;
