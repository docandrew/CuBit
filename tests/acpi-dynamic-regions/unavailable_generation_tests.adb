pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with AML_Dynamic_Region_Connections;
procedure Unavailable_Generation_Tests is
   package C is new AML_Dynamic_Region_Connections (2);
   use C;
   use type Interfaces.Unsigned_8;
   Run : State;
   Ready_None : constant Ready_Set := [others => False];
   Ready_All : constant Ready_Set := [others => True];
   Accepted, Found : Boolean;
   Item : Region_Identity;
   Space : Interfaces.Unsigned_8;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "unavailable generation check" & Checks'Image; end if;
   end Check;
begin
   Publish (Run, (1, 7), 3, Accepted); Check (Accepted);
   Begin_Turn (Run, 1, 1, Accepted); Check (Accepted);
   Take_Next (Run, Ready_None, Found, Item, Space); Check (not Found);
   Check (Observe (Run, (1, 7)).Current = Pending_Connect and then
          Observe (Run, (1, 7)).Attempt_Generation = 0);
   Begin_Turn (Run, 2, 1, Accepted); Check (Accepted);
   Take_Next (Run, Ready_All, Found, Item, Space);
   Check (Found and then Item = Region_Identity'(1, 7) and then Space = 3);
   Finish_Connect (Run, Item, 1, True, Accepted);
   Check (Accepted and then Observe (Run, Item).Current = Connected);
   Publish (Run, (2, 9), 3, Accepted); Check (Accepted);
   Begin_Turn (Run, 3, 1, Accepted); Check (Accepted);
   Take_Next (Run, Ready_All, Found, Item, Space);
   Check (Found and then Item = Region_Identity'(2, 9));
   Finish_Connect (Run, Item, 1, False, Accepted);
   Check (Accepted and then Observe (Run, Item).Current = Connect_Failed);
   Begin_Turn (Run, 4, 1, Accepted); Check (Accepted);
   Take_Next (Run, Ready_All, Found, Item, Space); Check (not Found);
   Begin_Turn (Run, 5, 2, Accepted); Check (Accepted);
   Take_Next (Run, Ready_All, Found, Item, Space);
   Check (Found and then Item = Region_Identity'(2, 9));
   Ada.Text_IO.Put_Line ("unavailable generation checks" & Checks'Image);
end Unavailable_Generation_Tests;
