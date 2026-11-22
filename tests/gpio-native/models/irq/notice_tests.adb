pragma Ada_2022;
with Ada.Text_IO; with GPIO_IRQ_Notices; with CuBit.Process_IDs;
procedure Notice_Tests is
   package N renames GPIO_IRQ_Notices;
   package P renames CuBit.Process_IDs;
   type Publishers is array (Positive range <>) of P.Process_ID;
   Identities : constant Publishers := [P.No_Process, P.From_Word (16#0100_0001#),
      P.From_Word (16#FFFF_FFFF_0100_0001#), P.From_Word (16#FFFF_FFFF_FFFF_FFFF#)];
   use type P.Process_ID;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   OK, Pending : Boolean;
begin
   for Present in Boolean loop
      for Publisher of Identities loop
         for Doorbell in Boolean loop
            for Frame in Boolean loop
               declare S : N.State;
                  Expected : constant Boolean := Present and Publisher = P.No_Process and Doorbell and Frame;
               begin
                  N.Configure (S, Present, OK); Check (OK);
                  N.Configure (S, not Present, OK); Check (not OK and (N.Parent_Present (S) = Present));
                  N.Note (S, Publisher, Doorbell, Frame, OK); Check (OK = Expected);
                  Check (N.Requests_Held (S) = Expected);
                  N.Take (S, Pending); Check (Pending = Expected);
                  Check (N.Requests_Held (S) = Expected);
                  N.Take (S, Pending); Check (not Pending);
                  N.Hold (S, False); Check (not N.Requests_Held (S));
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   declare S : N.State; begin
      N.Note (S, P.No_Process, True, True, OK); Check (not OK and not N.Requests_Held (S));
      N.Configure (S, True, OK); Check (OK);
      N.Note (S, P.No_Process, True, True, OK); Check (OK);
      N.Hold (S, False); Check (N.Requests_Held (S));
      N.Take (S, Pending); Check (Pending and N.Requests_Held (S));
      N.Note (S, P.No_Process, True, True, OK); Check (OK);
      N.Hold (S, False); Check (N.Requests_Held (S));
      N.Take (S, Pending); N.Hold (S, False); Check (Pending and not N.Requests_Held (S));
   end;
   Ada.Text_IO.Put_Line ("GPIO IRQ notice checks" & Checks'Image);
end Notice_Tests;
