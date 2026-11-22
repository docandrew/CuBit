--  Hosted tests for Kernel_Credits: a flooder fills only its own ring and
--  is then Busy while others are still admitted into entries of their own;
--  Take is round-robin and keeps each sender's order; a forgotten sender's
--  ring goes; a later life is not mixed with an earlier one's messages.
pragma Ada_2022;
with Ada.Text_IO;
with Kernel_Credits; use Kernel_Credits;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   Each : constant := 4;
   R : Receiver_State;
   Result : Admit_Result;
   At_Position : Position;
   From : Sender;
   Found : Boolean;
   Flooder : constant Sender := 7;
   Client  : constant Sender := 9;
   Used : array (0 .. Maximum_Senders * Each) of Boolean := [others => False];
   Served_Client : Boolean := False;
begin
   Initialize (R, Each);
   Check (Empty (R), "starts empty");

   for N in 1 .. Each loop
      Admit (R, Flooder, 1, At_Position, Result);
      Check (Result = Admitted, "flooder within credit");
      Check (not Used (Entry_Of (R, Flooder, At_Position)), "a free entry");
      Used (Entry_Of (R, Flooder, At_Position)) := True;
   end loop;
   Admit (R, Flooder, 1, At_Position, Result);
   Check (Result = Busy, "flooder at credit: Busy");
   Admit (R, Client, 1, At_Position, Result);
   Check (Result = Admitted, "client admitted despite the flood");
   Check (not Used (Entry_Of (R, Client, At_Position)), "client's entry is its own");

   --  Round-robin: the client is served within one turn of the flooder.
   for N in 1 .. 2 loop
      Take (R, From, At_Position, Found);
      Check (Found, "something to take");
      Served_Client := Served_Client or else From = Client;
   end loop;
   Check (Served_Client, "client served within one turn");

   --  The flooder's messages come out in the order they went in.
   declare
      Expected : Position := 1;
   begin
      loop
         Take (R, From, At_Position, Found);
         exit when not Found;
         Check (From = Flooder and then At_Position = Expected, "flooder's order kept");
         Expected := Expected + 1;
      end loop;
   end;
   Check (Empty (R), "empty again");

   --  A forgotten sender's ring goes; a later life is not mixed in.
   Admit (R, Client, 1, At_Position, Result);
   Admit (R, Client, 2, At_Position, Result);
   Check (Result = Busy, "a later life waits for the earlier one's ring");
   Forget (R, Client);
   Check (Empty (R), "forgotten");
   Admit (R, Client, 2, At_Position, Result);
   Check (Result = Admitted, "the later life starts afresh");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("kernel-credits: PASS");
   else
      Ada.Text_IO.Put_Line ("kernel-credits: FAIL" & Failures'Image);
   end if;
end Main;
