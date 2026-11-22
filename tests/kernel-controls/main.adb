--  Hosted tests for Kernel_Controls: each sender's messages are kept apart
--  until read, repeats are the same fact, a full table says Busy (the
--  sender keeps its message), and a closed or other life takes nothing.
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with IPC_Labels; use IPC_Labels;
with Kernel_Controls; use Kernel_Controls;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   Life : constant Unsigned_64 := 4;
   T : Target_State;
   Result : Send_Result;
   Sender : Sender_Id;
   Generation : Unsigned_64;
   Kind : Control_Kind;
   Found : Boolean;
   Taken : Natural;
begin
   Send (T, Life, 2, 1, Control_Stop, Result);
   Check (Result = Not_Open, "nothing before the target opens");
   Open (T, Life);
   Send (T, Life + 1, 2, 1, Control_Stop, Result);
   Check (Result = Not_Open, "another life takes nothing");

   --  Two senders' Stops are two facts; a repeat is the same fact.
   Send (T, Life, 2, 1, Control_Stop, Result);
   Check (Result = Accepted, "parent's Stop");
   Send (T, Life, 2, 1, Control_Stop, Result);
   Check (Result = Accepted, "repeat accepted");
   Send (T, Life, 3, 7, Control_Stop, Result);
   Check (Result = Accepted, "second sender's Stop");
   Send (T, Life, 2, 1, Control_Reload, Result);
   Check (Result = Accepted, "parent's Reload");
   Taken := 0;
   loop
      Take (T, Sender, Generation, Kind, Found);
      exit when not Found;
      Taken := Taken + 1;
   end loop;
   Check (Taken = 3, "three facts: two Stops, one Reload");

   --  Every slot held by other unread senders: Busy, then room again.
   for S in 1 .. Sender_Slots loop
      Send (T, Life, Sender_Id (10 + S), 1, Control_Interrupt, Result);
      Check (Result = Accepted, "slot filled");
   end loop;
   Send (T, Life, 99, 1, Control_Stop, Result);
   Check (Result = Busy, "busy when every slot is another sender's");
   Send (T, Life, 11, 1, Control_Stop, Result);
   Check (Result = Accepted, "a sender with a slot still adds to it");
   Take (T, Sender, Generation, Kind, Found);
   Take (T, Sender, Generation, Kind, Found);
   Send (T, Life, 99, 1, Control_Stop, Result);
   Check (Result = Accepted, "room once a slot is read out");

   Close (T);
   Take (T, Sender, Generation, Kind, Found);
   Check (not Found, "a closed target's messages go with it");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("kernel-controls: PASS");
   else
      Ada.Text_IO.Put_Line ("kernel-controls: FAIL" & Failures'Image);
   end if;
end Main;
