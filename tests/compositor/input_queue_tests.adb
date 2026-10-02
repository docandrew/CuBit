with Ada.Text_IO;
with Compositor_Input_Queue; use Compositor_Input_Queue;
procedure Input_Queue_Tests is
   use type Word;
   Motion : constant Word := 1;
   Recovery : constant Event := (True, 0, 9, 42, 123, 456);
   Q : Queue;
   Next : Word := 1;
   R : Outcome;
   procedure Put (Kind, Value : Word) is
   begin Push (Q,Next,(True,0,Kind,42,Value,Value+1),Recovery,Motion,R); end;
begin
   for Cycle in 1 .. 1000 loop
      Q := (others => (others => <>)); Next := 1;
      Put (Motion, 0);
      for I in 1 .. 100 loop
         Put (Motion, Word(I));
         pragma Assert (R=Motion_Replaced and Next=2 and Q(0).Serial=1);
      end loop;
      for Kind in Word range 2 .. 8 loop
         Put (Kind,Kind);
         pragma Assert (R=Appended);
         Put (Motion,Kind);
         pragma Assert (R=Appended);
         Put (Motion,Kind+1);
         pragma Assert (R=Motion_Replaced);
      end loop;
      while Vacant(Q) /= -1 loop Put (2,1); end loop;
      Put (3,77);
      pragma Assert (R=Resynchronized and Q(0)=Numbered(Recovery,Next-1));
      pragma Assert (Vacant(Q)=1 and Newest(Q)=0);
      Put (Motion,88);
      pragma Assert (R=Appended and Q(0).Kind=9);
      declare Before : constant Queue := Q; begin
         Next := Word'Last; Put (Motion,99);
         pragma Assert (R=Exhausted and Next=Word'Last and Q=Before);
         Next := 0; Put (2,99);
         pragma Assert (R=Exhausted and Next=0 and Q=Before);
      end;
   end loop;
   -- Slot order is not serial order after older events have been dequeued.
   Q := (others => (others => <>));
   Q(0) := (True,90,2,42,1,2); Q(31) := (True,89,Motion,42,3,4);
   Next := 91; Put(Motion,5);
   pragma Assert(R=Appended and Q(1).Serial=91 and Q(31).Payload0=3);
   declare
      E : Event;
      Selected : Selection;
      Accepted : Boolean;
      Serial : Word;
   begin
      for After in Word range 0 .. 32 loop
         for I in Index loop
            Q(I) := (True,Word((I*13) mod 32+1),2,42,Word(I),0);
         end loop;
         for Expected in After+1 .. 32 loop
            Pop(Q,Expected-1,Selected,E);
            pragma Assert(Selected /= -1 and E.Valid and E.Serial=Expected);
            pragma Assert(not Has_After(Q,32));
         end loop;
         Pop(Q,32,Selected,E);
         pragma Assert(Selected = -1 and not E.Valid and not Has_After(Q,0));
      end loop;
      Next := Word'Last-1;
      Recover(Q,Next,Recovery,Accepted);
      pragma Assert(Accepted and Next=Word'Last and Q(0).Serial=Word'Last-1);
      declare Before : constant Queue := Q; begin
         Recover(Q,Next,Recovery,Accepted);
         pragma Assert(not Accepted and Q=Before and Next=Word'Last);
      end;
      Reserve(Next,Serial);
      pragma Assert(Serial=0 and Next=Word'Last);
      Next:=1; Reserve(Next,Serial);
      pragma Assert(Serial=1 and Next=2);
   end;
   Ada.Text_IO.Put_Line("INPUT-QUEUE: PASS 1000 motion/barrier/overflow/exhaustion cycles fragmented queue order, stale-ack draining and recovery reservation");
end Input_Queue_Tests;
