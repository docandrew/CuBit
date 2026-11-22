-- Regression test for laptop image 11 (micro ffcf9fa11): launching
-- teapots.app while desktop.svc allocated made intel-gpu answer six
-- Creates "allocation unavailable reason=APPLICATION_PENDING", and
-- desktop.svc's renderer exited. The buffer service allocates one buffer
-- at a time; a Create that meets an allocation in flight must wait for the
-- allocator (Intel_GPU_Allocation_Waiters), not be refused.
--
-- The dispatcher below follows intel-gpu main.adb's rule exactly
-- (Must_Wait / Ready / Take / Restore around the real
-- Intel_GPU_Buffer_Requests service); replies are modeled tokens, so the
-- test sees every reply delivered exactly once.
--
-- Run with --without-waiters to dispatch as the driver did before the fix
-- (every Create straight to the service): the two-client scenario fails.
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Allocation_Waiters;
with Intel_GPU_Waiter_Ring;
procedure Allocation_Waiters_Tests is
   package Layout renames Intel_GPU_Buffer_Backing;
   Page_Bytes : constant := 4096;
   Without_Waiters : constant Boolean :=
     Ada.Command_Line.Argument_Count = 1 and then
     Ada.Command_Line.Argument (1) = "--without-waiters";

   -- Clients: the desktop and teapots, each its own admitted session.
   Desktop_Sender : constant Unsigned_64 := 101;
   Teapots_Sender : constant Unsigned_64 := 202;
   Desktop_Session : constant Unsigned_64 := 7;
   Teapots_Session : constant Unsigned_64 := 9;
   Teapots_Admitted : Boolean := True;
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = Desktop_Sender and then Stamp = Desktop_Session then Desktop_Session
      elsif Sender = Teapots_Sender and then Stamp = Teapots_Session and then Teapots_Admitted
      then Teapots_Session else 0);
   function Owner_Ready return Boolean is (True);
   package Buffers is new Intel_GPU_Buffer_Requests (Session_Of, Owner_Ready);
   use type Buffers.Ticket;

   -- A modeled saved reply: a token naming the client call it answers.
   subtype Call_Number is Natural range 0 .. 64;
   No_Call : constant Call_Number := 0;
   type Token is limited record
      Call : Call_Number := No_Call;
   end record;
   function Held (Item : Token) return Boolean is (Item.Call /= No_Call);
   procedure Move (From, Into : in out Token) is
   begin
      if Held (Into) then return; end if;
      Into.Call := From.Call;
      From.Call := No_Call;
   end Move;

   type Create_Request is record
      Sender, Stamp, Bytes : Unsigned_64 := 0;
   end record;
   Initial_Waiters : constant := 1;
   Driver_Ceiling : constant := 16;
   package Waiters is new Intel_GPU_Allocation_Waiters
     (Create_Request, Token, Held, Move,
      Initial_Size => Initial_Waiters, Ceiling => Driver_Ceiling);

   -- Every reply a client received, by call number, and in which order.
   type Reply_Record is record
      Answered : Natural := 0;
      Status : Unsigned_64 := 0;
      Order : Natural := 0;
   end record;
   Replies : array (Call_Number range 1 .. Call_Number'Last) of Reply_Record;
   Delivered : Natural := 0;
   procedure Answer (Owed : in out Token; Status : Unsigned_64) is
   begin
      pragma Assert (Held (Owed), "a reply was lost or answered twice");
      Delivered := Delivered + 1;
      Replies (Owed.Call).Answered := Replies (Owed.Call).Answered + 1;
      Replies (Owed.Call).Status := Status;
      Replies (Owed.Call).Order := Delivered;
      Owed.Call := No_Call;
   end Answer;

   Object : Buffers.Service;
   Queue : Waiters.Queue;
   In_Flight : Buffers.Ticket := 0;
   In_Flight_Reply : Token;
   In_Flight_Bytes : Unsigned_64 := 0;
   Dispatching_Oldest : Boolean := False;

   function Allocator_Busy return Boolean is
     (Buffers.Allocation_In_Flight (Object) or else Held (In_Flight_Reply));

   -- main.adb Handle_Application_Buffer for a Create.
   procedure Dispatch (Item : Create_Request; Owed : in out Token) is
      Response : Buffers.Words;
      Deferred : Buffers.Ticket;
      Result : Waiters.Wait_Result;
      use type Waiters.Wait_Result;
   begin
      if not Without_Waiters and then
        Waiters.Must_Wait (Queue, Allocator_Busy, Dispatching_Oldest)
      then
         if Dispatching_Oldest then
            Waiters.Restore (Queue, Item, Owed, Result);
         else
            Waiters.Append (Queue, Item, Owed, Result);
         end if;
         Dispatching_Oldest := False;
         if Result /= Waiters.Waiting then Answer (Owed, Buffers.Unavailable); end if;
         return;
      end if;
      Dispatching_Oldest := False;
      Buffers.Handle (Object, Item.Sender, Item.Stamp, Buffers.Label, 4, 0, 0,
                      [Buffers.Version, Buffers.Create, Item.Bytes, 0], Response, Deferred);
      if Deferred /= 0 then
         In_Flight := Deferred;
         In_Flight_Bytes := Item.Bytes;
         Move (Owed, In_Flight_Reply);
      else
         Answer (Owed, Response (0));
      end if;
   end Dispatch;

   procedure Arrive (Sender, Stamp : Unsigned_64; Call : Call_Number;
                     Bytes : Unsigned_64 := Page_Bytes) is
      Owed : Token := (Call => Call);
   begin
      Dispatch ((Sender, Stamp, Bytes), Owed);
      pragma Assert (not Held (Owed));
   end Arrive;

   -- devmgr's backing arrives; main.adb Finish_Application_Buffer.
   procedure Complete_In_Flight is
      Offset : constant Unsigned_64 :=
        Unsigned_64 (Buffers.Ticket_Slot (In_Flight) - 1) * Page_Bytes;
      Response : Buffers.Words;
      Consumed : Boolean;
   begin
      Buffers.Complete
        (Object, In_Flight,
         Intel_GPU_Buffer_Reply.From_Linear
           (16#1000_0000# + Offset, Layout.CPU_Base + Offset, In_Flight_Bytes, 16#1000_0000#),
         Response, Consumed);
      pragma Assert (Consumed);
      In_Flight := 0;
      Answer (In_Flight_Reply, Response (0));
   end Complete_In_Flight;

   -- main.adb's loop: the oldest waiter goes once the allocator is idle.
   procedure Drain is
      Owed : Token;
      Item : Create_Request;
      Found : Boolean;
   begin
      while Waiters.Ready (Queue, Allocator_Busy) loop
         Waiters.Take (Queue, Item, Owed, Found);
         pragma Assert (Found);
         Dispatching_Oldest := True;
         Dispatch (Item, Owed);
      end loop;
   end Drain;

   procedure Run_Until_Idle is
   begin
      loop
         Drain;
         exit when In_Flight = 0;
         Complete_In_Flight;
      end loop;
   end Run_Until_Idle;
begin
   -- 1. Laptop image 11: desktop.svc allocates; teapots.app starts and
   --    allocates six buffers; the desktop asks again. Every Create is
   --    answered OK, exactly once, in arrival order.
   Arrive (Desktop_Sender, Desktop_Session, 1);
   pragma Assert (Allocator_Busy);
   for Call in Call_Number range 2 .. 7 loop
      Arrive (Teapots_Sender, Teapots_Session, Call);
   end loop;
   Arrive (Desktop_Sender, Desktop_Session, 8);
   if not Without_Waiters then
      pragma Assert (Waiters.Length (Queue) = 7 and then Delivered = 0,
                     "Creates met with a busy allocator must wait, not be refused");
   end if;
   Run_Until_Idle;
   for Call in Call_Number range 1 .. 8 loop
      if Replies (Call).Status /= Buffers.OK then
         Ada.Text_IO.Put_Line ("FAIL: call" & Call'Image & " answered status" &
           Replies (Call).Status'Image &
           (if Replies (Call).Status = Buffers.Unavailable
            then " (Unavailable: refused while another allocation was in flight)" else ""));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;
   end loop;
   for Call in Call_Number range 1 .. 8 loop
      pragma Assert (Replies (Call).Answered = 1 and then Replies (Call).Order = Natural (Call),
                     "every Create answered once, first come first served");
   end loop;
   pragma Assert (Waiters.Length (Queue) = 0 and then Waiters.Peak (Queue) = 7);
   Ada.Text_IO.Put_Line ("Two clients PASS: 8 concurrent Creates, all OK, in arrival order");

   -- 2. A newcomer never passes older waiters, even when it arrives the
   --    moment the allocator goes idle.
   Arrive (Desktop_Sender, Desktop_Session, 9);
   Arrive (Teapots_Sender, Teapots_Session, 10);
   Complete_In_Flight;                        -- allocator idle, 10 still waits
   Arrive (Desktop_Sender, Desktop_Session, 11);
   pragma Assert (Waiters.Length (Queue) = 2 and then Replies (11).Answered = 0);
   Run_Until_Idle;
   pragma Assert (Replies (10).Order < Replies (11).Order and then
                  Replies (10).Status = Buffers.OK and then Replies (11).Status = Buffers.OK);
   Ada.Text_IO.Put_Line ("Arrival order PASS: a newcomer waits behind older waiters");

   -- 3. The client went away while waiting: its Create is still answered
   --    (Denied, by the service), never dropped; the next one is served.
   Arrive (Desktop_Sender, Desktop_Session, 12);
   Arrive (Teapots_Sender, Teapots_Session, 13);
   Arrive (Desktop_Sender, Desktop_Session, 14);
   Teapots_Admitted := False;
   Run_Until_Idle;
   pragma Assert (Replies (13).Answered = 1 and then Replies (13).Status = Buffers.Denied);
   pragma Assert (Replies (14).Answered = 1 and then Replies (14).Status = Buffers.OK);
   Ada.Text_IO.Put_Line ("Departed client PASS: its waiting Create answered Denied, next served");

   -- 4. Queue mechanics against a reference model: growth from one slot by
   --    doubling, wrap-around, Restore at the head, and the named ceiling
   --    (the excess is refused with its reply left to the caller).
   declare
      package Small is new Intel_GPU_Allocation_Waiters
        (Create_Request, Token, Held, Move, Initial_Size => 1, Ceiling => 5);
      Q : Small.Queue;
      Model : array (1 .. 64) of Call_Number;
      First, Last : Natural := 1;                -- model holds First .. Last - 1
      Next_Call : Call_Number := 20;
      Result : Small.Wait_Result;
      Owed : Token;
      Item : Create_Request;
      Found : Boolean;
      use type Small.Wait_Result;
      procedure Push is
      begin
         Owed.Call := Next_Call;
         Small.Append (Q, (Unsigned_64 (Next_Call), 0, 0), Owed, Result);
         if Last - First < 5 then
            pragma Assert (Result = Small.Waiting and then not Held (Owed));
            Model (Last) := Next_Call; Last := Last + 1;
         else
            pragma Assert (Result = Small.At_Ceiling and then Held (Owed),
                           "at the ceiling the caller keeps the reply to answer it");
            Owed.Call := No_Call;
         end if;
         Next_Call := Next_Call + 1;
      end Push;
      procedure Pop is
      begin
         Small.Take (Q, Item, Owed, Found);
         pragma Assert (Found = (Last > First));
         if Found then
            pragma Assert (Owed.Call = Model (First) and then
                           Item.Sender = Unsigned_64 (Model (First)));
            First := First + 1;
            Owed.Call := No_Call;
         end if;
      end Pop;
   begin
      Pop;                                      -- empty: nothing found
      Push; Push; Pop; Push; Push; Push; Pop;   -- grows 1 -> 2 -> 4, wraps
      Push; Push; Push;                         -- grows to the ceiling 5; one refused
      pragma Assert (Small.Length (Q) = 5);
      -- A taken waiter that finds the allocator busy keeps its place.
      Small.Take (Q, Item, Owed, Found);
      pragma Assert (Found and then Owed.Call = Model (First));
      Small.Restore (Q, Item, Owed, Result);
      pragma Assert (Result = Small.Waiting and then not Held (Owed) and then Small.Length (Q) = 5);
      while Last > First loop Pop; end loop;
      Pop;
      pragma Assert (Small.Length (Q) = 0 and then Small.Peak (Q) = 5);
   end;
   pragma Assert (Intel_GPU_Waiter_Ring.Grown (3, 5) = 5 and then
                  Intel_GPU_Waiter_Ring.Grown (2, 5) = 4 and then
                  Intel_GPU_Waiter_Ring.Before (0, 4) = 3);
   Ada.Text_IO.Put_Line ("Queue PASS: growth, wrap-around, head restore and ceiling match the model");
end Allocation_Waiters_Tests;
