with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Closed_Tables;
with Intel_GPU_Buffer_Reply;
procedure Session_Retire_Steps_Tests is
   function Owner return Boolean is (True);
   function Resolve (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 then Stamp else 0);
   package P is new Intel_GPU_Buffer_Requests (Resolve, Owner);
   use type P.Words;
   package T is new P.Closed_Tables;
   Pool, Other : P.Service;
   State : P.Session_Retirement;
   type Storage is array (Natural range 0 .. 8191) of Unsigned_64;
   Memory : Storage := [others => 0] with Alignment => 4096;
   Handle_Memory : Storage := [others => 0] with Alignment => 4096;
   IDs : array (1 .. 65) of P.Ticket;
   Pending, Rejected : P.Ticket;
   Reply : P.Words;
   OK, Done, Consumed : Boolean;
begin
   P.Configure_Client_Budgets (Pool, 512 * 1024, OK); pragma Assert (OK);
   P.Extend_Tickets (Pool, Unsigned_64 (To_Integer (Memory'Address)), 65536, OK);
   pragma Assert (OK);
   P.Extend_Handles (Pool, Unsigned_64 (To_Integer (Handle_Memory'Address)), 65536, OK);
   pragma Assert (OK);
   P.Admit_Slots (Pool, 128, (others => 128), OK); pragma Assert (OK);
   for I in IDs'Range loop
      P.Reserve_Private (Pool, (if I mod 2 = 0 then 100 else 99), IDs (I), True, Pages => 1);
      pragma Assert (IDs (I) /= 0);
      P.Finish_Private (Pool, IDs (I), OK); pragma Assert (OK);
   end loop;
   P.Handle (Pool, 42, 99, P.Label, 4, 0, 0, [1, P.Create, 4096, 0], Reply, Pending);
   pragma Assert (Pending /= 0);
   P.Begin_Retire_Session (Pool, 99, State, OK); pragma Assert (OK);
   pragma Assert (P.Client_Usage (Pool, 99).Closed);
   P.Begin_Retire_Session (Pool, 100, State, OK); pragma Assert (not OK);
   P.Retire_Session_Step (Other, State, Done); pragma Assert (not Done);
   P.Complete (Pool, Pending, Intel_GPU_Buffer_Reply.From_Linear
     (16#20000000#, Intel_GPU_Buffer_Reply.Layout.CPU_Base, 4096, 16#20000000#), Reply, Consumed);
   pragma Assert (Consumed and Reply (0) = P.Denied);
   P.Handle (Pool, 42, 99, P.Label, 4, 0, 0, [1, P.Create, 4096, 0], Reply, Rejected);
   pragma Assert (Rejected = 0);
   P.Retire_Session_Step (Pool, State, Done); pragma Assert (not Done); -- names phase
   for Turn in 1 .. 3 loop
      P.Retire_Session_Step (Pool, State, Done);
      pragma Assert (Done = (Turn = 3));
      for I in IDs'Range loop
         pragma Assert (T.Can_Retire (Pool, 99, IDs (I)) =
           (I mod 2 = 1 and I <= 32 * Turn));
         if I mod 2 = 0 then
            pragma Assert (P.Is_Table_Allocation (Pool, 100, IDs (I), P.Replacement_Tables));
         end if;
      end loop;
   end loop;
   pragma Assert (P.Client_Usage (Pool, 99).Charged = 34 * 4096);
   pragma Assert (P.Client_Usage (Pool, 100).Charged = 32 * 4096);
   P.Retire_Session_Step (Pool, State, Done); pragma Assert (Done);
   declare
      Names : P.Service;
      Sweep : P.Session_Retirement;
      Ticket_Memory, Name_Memory : Storage := [others => 0] with Alignment => 4096;
      Tickets : array (1 .. 65) of P.Ticket;
      In_Flight : P.Ticket;
   begin
      P.Configure_Client_Budgets (Names, 512 * 1024, OK); pragma Assert (OK);
      P.Extend_Tickets (Names, Unsigned_64 (To_Integer (Ticket_Memory'Address)), 65536, OK);
      pragma Assert (OK);
      P.Extend_Handles (Names, Unsigned_64 (To_Integer (Name_Memory'Address)), 65536, OK);
      pragma Assert (OK);
      P.Admit_Slots (Names, 128, (others => 128), OK); pragma Assert (OK);
      for I in Tickets'Range loop
         P.Handle (Names, 42, 99, P.Label, 4, 0, 0,
           [1, P.Create, 4096, 0], Reply, Tickets (I));
         pragma Assert (Tickets (I) /= 0);
         declare Offset : constant Unsigned_64 := Unsigned_64 (I) * 4096; begin
            P.Complete (Names, Tickets (I), Intel_GPU_Buffer_Reply.From_Linear
              (16#30000000# + Offset, Intel_GPU_Buffer_Reply.Layout.CPU_Base + Offset,
               4096, 16#30000000#), Reply, Consumed);
         end;
         pragma Assert (Consumed and Reply (0) = P.OK);
      end loop;
      P.Handle (Names, 42, 99, P.Label, 4, 0, 0,
        [1, P.Create, 4096, 0], Reply, In_Flight);
      pragma Assert (In_Flight /= 0);
      P.Begin_Retire_Session (Names, 99, Sweep, OK); pragma Assert (OK);
      P.Complete (Names, In_Flight, Intel_GPU_Buffer_Reply.From_Linear
        (16#40000000#, Intel_GPU_Buffer_Reply.Layout.CPU_Base, 4096, 16#40000000#),
        Reply, Consumed);
      pragma Assert (Consumed and Reply = [P.Denied, P.Version, 0, 0]);
      -- Cancellation publishes nothing and must not close any name ahead
      -- of the bounded sweep, even when valid backing completes late.
      for ID of Tickets loop
         pragma Assert (not P.Can_Retire (Names, 99, ID));
      end loop;
      for Turn in 1 .. 3 loop
         P.Retire_Session_Step (Names, Sweep, Done); pragma Assert (not Done);
         for I in Tickets'Range loop
            pragma Assert (P.Can_Retire (Names, 99, Tickets (I)) = (I <= Turn * 32));
         end loop;
      end loop;
      for Turn in 1 .. 3 loop
         P.Retire_Session_Step (Names, Sweep, Done);
         pragma Assert (Done = (Turn = 3));
      end loop;
      pragma Assert (P.Client_Usage (Names, 99).Charged = 66 * 4096);
      pragma Assert (not P.Can_Retire (Names, 99, In_Flight));
   end;
   Ada.Text_IO.Put_Line ("Session retirement steps PASS: bounded names/records, late cancellation, retained charges and other session");
end Session_Retire_Steps_Tests;
