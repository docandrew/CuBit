--  Hosted checks for Futex_Queues (docs/threads.md). Assertions enabled, so
--  every contract is also checked at run time. A randomized operation
--  sequence is compared against an independent reference: a FIFO list of
--  (waiter, key) pairs per bucket.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Futex_Keys; use Futex_Keys;
with Futex_Queues_Small; use Futex_Queues_Small;
with Futex_Queues_Large;
with Futex_Protocol;

procedure Main is
   Checked : Natural := 0;

   --  Deterministic xorshift generator (reproducible failures).
   Seed : Unsigned_64 := 16#2545_F491_4F6C_DD1D#;
   function Next return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next;

   --  Reference model: one FIFO list per bucket.
   Max_Ref : constant := 64;
   type Ref_Entry is record
      W : Waiter_Id;
      K : Key;
   end record;
   type Ref_List is array (1 .. Max_Ref) of Ref_Entry;
   type Ref_Bucket is record
      L : Ref_List;
      N : Natural := 0;
   end record;

   Buckets : array (Bucket_Index) of Bucket := (others => Empty_Bucket);
   Ref     : array (Bucket_Index) of Ref_Bucket;

   --  Where each waiter sleeps (the kernel's thread record).
   type Where is record
      Waiting : Boolean := False;
      B       : Bucket_Index := 0;
      S       : Slot_Index := 0;
   end record;
   Threads : array (Waiter_Id range 1 .. 200) of Where;

   Keys : constant array (1 .. 6) of Key :=
     [(1, 16#1000#), (1, 16#1004#), (2, 16#1000#), (3, 16#7FFF_FFFF_FFFC#),
      (1, 16#2000_0000#), (2, 16#2000_0004#)];

   procedure Ref_Remove (B : Bucket_Index; W : Waiter_Id) is
   begin
      for I in 1 .. Ref (B).N loop
         if Ref (B).L (I).W = W then
            for J in I .. Ref (B).N - 1 loop
               Ref (B).L (J) := Ref (B).L (J + 1);
            end loop;
            Ref (B).N := Ref (B).N - 1;
            return;
         end if;
      end loop;
      raise Program_Error with "reference lost a waiter";
   end Ref_Remove;

   function Ref_Oldest (B : Bucket_Index; K : Key) return Waiter_Id is
   begin
      for I in 1 .. Ref (B).N loop
         if Ref (B).L (I).K = K then
            return Ref (B).L (I).W;
         end if;
      end loop;
      return No_Waiter;
   end Ref_Oldest;

   procedure Check_Agrees (B : Bucket_Index) is
      Count : Natural := 0;
   begin
      pragma Assert (Well_Formed (Buckets (B)));
      for I in Slot_Index loop
         if Buckets (B).S (I).Used then
            Count := Count + 1;
         end if;
      end loop;
      pragma Assert (Count = Ref (B).N);
      for I in 1 .. Ref (B).N loop
         pragma Assert (Contains (Buckets (B), Ref (B).L (I).W));
      end loop;
   end Check_Agrees;

   Full_Seen, Timeouts, Wakes, Empty_Wakes : Natural := 0;
begin
   for Step in 1 .. 400_000 loop
      declare
         R : constant Unsigned_64 := Next;
         W : constant Waiter_Id := Waiter_Id (R mod 200 + 1);
         K : constant Key := Keys (Positive ((R / 256) mod Keys'Length + 1));
         B : constant Bucket_Index := Bucket_Of (K);
         Op : constant Unsigned_64 := (R / 65536) mod 10;
      begin
         if Op < 5 then
            --  FUTEX_WAIT by an idle thread.
            if not Threads (W).Waiting then
               declare
                  S  : Slot_Index;
                  Ok : Boolean;
               begin
                  Enqueue (Buckets (B), W, K, Buckets (B).Next_Ticket, S, Ok);
                  if Ok then
                     Threads (W) := (True, B, S);
                     Ref (B).N := Ref (B).N + 1;
                     Ref (B).L (Ref (B).N) := (W, K);
                  else
                     pragma Assert (Ref (B).N = 32);
                     Full_Seen := Full_Seen + 1;
                  end if;
                  Check_Agrees (B);
               end;
            end if;
         elsif Op < 8 then
            --  FUTEX_WAKE of one waiter: must be the reference's oldest.
            declare
               Woken    : Waiter_Id;
               From     : Slot_Index;
               Expected : constant Waiter_Id := Ref_Oldest (B, K);
            begin
               Wake_One (Buckets (B), K, Woken, From);
               pragma Assert (Woken = Expected);
               if Woken /= No_Waiter then
                  pragma Assert (Threads (Woken).Waiting and then
                                 Threads (Woken).S = From);
                  Threads (Woken).Waiting := False;
                  Ref_Remove (B, Woken);
                  Wakes := Wakes + 1;
               else
                  Empty_Wakes := Empty_Wakes + 1;
               end if;
               Check_Agrees (B);
            end;
         else
            --  Timeout or kill of a waiting thread, via its recorded slot.
            if Threads (W).Waiting then
               declare
                  Removed : Boolean;
                  WB : constant Bucket_Index := Threads (W).B;
               begin
                  Remove_At (Buckets (WB), Threads (W).S, W, Removed);
                  pragma Assert (Removed);
                  Threads (W).Waiting := False;
                  Ref_Remove (WB, W);
                  Timeouts := Timeouts + 1;
                  Check_Agrees (WB);
                  --  A stale removal (the slot no longer holds W) changes nothing.
                  Remove_At (Buckets (WB), Threads (W).S, W, Removed);
                  pragma Assert (not Removed);
               end;
            end if;
         end if;
         Checked := Checked + 1;
      end;
   end loop;

   --  The overflow instance holds a waiter for every possible thread.
   declare
      L  : Futex_Queues_Large.Bucket := Futex_Queues_Large.Empty_Bucket;
      S  : Futex_Queues_Large.Slot_Index;
      Ok : Boolean;
      Woken : Waiter_Id;
   begin
      for W in 1 .. Waiter_Id'Last loop
         Futex_Queues_Large.Enqueue (L, W, Keys (Integer (W mod 6) + 1),
                                     L.Next_Ticket, S, Ok);
         pragma Assert (Ok);
      end loop;
      Futex_Queues_Large.Wake_One (L, Keys (2), Woken, S);
      pragma Assert (Woken = 1);   --  the oldest waiter on key 2
      Checked := Checked + 1;
   end;

   --  The proved lemmas, executed.
   Prove_FIFO (Empty_Bucket, Keys (1), 5, 6);
   Prove_Key_Isolation (Empty_Bucket, Keys (1), Keys (2), 7);
   Futex_Protocol.Prove_Racing_Store_Covered (0, 1);

   pragma Assert (Full_Seen > 0 and then Timeouts > 0 and then
                  Wakes > 0 and then Empty_Wakes > 0);
   Ada.Text_IO.Put_Line
     ("futex-queues: PASS" & Checked'Image & " operations," & Wakes'Image &
      " wakes," & Timeouts'Image & " removals," & Full_Seen'Image &
      " full-bucket refusals");
end Main;
