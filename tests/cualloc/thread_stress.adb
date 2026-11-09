pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Exceptions;
with Ada.Text_IO;
with Heap;

package body Thread_Stress is
   type Parcel is record
      Item, Size : Unsigned_64 := 0;
      Value : Storage_Element := 0;
   end record;
   QUEUE_SLOTS : constant := 4_096;
   type Parcels is array (0 .. QUEUE_SLOTS - 1) of Parcel;

   protected Queue is
      procedure Put (P : Parcel; Accepted : out Boolean);
      procedure Take (P : out Parcel; Found : out Boolean);
   private
      Items : Parcels;
      Head, Count : Natural := 0;
   end Queue;

   protected body Queue is
      procedure Put (P : Parcel; Accepted : out Boolean) is
      begin
         Accepted := Count < QUEUE_SLOTS;
         if Accepted then
            Items ((Head + Count) mod QUEUE_SLOTS) := P;
            Count := Count + 1;
         end if;
      end Put;
      procedure Take (P : out Parcel; Found : out Boolean) is
      begin
         Found := Count > 0;
         if Found then
            P := Items (Head);
            Head := (Head + 1) mod QUEUE_SLOTS;
            Count := Count - 1;
         else
            P := (others => <>);
         end if;
      end Take;
   end Queue;

   protected Flags is
      procedure Note (Corrupt, Unaligned, Refused : Boolean; Moved : Natural);
      function Get return Outcome;
   private
      State : Outcome;
   end Flags;
   protected body Flags is
      procedure Note (Corrupt, Unaligned, Refused : Boolean; Moved : Natural) is
      begin
         State.Corrupt := State.Corrupt or Corrupt;
         State.Unaligned := State.Unaligned or Unaligned;
         State.Refused := State.Refused or Refused;
         State.Blocks := State.Blocks + Moved;
      end Note;
      function Get return Outcome is (State);
   end Flags;

   function Addr (V : Unsigned_64) return System.Address is (To_Address (Integer_Address (V)));
   procedure Fill (Item, Bytes : Unsigned_64; Value : Storage_Element) is
      B : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => Addr (Item);
   begin
      B := [others => Value];
   end Fill;
   function Holds (Item, Bytes : Unsigned_64; Value : Storage_Element) return Boolean is
      B : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => Addr (Item);
   begin
      return (for all X of B => X = Value);
   end Holds;

   procedure Check_And_Free (P : Parcel; Corrupt : in out Boolean) is
   begin
      Corrupt := Corrupt or else not Holds (P.Item, P.Size, P.Value);
      Heap.Free (P.Item);
   end Check_And_Free;

   task type Worker is
      entry Start (Number, Rounds : Positive);
   end Worker;

   task body Worker is
      Id, Count : Positive;
      Seed : Unsigned_64;
      LOCAL : constant := 64;
      Own : array (1 .. LOCAL) of Parcel;
      Corrupt, Unaligned, Refused : Boolean := False;
      Moved : Natural := 0;
      function Next return Unsigned_64 is
      begin
         Seed := Seed xor Shift_Left (Seed, 13);
         Seed := Seed xor Shift_Right (Seed, 7);
         Seed := Seed xor Shift_Left (Seed, 17);
         return Seed;
      end Next;
   begin
      accept Start (Number, Rounds : Positive) do
         Id := Number;
         Count := Rounds;
      end Start;
      Seed := 16#9E37_79B9_7F4A_7C15# * Unsigned_64 (Id);
      for Round in 1 .. Count loop
         declare
            K : constant Positive := Natural (Next mod LOCAL) + 1;
            R : constant Unsigned_64 := Next;
            P : Parcel;
            Accepted, Found : Boolean;
         begin
            --  Keep a few blocks of our own (local frees) ...
            if Own (K).Item /= 0 then
               Check_And_Free (Own (K), Corrupt);
               Own (K).Item := 0;
            end if;
            P.Size := (if R mod 61 = 0 then R mod 300_000 + 1 else R mod 3_000 + 1);
            P.Value := Storage_Element ((Id * 31 + Round) mod 251);
            P.Item := Heap.Allocate (P.Size, 16);
            if P.Item = 0 then
               Refused := True;
            else
               Unaligned := Unaligned or else P.Item mod 16 /= 0;
               Fill (P.Item, P.Size, P.Value);
               --  ... and hand every other one to whoever takes it.
               if Round mod 2 = 0 then
                  Own (K) := P;
               else
                  Queue.Put (P, Accepted);
                  if not Accepted then
                     Check_And_Free (P, Corrupt);
                  end if;
               end if;
            end if;
            Queue.Take (P, Found);
            if Found then
               Check_And_Free (P, Corrupt);
               Moved := Moved + 1;
            end if;
         end;
      end loop;
      for P of Own loop
         if P.Item /= 0 then
            Check_And_Free (P, Corrupt);
         end if;
      end loop;
      Flags.Note (Corrupt, Unaligned, Refused, Moved);
   exception
      when X : others =>
         Ada.Text_IO.Put_Line ("worker:" & Ada.Exceptions.Exception_Information (X));
         Flags.Note (True, Unaligned, Refused, Moved);
   end Worker;

   procedure Run (Workers, Generations, Rounds : Positive; Result : out Outcome) is
      Arenas : Unsigned_64 := 0;
   begin
      Result.Before_Arenas := Unsigned_64 (Heap.Arena_Count);
      for Generation in 1 .. Generations loop
         declare
            Crew : array (1 .. Workers) of Worker;
         begin
            for W in Crew'Range loop
               Crew (W).Start ((Generation - 1) * Workers + W, Rounds);
            end loop;
         end;  --  every worker has ended: its heap is abandoned
         Arenas := Unsigned_64 (Heap.Arena_Count);
         if Generation = 1 then
            Result.First_Arenas := Arenas;
         end if;
      end loop;
      --  What is still queued goes back from this thread.
      declare
         P : Parcel;
         Found : Boolean := True;
         Corrupt : Boolean := False;
      begin
         while Found loop
            Queue.Take (P, Found);
            if Found then
               Check_And_Free (P, Corrupt);
            end if;
         end loop;
         Flags.Note (Corrupt, False, False, 0);
      end;
      declare
         Seen : constant Outcome := Flags.Get;
      begin
         Result.Corrupt := Seen.Corrupt;
         Result.Unaligned := Seen.Unaligned;
         Result.Refused := Seen.Refused;
         Result.Blocks := Seen.Blocks;
      end;
      Result.Last_Arenas := Unsigned_64 (Heap.Arena_Count);
   end Run;
end Thread_Stress;
