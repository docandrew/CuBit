with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Deferred_Retirement; use Intel_GPU_Deferred_Retirement;
with Intel_GPU_Buffer_Backing;
with System.Storage_Elements; use System.Storage_Elements;
procedure Deferred_Retirement_Tests is
   Object : Queue;
   Calls : Natural := 0;
   Expected : Candidate;
   Expected_Index : Slot := 1;
   Decision : Outcome := Waiting;
   function Attempt (Index : Slot; Item : Candidate) return Outcome is
   begin
      Calls := Calls + 1;
      pragma Assert (Index = Expected_Index and Item = Expected);
      return Decision;
   end Attempt;
   procedure Poll is new Intel_GPU_Deferred_Retirement.Poll (Attempt);
   procedure Sweep is
   begin
      Poll (Object); -- A sole pending item is visited immediately, regardless of capacity.
   end Sweep;
begin
   Sweep; pragma Assert (Calls = 0);
   for Generation in Unsigned_64 range 1 .. 128 loop
      for Index in 1 .. Capacity (Object) loop
         Expected_Index := Index;
         Expected := ((Generation - 1) * Intel_GPU_Buffer_Backing.Ticket_Stride + Unsigned_64 (Index),
                      123, 456, 789, Generation * 16 + Unsigned_64 (Index));
         Remember (Object, Index, Expected);
         -- Duplicate or stale notifications cannot replace saved authority.
         declare Forged : Candidate := Expected; begin
            Forged.Stamp := 999;
            Remember (Object, Index, Forged);
            if Generation > 1 then
               Forged.Ticket := Forged.Ticket - Intel_GPU_Buffer_Backing.Ticket_Stride;
               Remember (Object, Index, Forged);
            end if;
         end;
         Calls := 0; Decision := Waiting;
         Sweep; pragma Assert (Calls = 1);
         Sweep; pragma Assert (Calls = 2);
         Decision := (if Index mod 2 = 0 then Discarded else Submitted);
         Sweep; pragma Assert (Calls = 3);
         Sweep; pragma Assert (Calls = 3); -- no retry after submission/discard
      end loop;
   end loop;
   Calls := 0;
   for Field in 1 .. 6 loop
      declare Invalid : Candidate := (1, 123, 456, 789, 1); begin
         case Field is
            when 1 => Invalid.Ticket := 0;
            when 2 => Invalid.Session := 0;
            when 3 => Invalid.Sender := 0;
            when 4 => Invalid.Handle := 0;
            when 5 => Invalid.Ticket := 2; -- wrong slot
            when 6 => Invalid.Ticket := Unsigned_64'Last;
         end case;
         Remember (Object, 1, Invalid);
         Sweep;
         pragma Assert (Calls = 0);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Deferred retirement PASS:2048 candidates, saved identity, bounded polling, no submission replay, invalid admission");
   declare
      type Bytes is array (Natural range <>) of Unsigned_8;
      Storage : Bytes (0 .. 5 * 4096 - 1) := [others => 16#A5#]
        with Alignment => 4096;
      Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Storage'Address));
      Accepted : Boolean;
      Old_Cursor : Slot;
      Previous_Capacity : Positive := Capacity (Object);
   begin
      for Page in 1 .. 4 loop
         Expected_Index := 1;
         Expected := (1, 123, 456, 789, 1);
         Remember (Object, 1, Expected);
         Old_Cursor := Next_Slot (Object);
         Extend_Storage (Object, Base, Unsigned_64 (Page) * 4096, Accepted);
         pragma Assert (Accepted and Capacity (Object) > Previous_Capacity);
         pragma Assert (Next_Slot (Object) = Old_Cursor and Item_At (Object, 1) = Expected);
         for I in Previous_Capacity + 1 .. Capacity (Object) loop
            pragma Assert (Item_At (Object, I) = Candidate'(others => 0));
         end loop;
         for I in Page * 4096 .. Storage'Last loop
            pragma Assert (Storage (I) = 16#A5#);
         end loop;
         Calls := 0; Decision := Submitted;
         Sweep; pragma Assert (Calls = 1);
         for Index in Previous_Capacity + 1 .. Capacity (Object) loop
            Expected_Index := Index;
            Expected := (Unsigned_64 (Index), 123, 456, 789, 1);
            Remember (Object, Index, Expected);
            Calls := 0; Decision := Waiting;
            Sweep; pragma Assert (Calls = 1);
            Decision := Submitted;
            Sweep; pragma Assert (Calls = 2);
            Sweep; pragma Assert (Calls = 2);
         end loop;
         Previous_Capacity := Capacity (Object);
      end loop;
      Old_Cursor := Next_Slot (Object);
      Extend_Storage (Object, Base + 4096, 5 * 4096, Accepted);
      pragma Assert (not Accepted and Capacity (Object) = Previous_Capacity);
      pragma Assert (Next_Slot (Object) = Old_Cursor);
      Remember (Object, Capacity (Object) + 1,
        (Unsigned_64 (Capacity (Object) + 1), 123, 456, 789, 1));
      Calls := 0; Sweep; pragma Assert (Calls = 0);
   end;
   Ada.Text_IO.Put_Line ("Deferred growth PASS: four committed boundaries, stable candidates/cursor, new-slot polling, no replay or tail writes");
   declare
      Fair : Queue;
      Indices : constant array (0 .. 2) of Slot := [1, 8, 16];
      Tickets : array (0 .. 2) of Unsigned_64 := [1, 8, 16];
      Visits : Natural := 0;
      Remove : Boolean := False;
      function Visit (Index : Slot; Item : Candidate) return Outcome is
         Position : constant Natural := Visits mod 3;
      begin
         pragma Assert (Index = Indices (Position));
         pragma Assert (Item.Ticket = Tickets (Position) and Item.Stamp = 789);
         Visits := Visits + 1;
         return (if not Remove then Waiting elsif Index = 8 then Discarded else Submitted);
      end Visit;
      procedure Rotate is new Intel_GPU_Deferred_Retirement.Poll (Visit);
   begin
      for Index of Indices loop
         Remember (Fair, Index, (Unsigned_64 (Index), 123, 456, 789, 1));
      end loop;
      for Turn in 1 .. 9 loop Rotate (Fair); end loop;
      pragma Assert (Visits = 9 and Next_Slot (Fair) = 1);
      Tickets (1) := Intel_GPU_Buffer_Backing.Ticket_Stride + 8;
      Remember (Fair, 8, (Tickets (1), 123, 456, 789, 2));
      -- A new generation updates in place, without duplicating or jumping queue.
      Remember (Fair, 8, (8, 123, 456, 999, 1));
      for Turn in 1 .. 3 loop Rotate (Fair); end loop;
      pragma Assert (Visits = 12);
      Remove := True;
      for Turn in 1 .. 20 loop Rotate (Fair); end loop;
      pragma Assert (Visits = 15);
      for Index of Indices loop
         pragma Assert (Item_At (Fair, Index).Ticket = 0);
         Remember (Fair, Index, (Tickets ((if Index = 1 then 0 elsif Index = 8 then 1 else 2)),
           123, 456, 789, 3));
      end loop;
      for Turn in 1 .. 20 loop Rotate (Fair); end loop;
      pragma Assert (Visits = 18);
   end;
   Ada.Text_IO.Put_Line ("Deferred FIFO PASS: pending-only fair rotation, generation update in place, terminal removal, empty-queue reuse");
end Deferred_Retirement_Tests;
