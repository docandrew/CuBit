with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Lease;
procedure Display_Lease_Tests is
   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;
   type Well is (Parent, Middle, Child);
   procedure Run (Inherited : Unsigned_64; Fail_Hold, Fail_Drop : Natural) is
      Holds, Drops : Natural := 0;
      Stopped : Boolean := False;
      Held_Bits, Dropped_Bits : Unsigned_64 := 0;
      function Valid (Bits : Unsigned_64) return Boolean is (Bits = 7);
      procedure Hold (Item : Well; Added, Success : out Boolean) is
         N : constant Natural := Well'Pos (Item);
      begin
         pragma Assert (not Stopped and N = Holds);
         Holds := Holds + 1;
         Held_Bits := Held_Bits or Shift_Left (Unsigned_64'(1), N);
         Added := (Inherited and Shift_Left (Unsigned_64'(1), N)) = 0;
         Success := Fail_Hold /= N + 1;
         Stopped := not Success;
      end Hold;
      procedure Drop (Item : Well; Added : Boolean; Success : out Boolean) is
         N : constant Natural := Well'Pos (Item);
         B : constant Unsigned_64 := Shift_Left (Unsigned_64'(1), N);
      begin
         pragma Assert (Added = ((Inherited and B) = 0));
         if not Added then Success := True; return; end if;
         pragma Assert (not Stopped and (Inherited and B) = 0);
         -- Every later added descendant must already have been released.
         for Later in Well loop
            if Well'Pos (Later) > N then
               pragma Assert (((Inherited or Dropped_Bits) and
                 Shift_Left (Unsigned_64'(1), Well'Pos (Later))) /= 0);
            end if;
         end loop;
         Drops := Drops + 1;
         Success := Fail_Drop /= N + 1;
         if Success then Dropped_Bits := Dropped_Bits or B; end if;
         Stopped := not Success;
      end Drop;
      package Lease is new Intel_GPU_Display_Lease (Well, Valid, Hold, Drop);
      use type Lease.State_Kind;
      OK : Boolean;
   begin
      for Bad of Unsigned_64_Array'[0, 1, 2, 4, 8, Unsigned_64'Last] loop
         Lease.Acquire (Bad, OK);
         pragma Assert (not OK and Lease.State = Lease.Idle and Holds = 0);
      end loop;
      Lease.Release (OK);
      pragma Assert (not OK and Drops = 0);
      Lease.Acquire (7, OK);
      pragma Assert (OK = (Fail_Hold = 0));
      if not OK then
         pragma Assert (Lease.State = Lease.Faulted and Drops = 0);
         pragma Assert (Lease.Retained = Held_Bits);
         pragma Assert (Lease.Uncertain = Shift_Left (Unsigned_64'(1), Fail_Hold - 1));
         Lease.Release (OK);
         pragma Assert (not OK and Drops = 0);
      else
         pragma Assert (Lease.State = Lease.Held and Lease.Retained = 7);
         Lease.Acquire (7, OK);
         pragma Assert (not OK and Holds = 3);
         Lease.Release (OK);
         if Fail_Drop = 0 or else
           (Inherited and Shift_Left (Unsigned_64'(1), Fail_Drop - 1)) /= 0
         then
            pragma Assert (OK and Lease.State = Lease.Idle);
            pragma Assert (Lease.Retained = 0 and Lease.Uncertain = 0);
            pragma Assert (Dropped_Bits = (7 and not Inherited));
         else
            pragma Assert (not OK and Lease.State = Lease.Faulted);
            pragma Assert (Lease.Uncertain = Shift_Left (Unsigned_64'(1), Fail_Drop - 1));
            pragma Assert (Lease.Retained = Shift_Left (Unsigned_64'(1), Fail_Drop) - 1);
         end if;
      end if;
      if Lease.State = Lease.Faulted then
         declare
            Before : constant Natural := Holds + Drops;
         begin
            Lease.Acquire (7, OK);
            pragma Assert (not OK);
            Lease.Release (OK);
            pragma Assert (not OK and Holds + Drops = Before);
         end;
      else
         -- A fully released reference is reusable; inherited requests still
         -- belong to the backend baseline and must again remain untouched.
         Holds := 0; Drops := 0; Held_Bits := 0; Dropped_Bits := 0;
         Lease.Acquire (7, OK);
         pragma Assert (OK and Holds = 3);
         Lease.Release (OK);
         pragma Assert (OK and Lease.State = Lease.Idle and Lease.Retained = 0);
         pragma Assert (Dropped_Bits = (7 and not Inherited));
      end if;
   end Run;
begin
   pragma Assert (Well'Pos (Parent) = 0 and Well'Pos (Middle) = 1 and Well'Pos (Child) = 2);
   for Inherited in Unsigned_64 range 0 .. 7 loop
      for H in 0 .. 3 loop
         for D in 0 .. 3 loop Run (Inherited, H, D); end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Display lease PASS: 128 inherited/hold/drop fault combinations");
end Display_Lease_Tests;
