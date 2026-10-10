--  Hosted regression for Pointer_Pending: agreed motion coalescing while the
--  consumer stalls, with buttons, wheel and positions exactly preserved.
--  The model consumer applies reports the way desktop.svc does: move by
--  (X, Y), then observe a button change and the wheel at the new position.
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with Input_Pending;
with Pointer_Pending; use Pointer_Pending;
procedure Pointer_Tests is
   use type Input_Pending.Item;

   function Signed (Field : Word; Bits : Positive) return Integer is
     (if Field >= 2 ** (Bits - 1) then Integer (Field) - 2 ** Bits
      else Integer (Field));

   function Decode (W : Word) return Report is
     ((Buttons => Button_State (W and 16#FF#),
       X => Signed (Shift_Right (W, X_Shift) and 16#FFF#, Displacement_Bits),
       Y => Signed (Shift_Right (W, Y_Shift) and 16#FFF#, Displacement_Bits),
       Wheel => Signed (Shift_Right (W, Wheel_Shift) and 16#FF#, Wheel_Bits),
       Flags => Device_Flags (Shift_Right (W, Flags_Shift) and 16#FF#)));

   --  What a consumer can observe: transitions and wheel steps, each with
   --  the position at which it happened. Consecutive wheel steps at one
   --  position are one observation (their sum).
   Max_Log : constant := 20_000;
   type Kind is (Button_Change, Wheel_Turn);
   type Observation is record
      What : Kind := Button_Change;
      X, Y : Integer := 0;
      Value : Integer := 0;
   end record;
   type Observations is array (1 .. Max_Log) of Observation;
   type Consumer is record
      X, Y : Integer := 0;
      Buttons : Button_State := 0;
      Log : Observations;
      Used : Natural := 0;
   end record;

   procedure Apply (C : in out Consumer; R : Report) is
   begin
      C.X := C.X + R.X;
      C.Y := C.Y + R.Y;
      if R.Buttons /= C.Buttons then
         C.Used := C.Used + 1;
         C.Log (C.Used) := (Button_Change, C.X, C.Y, Integer (R.Buttons));
         C.Buttons := R.Buttons;
      end if;
      if R.Wheel /= 0 then
         if C.Used > 0 and then C.Log (C.Used).What = Wheel_Turn and then
           C.Log (C.Used).X = C.X and then C.Log (C.Used).Y = C.Y
         then
            C.Log (C.Used).Value := C.Log (C.Used).Value + R.Wheel;
         else
            C.Used := C.Used + 1;
            C.Log (C.Used) := (Wheel_Turn, C.X, C.Y, R.Wheel);
         end if;
      end if;
   end Apply;

   procedure Check_Same (Truth, Seen : Consumer; Label : String) is
   begin
      if Truth.X /= Seen.X or else Truth.Y /= Seen.Y or else
        Truth.Buttons /= Seen.Buttons or else Truth.Used /= Seen.Used
      then
         raise Program_Error with Label & ": final state differs";
      end if;
      for I in 1 .. Truth.Used loop
         if Truth.Log (I) /= Seen.Log (I) then
            raise Program_Error with Label & ": observation" & I'Image & " differs";
         end if;
      end loop;
   end Check_Same;

   S : State;
   Outcome : Append_Outcome;
   Truth, Seen : Consumer;
   Last_Sequence : Word := 0;
   Coalesced_Count, Appended_Count : Natural := 0;

   procedure Drain (Limit : Natural := Natural'Last) is
      Taken : Natural := 0;
   begin
      while Count (S) > 0 and then Taken < Limit loop
         declare
            Head : constant Input_Pending.Item := Element (S, 0);
         begin
            --  Coalescing never consumes a sequence number: no gap, ever.
            if Head.Sequence /= Input_Pending.Next_Sequence (Last_Sequence) then
               raise Program_Error with "sequence gap";
            end if;
            if Head.Recover then
               raise Program_Error with "unexpected recovery";
            end if;
            Last_Sequence := Head.Sequence;
            Apply (Seen, Decode (Head.Payload));
         end;
         Acknowledge (S);
         Taken := Taken + 1;
      end loop;
   end Drain;

   procedure Feed (R : Report) is
   begin
      Apply (Truth, R);
      Append (S, R, 0, Outcome);
      case Outcome is
         when Coalesced => Coalesced_Count := Coalesced_Count + 1;
         when Appended => Appended_Count := Appended_Count + 1;
         when Overflowed => raise Program_Error with "unexpected overflow";
      end case;
   end Feed;

   subtype Choice is Natural range 0 .. 999;
   package Random_Choice is new Ada.Numerics.Discrete_Random (Choice);
   Gen : Random_Choice.Generator;
   Buttons : Button_State := 0;
begin
   --  Encoding round trip at the field limits.
   for X in Displacement loop
      declare
         R : constant Report := (Buttons => 5, X => X, Y => -X - 1,
           Wheel => X mod 256 - 128, Flags => 1);
      begin
         if Decode (Encode (R)) /= R then
            raise Program_Error with "encode/decode";
         end if;
      end;
   end loop;

   --  1. A one-second 1000 Hz flood during a stalled consumer stays bounded
   --  and conserves displacement exactly.
   for I in 1 .. 1000 loop
      Feed ((X => 3, Y => -2, others => <>));
   end loop;
   if Count (S) > 4 then
      raise Program_Error with "flood not coalesced:" & Count (S)'Image;
   end if;
   Drain;
   Check_Same (Truth, Seen, "flood");

   --  2. A press, drag and release mid-flood: the press and release land at
   --  exactly the positions they happened at.
   for I in 1 .. 300 loop
      Feed ((Buttons => (if I in 100 .. 199 then 1 else 0),
             X => (if I mod 7 = 0 then -5 else 4), Y => 1, others => <>));
   end loop;
   if Count (S) > 8 then
      raise Program_Error with "drag not coalesced:" & Count (S)'Image;
   end if;
   Drain;
   Check_Same (Truth, Seen, "drag");

   --  3. Wheel between motion: never moved past later motion.
   Feed ((X => 10, others => <>));
   Feed ((Wheel => 1, others => <>));
   Feed ((Wheel => 1, others => <>));
   Feed ((X => 10, others => <>));
   Feed ((X => 10, Wheel => -1, others => <>));
   Feed ((Y => 3, others => <>));
   Drain;
   Check_Same (Truth, Seen, "wheel");

   --  4. Randomized: random reports and random partial drains (the consumer
   --  stalls and resumes). Fewer than Capacity transitions per stall.
   Random_Choice.Reset (Gen, 20261009);
   for Round in 1 .. 20_000 loop
      declare
         C : constant Choice := Random_Choice.Random (Gen);
      begin
         if C < 20 and then Truth.Used < Max_Log - 2 then
            Buttons := Buttons xor Button_State (2 ** (C mod 3));
         end if;
         Feed ((Buttons => Buttons,
                X => Random_Choice.Random (Gen) mod 255 - 127,
                Y => Random_Choice.Random (Gen) mod 255 - 127,
                Wheel => (if Random_Choice.Random (Gen) < 30
                          then Random_Choice.Random (Gen) mod 3 - 1 else 0),
                Flags => 0));
         if Random_Choice.Random (Gen) < 50 or else Count (S) > Input_Pending.Capacity - 4 then
            Drain (Random_Choice.Random (Gen) mod 4 + 1);
         end if;
      end;
   end loop;
   Drain;
   Check_Same (Truth, Seen, "random");

   --  5. True overflow: Capacity unpublished button transitions. Reported
   --  explicitly (Overflowed, recovery flag), never silently.
   Buttons := 0;
   for I in 1 .. Input_Pending.Capacity loop
      Buttons := Buttons xor 1;
      Append (S, (Buttons => Buttons, others => <>), 0, Outcome);
      if Outcome /= Appended then
         raise Program_Error with "early overflow";
      end if;
   end loop;
   Append (S, (Buttons => Buttons xor 1, others => <>), 0, Outcome);
   if Outcome /= Overflowed or else Count (S) /= 1 or else
     not Element (S, 0).Recover
   then
      raise Program_Error with "overflow not reported";
   end if;

   --  6. Consumer replacement: no merge into a stream with no basis.
   Reset (S);
   Append (S, (X => 1, others => <>), 0, Outcome);
   if Outcome /= Appended or else not Element (S, 0).Recover then
      raise Program_Error with "reset";
   end if;
   Append (S, (X => 1, others => <>), 0, Outcome);
   if Outcome /= Appended then
      raise Program_Error with "merged into recovery report";
   end if;

   --  7. The NUC failure, modelled: a mouse reporting at Rate Hz moves
   --  while desktop stalls for Stall ms (no events taken). The kernel takes
   --  Kernel_Credit reports from one publisher, then answers busy and the
   --  driver retains. Raw retention (the old policy: one slot per report)
   --  overflows once Rate * Stall exceeds credit + Capacity, which loses the
   --  backlog and shows desktop a sequence gap. Coalescing keeps a few.
   declare
      Kernel_Credit : constant := 16;
      type Rate_List is array (Positive range <>) of Positive;
      Rates : constant Rate_List := [125, 500, 1000];
      Stalls : constant Rate_List := [50, 100, 400, 1000];
   begin
      for Rate of Rates loop
         for Stall of Stalls loop
            declare
               Reports : constant Natural := Rate * Stall / 1000;
               Raw : Input_Pending.Queue;
               Raw_Lost : Boolean;
               Raw_Overflows : Natural := 0;
               New_State : State;
               New_Overflows, Peak : Natural := 0;
               In_Kernel_Raw, In_Kernel_New : Natural := 0;
            begin
               for I in 1 .. Reports loop
                  if In_Kernel_Raw < Kernel_Credit then
                     In_Kernel_Raw := In_Kernel_Raw + 1;
                  else
                     Input_Pending.Append (Raw, Encode ((X => 2, others => <>)), Raw_Lost);
                     if Raw_Lost then Raw_Overflows := Raw_Overflows + 1; end if;
                  end if;
                  --  Coalescing driver: the kernel takes the head while it
                  --  has credit; the rest stay pending and merge.
                  Append (New_State, (X => 2, others => <>), 0, Outcome);
                  if Outcome = Overflowed then New_Overflows := New_Overflows + 1; end if;
                  while Count (New_State) > 0 and then In_Kernel_New < Kernel_Credit loop
                     Acknowledge (New_State);
                     In_Kernel_New := In_Kernel_New + 1;
                  end loop;
                  Peak := Natural'Max (Peak, Count (New_State));
               end loop;
               Ada.Text_IO.Put_Line
                 ("POINTER-STALL: rate=" & Rate'Image & "Hz stall=" & Stall'Image &
                  "ms reports=" & Reports'Image &
                  " raw_overflows=" & Raw_Overflows'Image &
                  " coalesced_overflows=" & New_Overflows'Image &
                  " coalesced_peak_pending=" & Peak'Image);
               if New_Overflows /= 0 or else Peak > 2 then
                  raise Program_Error with "stall model lost input";
               end if;
               if (Reports > Kernel_Credit + Input_Pending.Capacity) /= (Raw_Overflows > 0) then
                  raise Program_Error with "raw model mismatch";
               end if;
            end;
         end loop;
      end loop;
   end;

   Ada.Text_IO.Put_Line ("POINTER-PENDING: appended" & Appended_Count'Image &
     " coalesced" & Coalesced_Count'Image & " observations" & Truth.Used'Image);
   Ada.Text_IO.Put_Line ("POINTER-PENDING: PASS coalesce/conserve/order/wheel/overflow/reset");
end Pointer_Tests;
