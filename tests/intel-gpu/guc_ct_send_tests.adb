with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GuC_CT_Send;
procedure GuC_CT_Send_Tests is
   Memory : array (Unsigned_32 range 0 .. 7) of Unsigned_32;
   Head : Unsigned_32 := 6;
   Descriptor_Status : Unsigned_32 := 0;
   Tail : Unsigned_32;
   Expected_Start : Unsigned_32 := 6;
   Expected_Tail : Unsigned_32 := 2;
   Expected_Writes : Natural := 4;
   Calls, Failure, Writes, Barriers, Notifications : Natural;
   procedure Step (OK : out Boolean) is
   begin Calls := Calls + 1; OK := Calls /= Failure; end Step;
   procedure Read_Descriptor (H, T, S : out Unsigned_32; OK : out Boolean) is
   begin H := Head; T := Tail; S := Descriptor_Status; Step (OK); end Read_Descriptor;
   procedure Write_Word (Index, Value : Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (Barriers = 0 and Index = (Expected_Start + Unsigned_32 (Writes)) mod 8);
      Memory (Index) := Value; Writes := Writes + 1; Step (OK);
   end Write_Word;
   procedure Barrier (OK : out Boolean) is
   begin pragma Assert (Writes = Expected_Writes); Barriers := Barriers + 1; Step (OK); end Barrier;
   procedure Write_Tail (Value : Unsigned_32; OK : out Boolean) is
   begin pragma Assert (Barriers = 1 and Value = Expected_Tail); Tail := Value; Step (OK); end Write_Tail;
   procedure Notify (OK : out Boolean) is
   begin pragma Assert (Barriers = 2 and Tail = Expected_Tail); Notifications := Notifications + 1; Step (OK); end Notify;
   package Transport is new Intel_GPU_GuC_CT_Send
     (Read_Descriptor, Write_Word, Barrier, Write_Tail, Notify);
   use Transport;
   Status : Result;
begin
   for Fail in 0 .. 9 loop
      declare
         Object : Channel;
         Saved : Natural;
      begin
         Calls := 0; Writes := 0; Barriers := 0; Notifications := 0;
         Failure := Fail; Tail := 6; Memory := [others => 0];
         Initialize (Object, True, 8, 6);
         Send (Object, [10 => 16#100#, 11 => 16#200#, 12 => 16#300#], 16#1234#, Status);
         if Fail = 0 then
            pragma Assert (Status = Queued and State (Object) = Active);
            pragma Assert (Memory (6) = 16#12340003# and Memory (7) = 16#100# and
                           Memory (0) = 16#200# and Memory (1) = 16#300#);
            pragma Assert (Calls = 9 and Notifications = 1);
         else
            pragma Assert (Status = (if Fail = 1 then Corrupt else Quarantined));
            pragma Assert (State (Object) = Broken and Calls = Fail);
            Saved := Calls;
            Initialize (Object, True, 8, Tail);
            Send (Object, [1, 2, 3], 2, Status);
            pragma Assert (Status = Rejected and Calls = Saved and State (Object) = Broken);
         end if;
      end;
   end loop;
   -- A full ring is ordinary backpressure, not channel corruption. Firmware
   -- advancing its head must allow the same request to be submitted once.
   declare
      Object : Channel;
   begin
      Calls := 0; Writes := 0; Barriers := 0; Notifications := 0;
      Failure := 0; Tail := 6; Head := 7; Memory := [others => 0];
      Initialize (Object, True, 8, 6);
      for Attempt in 1 .. 3 loop
         Send (Object, [1, 2, 3], 1, Status);
         pragma Assert (Status = Would_Block and State (Object) = Active);
         pragma Assert (Calls = Attempt and Writes = 0 and Barriers = 0 and
                        Notifications = 0 and Tail = 6);
      end loop;
      Head := 6;
      Send (Object, [1, 2, 3], 1, Status);
      pragma Assert (Status = Queued and State (Object) = Active);
      pragma Assert (Calls = 12 and Writes = 4 and Notifications = 1);
   end;
   -- Invalid descriptors break the channel before any device-visible write.
   for Fault in 1 .. 4 loop
      declare
         Object : Channel;
      begin
         Calls := 0; Writes := 0; Barriers := 0; Notifications := 0;
         Failure := 0; Head := 6; Tail := 6; Descriptor_Status := 0;
         case Fault is
            when 1 => Head := 8;
            when 2 => Tail := 8;
            when 3 => Tail := 5; -- In range but differs from owned producer tail.
            when 4 => Descriptor_Status := 1;
         end case;
         Initialize (Object, True, 8, 6);
         Send (Object, [1, 2, 3], 1, Status);
         pragma Assert (Status = Corrupt and State (Object) = Broken);
         pragma Assert (Calls = 1 and Writes = 0 and Barriers = 0 and Notifications = 0);
         Head := 6; Tail := 6; Descriptor_Status := 0;
         Send (Object, [1, 2, 3], 1, Status);
         pragma Assert (Status = Rejected and Calls = 1);
      end;
   end loop;
   -- Local input rejection does not even inspect firmware memory, and must
   -- not poison an otherwise usable channel.
   declare
      Object : Channel;
      Empty : Words (1 .. 0);
      Oversized : constant Words (1 .. 256) := [others => 0];
   begin
      Calls := 0; Writes := 0; Barriers := 0; Notifications := 0;
      Failure := 0; Head := 6; Tail := 6;
      Send (Object, [1, 2, 3], 1, Status);
      pragma Assert (Status = Rejected and Calls = 0 and State (Object) = Uninitialized);
      Initialize (Object, True, 8, 6);
      Send (Object, Empty, 1, Status);
      pragma Assert (Status = Rejected and Calls = 0 and State (Object) = Active);
      Send (Object, Oversized, 1, Status);
      pragma Assert (Status = Rejected and Calls = 0 and State (Object) = Active);
      Send (Object, [1, 2, 3], 1, Status);
      pragma Assert (Status = Queued and Calls = 9);
   end;
   for Fault in 1 .. 2 loop
      declare
         Object : Channel;
      begin
         Calls := 0;
         Initialize (Object, Fault /= 1, 8, (if Fault = 2 then 8 else 6));
         pragma Assert (State (Object) = Broken);
         Initialize (Object, True, 8, 6);
         Send (Object, [1, 2, 3], 1, Status);
         pragma Assert (Status = Rejected and Calls = 0 and State (Object) = Broken);
      end;
   end loop;
   -- Consecutive successful sends must retain the new producer cursor, not
   -- reuse the initialization cursor or overwrite an outstanding message.
   declare
      Object : Channel;
   begin
      Failure := 0; Head := 6; Tail := 6; Descriptor_Status := 0;
      Initialize (Object, True, 8, 6);
      for Iteration in 1 .. 20 loop
         Calls := 0; Writes := 0; Barriers := 0; Notifications := 0;
         Expected_Start := Tail; Expected_Tail := (Tail + 4) mod 8;
         Send (Object, [1, 2, 3], Unsigned_16 (Iteration), Status);
         pragma Assert (Status = Queued and State (Object) = Active and Calls = 9);
         pragma Assert (Memory (Expected_Start) =
                          Shift_Left (Unsigned_32 (Iteration), 16) + 3);
         -- One unused DWORD leaves only three free: another four-DWORD
         -- message must wait for firmware to consume the outstanding one.
         Send (Object, [1, 2, 3], 0, Status);
         pragma Assert (Status = Would_Block and Calls = 10 and Writes = 4);
         Head := Tail;
      end loop;
   end;
   -- Mix request lengths on one channel and visit every producer position.
   -- Unlike four-word-only traffic this crosses odd and even wrap boundaries.
   declare
      Object : Channel;
      Seen : array (Unsigned_32 range 0 .. 7) of Boolean := [others => False];
   begin
      Failure := 0; Head := 0; Tail := 0; Descriptor_Status := 0;
      Initialize (Object, True, 8, 0);
      for Iteration in 1 .. 48 loop
         declare
            Length : constant Positive := 1 + (Iteration mod 6);
            Payload : Words (10 .. 10 + Length - 1);
            Before : constant Words := Words (Memory);
         begin
            for I in Payload'Range loop
               Payload (I) := Unsigned_32 (Iteration * 256 + I);
            end loop;
            Calls := 0; Writes := 0; Barriers := 0; Notifications := 0;
            Expected_Start := Tail; Seen (Tail) := True;
            Expected_Writes := Length + 1;
            Expected_Tail := (Tail + Unsigned_32 (Expected_Writes)) mod 8;
            Send (Object, Payload, Unsigned_16 (Iteration), Status);
            pragma Assert (Status = Queued and State (Object) = Active);
            pragma Assert (Calls = Expected_Writes + 5 and Notifications = 1);
            pragma Assert (Memory (Expected_Start) =
              Shift_Left (Unsigned_32 (Iteration), 16) + Unsigned_32 (Length));
            for Offset in 1 .. Length loop
               pragma Assert (Memory ((Expected_Start + Unsigned_32 (Offset)) mod 8) =
                                Payload (Payload'First + Offset - 1));
            end loop;
            for Offset in Expected_Writes .. 7 loop
               pragma Assert (Memory ((Expected_Start + Unsigned_32 (Offset)) mod 8) =
                                Before (Natural ((Expected_Start + Unsigned_32 (Offset)) mod 8)));
            end loop;
            Head := Tail; -- Model firmware consumption, not an acknowledgment.
         end;
      end loop;
      pragma Assert (for all Visited of Seen => Visited);
   end;
   -- A fault after a successful request must remain terminal, including an
   -- uncertain interrupt write. Never replay that request or reset its cursor.
   for Fail in 1 .. 9 loop
      declare
         Object : Channel;
      begin
         Failure := 0; Head := 6; Tail := 6; Descriptor_Status := 0;
         Expected_Writes := 4; Expected_Start := 6; Expected_Tail := 2;
         Calls := 0; Writes := 0; Barriers := 0; Notifications := 0;
         Initialize (Object, True, 8, 6);
         Send (Object, [1, 2, 3], 1, Status);
         pragma Assert (Status = Queued and Notifications = 1);
         Head := Tail; Expected_Start := Tail; Expected_Tail := 6;
         Calls := 0; Writes := 0; Barriers := 0; Notifications := 0;
         Failure := Fail;
         Send (Object, [4, 5, 6], 2, Status);
         pragma Assert (Status = (if Fail = 1 then Corrupt else Quarantined));
         pragma Assert (State (Object) = Broken and Calls = Fail);
         Initialize (Object, True, 8, Tail);
         Send (Object, [4, 5, 6], 2, Status);
         pragma Assert (Status = Rejected and Calls = Fail);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("GuC CT send PASS: mixed-length repeated sends, wrap, ordering, callback failures, backpressure, corruption and rejection");
end GuC_CT_Send_Tests;
