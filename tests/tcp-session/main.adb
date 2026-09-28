with Ada.Text_IO;
with Interfaces; use Interfaces;
with Net;
with Internet_Checksum;
with IPv6_Header;
with IPv6_Link_Proof;
with TCP_Slots;
with TCP_Wire;
with TCP_Options;
with TCP_Time_Wait;
with TCP_Sequence;
with TCP_Listeners;
with Network_Channel_Handles;

procedure Main is
   Remote_IP : constant Net.IPv4Address := [10, 0, 2, 2];
   procedure Test_Listeners is
      use TCP_Listeners;
      Listeners : Table;
      Listener, Other, Replacement : Handle;
      Status : Bind_Status;
      Child_Index : TCP_Listeners.Connection_Index;
      Children : Connection_List;
      Success : Boolean;
   begin
      Initialize (Listeners);
      pragma Assert (Next_Deadline (Listeners) = Unsigned_64'Last);
      Bind (Listeners, 42, 0, 8080, Listener, Status);
      pragma Assert (Status = Invalid_Address and Listener = No_Handle);
      Bind (Listeners, 42, 16#0A00_020F#, 8080, Listener, Status);
      pragma Assert (Status = Bound and Listener /= No_Handle);
      pragma Assert (Owned (Listeners, 42, Listener));
      pragma Assert (not Owned (Listeners, 43, Listener));
      pragma Assert (not Owned (Listeners, 42, No_Handle));
      Bind (Listeners, 43, 16#0A00_020F#, 8080, Other, Status);
      pragma Assert (Status = Address_In_Use and Other = No_Handle);
      Bind (Listeners, 43, 16#0A00_020F#, 8081, Other, Status);
      pragma Assert (Status = Bound and Other /= Listener);
      Reserve (Listeners, Listener, 0, 100, Success);
      pragma Assert (Success and Next_Deadline (Listeners) = 100);
      Reserve (Listeners, Other, 0, 101, Success);
      pragma Assert (not Success); -- no duplicate ownership across listeners
      Accept_Ready (Listeners, 42, Listener, Child_Index, Success);
      pragma Assert (not Success); -- a SYN is not yet a usable connection
      for Child in 1 .. Maximum_Backlog - 1 loop
         Reserve (Listeners, Listener, Child, 200, Success);
         pragma Assert (Success);
      end loop;
      Reserve (Listeners, Listener, Maximum_Backlog, 300, Success);
      pragma Assert (not Success); -- bounded half-open + ready backlog
      pragma Assert (not Has_Ready (Listeners, 42, Listener));
      Mark_Ready (Listeners, 0);
      pragma Assert (Has_Ready (Listeners, 42, Listener));
      pragma Assert (not Has_Ready (Listeners, 43, Listener)); -- not the owner's
      Accept_Ready (Listeners, 43, Listener, Child_Index, Success);
      pragma Assert (not Success); -- wrong owner cannot consume it
      Accept_Ready (Listeners, 42, Listener, Child_Index, Success);
      pragma Assert (Success and Child_Index = 0);
      pragma Assert (not Has_Ready (Listeners, 42, Listener));
      pragma Assert (Next_Deadline (Listeners) = 200);
      Close (Listeners, 43, Listener, Children, Success);
      pragma Assert (not Success and Children = Connection_List'[others => False]);
      Close (Listeners, 42, Listener, Children, Success);
      pragma Assert (Success and Children =
                     Connection_List'[1 .. Maximum_Backlog - 1 => True, others => False]);
      pragma Assert (not Owned (Listeners, 42, Listener));
      Bind (Listeners, 42, 16#0A00_020F#, 8080, Replacement, Status);
      pragma Assert (Status = Bound and Replacement /= Listener);
      Reserve (Listeners, Listener, 0, 400, Success);
      pragma Assert (not Success); -- stale handle cannot select reused slot
      Reserve (Listeners, Replacement, 0, 400, Success);
      pragma Assert (Success);
      Reserve (Listeners, Other, 2, 500, Success);
      pragma Assert (Success);
      Mark_Ready (Listeners, 2);
      Expire (Listeners, 399, Children);
      pragma Assert (Children = Connection_List'[others => False]);
      Expire (Listeners, 400, Children);
      pragma Assert (Children = Connection_List'[0 => True, others => False]);
      pragma Assert (Next_Deadline (Listeners) = 500);
      Expire (Listeners, 500, Children);
      pragma Assert (Children = Connection_List'[2 => True, others => False]);
      pragma Assert (Next_Deadline (Listeners) = Unsigned_64'Last);
      Reserve (Listeners, Other, 3, 600, Success);
      pragma Assert (Success);
      Remove (Listeners, 3);
      pragma Assert (Next_Deadline (Listeners) = Unsigned_64'Last);
      --  Two listeners are bound (owners 42 and 43): fill the table.
      for Port in 8082 .. 8082 + Maximum_Listeners - 3 loop
         Bind (Listeners, 42, 16#0A00_020F#, Unsigned_16 (Port), Replacement, Status);
         pragma Assert (Status = Bound);
      end loop;
      Bind (Listeners, 42, 16#0A00_020F#, 9000, Replacement, Status);
      pragma Assert (Status = Table_Full and Replacement = No_Handle);
      --  Owner 42 exits: its listeners and their children go, 43's stay.
      Reserve (Listeners, Other, 5, 700, Success);
      pragma Assert (Success);
      Close_Owned (Listeners, 42, Children);
      pragma Assert (Children = Connection_List'[others => False]);
      pragma Assert (Owned (Listeners, 43, Other));
      Bind (Listeners, 42, 16#0A00_020F#, 9000, Replacement, Status);
      pragma Assert (Status = Bound);
      Close_Owned (Listeners, 43, Children);
      pragma Assert (Children = Connection_List'[5 => True, others => False]);
      pragma Assert (not Owned (Listeners, 43, Other));
      Ada.Text_IO.Put_Line ("TCP listeners: ownership, bounded backlog, stale handles, admission to accept, expiry, owner exit PASS");
   end Test_Listeners;
   procedure Test_Channel_Handles is
      use Network_Channel_Handles;
      Handles : Table;
      Slot : Channel_Reference;
      Old, New_Id : Handle;
      Saved : array (Channel_Index) of Handle;
   begin
      Allocate (Handles, 0, 42, Slot);
      pragma Assert (Slot = No_Channel);
      Allocate (Handles, 10, 0, Slot);
      pragma Assert (Slot = No_Channel);
      Allocate (Handles, 10, 42, Slot);
      pragma Assert (Slot = 0);
      Old := Value (Handles, Slot);
      pragma Assert (Old /= No_Handle);
      pragma Assert (Resolve (Handles, 10, 42, Old) = Slot);
      pragma Assert (Resolve (Handles, 11, 42, Old) = No_Channel);
      pragma Assert (Resolve (Handles, 10, 43, Old) = No_Channel);
      pragma Assert (Resolve (Handles, 10, 42, No_Handle) = No_Channel);
      pragma Assert (Resolve (Handles, 10, 42, Handle'Last) = No_Channel);
      for Round in 1 .. 1000 loop
         Release (Handles, 0);
         Allocate (Handles, 10, 42, Slot);
         pragma Assert (Slot = 0);
         New_Id := Value (Handles, Slot);
         pragma Assert (New_Id > Old);
         pragma Assert (Resolve (Handles, 10, 42, Old) = No_Channel);
         pragma Assert (Resolve (Handles, 10, 42, New_Id) = 0);
         Old := New_Id;
      end loop;
      Saved (0) := Old;
      for I in Channel_Index range 1 .. Channel_Index'Last loop
         Allocate (Handles, 10, 42, Slot);
         pragma Assert (Slot = I);
         Saved (I) := Value (Handles, I);
      end loop;
      Allocate (Handles, 10, 42, Slot);
      pragma Assert (Slot = No_Channel);
      for I in Saved'Range loop
         pragma Assert (Resolve (Handles, 10, 42, Saved (I)) = I);
      end loop;
      Release (Handles, 3);
      Release (Handles, 3); -- repeated internal release cannot revive an ID
      Allocate (Handles, 11, 43, Slot);
      pragma Assert (Slot = 3);
      pragma Assert (Resolve (Handles, 10, 42, Saved (3)) = No_Channel);
      pragma Assert (Resolve (Handles, 11, 43, Value (Handles, 3)) = 3);
      Ada.Text_IO.Put_Line ("Network channel handles: owner/tag binding, non-reuse, stale rejection, table exhaustion PASS");
   end Test_Channel_Handles;


   --  The word-wise checksums against RFC 1071's 16-bit big-endian sum.
   procedure Test_Checksums is
      type Buffer is array (0 .. 1_700) of Unsigned_8;
      B : Buffer;
      Seed : Unsigned_32 := 12345;
      function Reference (Pseudo : Boolean; Off, Len : Natural) return Unsigned_16 is
         Sum : Unsigned_32 := 0;
         I   : Natural := 0;
      begin
         if Pseudo then   --  10.0.2.15 -> 10.0.2.2, TCP, Len
            Sum := 16#0A00# + 16#020F# + 16#0A00# + 16#0202# + 6 + Unsigned_32 (Len);
         end if;
         while I + 1 < Len loop
            Sum := Sum + Shift_Left (Unsigned_32 (B (Off + I)), 8) + Unsigned_32 (B (Off + I + 1));
            I := I + 2;
         end loop;
         if I < Len then
            Sum := Sum + Shift_Left (Unsigned_32 (B (Off + I)), 8);
         end if;
         while Sum > 16#FFFF# loop
            Sum := (Sum and 16#FFFF#) + Shift_Right (Sum, 16);
         end loop;
         return not Unsigned_16 (Sum);
      end Reference;
   begin
      for I in B'Range loop
         Seed := Seed * 1_103_515_245 + 12_345;
         B (I) := Unsigned_8 (Shift_Right (Seed, 16) and 16#FF#);
      end loop;
      B (0 .. 7) := [others => 16#FF#];   --  carries
      for Off in 0 .. 3 loop
         for Len in 0 .. 1_600 loop
            pragma Assert (Net.internetChecksum (B (Off)'Address, Len) = Reference (False, Off, Len));
            pragma Assert (Net.transportChecksum
                             ([10, 0, 2, 15], [10, 0, 2, 2], 6, B (Off)'Address, Len) =
                           Reference (True, Off, Len));
            declare
               Bytes : constant IPv6_Header.Bytes (Off .. Off + Len - 1) :=
                 IPv6_Header.Bytes (B (Off .. Off + Len - 1));
            begin
               pragma Assert (Internet_Checksum.Of_Bytes (Bytes) = Reference (False, Off, Len));
            end;
         end loop;
      end loop;
      Ada.Text_IO.Put_Line ("Checksums: word-wise and proved (Internet_Checksum) sums match RFC 1071 for lengths 0 .. 1600 at every alignment PASS");
   end Test_Checksums;

   --  The proved IPv6_Link instance: detection, then a router solicitation.
   procedure Test_IPv6_Link is
      use IPv6_Link_Proof;
      Before : Natural;
   begin
      Link.Start ([16#52#, 16#54#, 0, 16#12#, 16#34#, 16#56#], (K0 => 1, K1 => 2), 0, False);
      pragma Assert (Frames = 1);                      --  the detection probe
      pragma Assert (Link.Next_Deadline = 1_000);
      Link.Tick (999);
      pragma Assert (Frames = 1 and then Lines = 0);
      Link.Tick (1_000);
      pragma Assert (Frames = 2 and then Lines = 1);   --  usable; solicit routers
      Before := Frames;
      Link.Receive ([0 .. 20 => 0], 2_000);            --  runt: ignored
      Link.Tick (Unsigned_64'Last);                    --  no overflow at the end of time
      pragma Assert (Frames >= Before);
      Ada.Text_IO.Put_Line ("IPv6 link (proved instance): detection period, router solicitation, runt frames, clock limit PASS");
   end Test_IPv6_Link;

   procedure Test_Time_Wait is
      use TCP_Time_Wait;
      use type TCP_Sequence.Seq;
      T : Table;
      D : Decision;
      K : constant Tuple := (Remote_IP => 16#0A00_0202#, Remote_Port => 80, Local_Port => 49152);
      M : constant MAC := [others => 1];
   begin
      Enter (T, K, M, 1000, 5000, Now => 10);
      pragma Assert (Find (T, K) /= No_Entry and then T (Find (T, K)).Deadline = 60_010);
      Arrive (T, Find (T, K), False, False, False, True, 5000, 20, D);
      pragma Assert (D = Ignore and then Find (T, K) /= No_Entry);   -- RFC 1337
      Arrive (T, Find (T, K), False, True, True, False, 4999, 30, D);
      pragma Assert (D = Acknowledge and then T (Find (T, K)).Deadline = 60_030);  -- FIN restarts
      Arrive (T, Find (T, K), True, False, False, False, 4000, 40, D);
      pragma Assert (D = Acknowledge and then Find (T, K) /= No_Entry);  -- old SYN: ACK it
      Arrive (T, Find (T, K), True, False, False, False, 6000, 50, D);
      pragma Assert (D = Reopen and then Find (T, K) = No_Entry);   -- new SYN: wait ends
      --  A full table gives up the wait that ends first.
      for I in 0 .. Capacity - 1 loop
         Enter (T, (Remote_IP => 1, Remote_Port => Unsigned_16 (I), Local_Port => 1), M, 0, 0,
                Now => Unsigned_64 (1_000 - (if I = 77 then 900 else 0) + I));
      end loop;
      Enter (T, K, M, 0, 0, Now => 5_000);
      pragma Assert (Find (T, K) /= No_Entry);
      pragma Assert (Find (T, (Remote_IP => 1, Remote_Port => 77, Local_Port => 1)) = No_Entry);
      pragma Assert (Find (T, (Remote_IP => 1, Remote_Port => 76, Local_Port => 1)) /= No_Entry);
      --  Expiry ends exactly the waits that are due.
      Expire (T, 62_000);
      pragma Assert (Find (T, (Remote_IP => 1, Remote_Port => 0, Local_Port => 1)) = No_Entry);
      pragma Assert (Find (T, K) /= No_Entry);
      pragma Assert (Next_Deadline (T) = 65_000);
      Ada.Text_IO.Put_Line ("TCP TIME-WAIT: RST ignored, FIN restarts, new SYN reopens, full table evicts earliest, expiry PASS");
   end Test_Time_Wait;

   procedure Test_Options is
      use TCP_Wire;
      O : TCP_Options.Received;
   begin
      --  Linux's SYN: MSS, SACK-permitted, timestamps, NOP, window scale.
      Parse ([2, 4, 16#05#, 16#B4#, 4, 2, 8, 10, 0, 0, 1, 2, 0, 0, 0, 3, 1, 3, 3, 7], O);
      pragma Assert (O.Has_MSS and then O.MSS = 1460 and then O.SACK_Permitted and then
                     O.Has_Timestamps and then O.TSval = 258 and then O.TSecr = 3 and then
                     O.Has_Window_Scale and then O.Shift_Count = 7);
      Parse ([1, 1, 2, 4, 2, 0], O);
      pragma Assert (O.Has_MSS and then O.MSS = 512 and then not O.Has_Window_Scale);
      Parse ([0, 2, 4, 5, 180], O);                   -- after End of Option List
      pragma Assert (not O.Has_MSS);
      Parse ([2, 4, 5], O);                           -- truncated
      pragma Assert (not O.Has_MSS);
      Parse ([8, 0, 2, 4, 5, 180], O);                -- zero length stops parsing
      pragma Assert (not O.Has_MSS);
      Parse ([2, 3, 5, 2, 4, 5, 180], O);             -- wrong MSS length skipped
      pragma Assert (O.Has_MSS and then O.MSS = 1460);
      Parse ([3, 4, 7, 0], O);                        -- wrong window scale length
      pragma Assert (not O.Has_Window_Scale);
      Parse ([30, 40, 1, 1], O);                      -- length past the end
      pragma Assert (not O.Has_MSS);
      Parse ([30, 4, 9, 9, 2, 4, 1, 0], O);           -- unknown kind skipped
      pragma Assert (O.Has_MSS and then O.MSS = 256);
      Ada.Text_IO.Put_Line ("TCP options: Linux SYN (MSS, SACK-permitted, timestamps, window scale), NOPs, end of list, truncated and malformed lengths PASS");
   end Test_Options;
begin
   Test_Listeners;
   Test_Channel_Handles;
   Test_Options;
   Test_Checksums;
   Test_Time_Wait;
   Test_IPv6_Link;
end Main;
