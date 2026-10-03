with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Memory;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.Retirement;
procedure Buffer_Memory_Tests is
   package Layout renames Intel_GPU_Buffer_Backing;
   package Replies renames Intel_GPU_Buffer_Reply;
   Owner_Calls, Fail_Owner : Natural := 0;
   function Owner_Ready return Boolean is
   begin
      Owner_Calls := Owner_Calls + 1;
      return Owner_Calls /= Fail_Owner;
   end Owner_Ready;
   type Meta_RAM is array (Natural range 0 .. 65535) of Unsigned_8 with Alignment => 4096;
   Meta : Meta_RAM;
   Meta_Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Meta'Address));
   Meta_Fault : Natural := 0;
   Reserves, Commits, Clears : Natural := 0;
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
   begin
      pragma Assert (Bytes = 65536);
      Reserves := Reserves + 1;
      return (if Meta_Fault = 1 then 0 else Meta_Base);
   end Reserve;
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
   begin
      pragma Assert (Base = Meta_Base and Offset = 0 and Bytes = 65536);
      Commits := Commits + 1;
      return Meta_Fault /= 2;
   end Commit;
   function Clear (Base, Bytes : Unsigned_64) return Boolean is
   begin
      pragma Assert (Base = Meta_Base and Bytes = 65536);
      Clears := Clears + 1;
      if Meta_Fault = 3 then return False; end if;
      Meta := [others => 0];
      return True;
   end Clear;
   package Storage is new Intel_GPU_Metadata_Arena (Reserve, Commit, Clear);
   package Buffers is new Intel_GPU_Buffer_Memory (Owner_Ready, Storage);
   use type Buffers.Allocation_Stage;
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   use type Interfaces.C.int;
   Span : constant := 8 * 4096;
   Sentinel : constant Unsigned_64 := 16#A5A5_A5A5_A5A5_A5A5#;
   type Words is array (Natural range 0 .. Span / 8 - 1) of Unsigned_64;
   RAM : Words with Import, Volatile,
     Address => To_Address (Integer_Address (Layout.CPU_Base));
   Mapping : System.Address;
   Result : Replies.Backing;
   procedure Reset is
   begin
      Owner_Calls := 0; Fail_Owner := 0;
      Bad_Token := False; Bad_Status := False; Submit_Fails := False;
      Pending := False; Retry_First := False;
      Submissions := 0; Polls := 0; Now := 0; Step := 0;
      Response := ((16#F004#, 4, 0, 0),
        [Layout.CPU_Base + 4096, 4096, 7, Layout.Allocation_Key (1, 1)]);
      RAM := [others => Sentinel];
   end Reset;
   procedure Check_Guards is
   begin
      for I in 0 .. 511 loop pragma Assert (RAM (I) = Sentinel); end loop;
      for I in 3 * 512 .. RAM'Last loop pragma Assert (RAM (I) = Sentinel); end loop;
   end Check_Guards;
begin
   pragma Assert (Layout.Valid_Allocation_Request
     (Layout.Request_Label, 3, 0, 0, Unsigned_64 (Layout.Slot'Last), 1, 1, 0));
   for Fault in 0 .. 12 loop
      declare
         Request : Message := ((Layout.Request_Label, 3, 0, 0), [1, 1, 1, 0]);
      begin
         case Fault is
            when 0 => null;
            when 1 => Request.tag.length := 2;
            when 2 => Request.tag.flags := 1;
            when 3 => Request.tag.reserved := 1;
            when 4 => Request.words (0) := 0;
            when 5 => Request.words (0) := Unsigned_64 (Layout.Slot'Last) + 1;
            when 6 => Request.words (1) := 0;
            when 7 => Request.words (1) := 4097;
            when 8 => Request.words (2) := 0;
            when 9 => Request.words (2) := 2 ** 32;
            when 10 => Request.words (3) := 1;
            when 11 => Request.tag.label := Layout.Extent_Request_Label;
            when 12 => Request.words := [16, 4096, 2 ** 32 - 1, 0];
            when others => null;
         end case;
         pragma Assert (Layout.Valid_Allocation_Request
           (Request.tag.label, Request.tag.length, Request.tag.flags, Request.tag.reserved,
            Request.words (0), Request.words (1), Request.words (2), Request.words (3)) =
           (Fault = 0 or Fault = 12));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Allocation request shape PASS: generation bounds and obsolete/malformed rejection");
   for Fault in 0 .. 15 loop
      declare
         Request : Message := ((Layout.Retire_Request_Label, 4, 0, 0), [1, 1, 7, 1]);
         Sender, Owner : Unsigned_64 := 7;
         Authority : Unsigned_64 := 16#4947#;
         Granted : Boolean := True;
      begin
         case Fault is
            when 0 => null;
            when 1 => Sender := 8;
            when 2 => Owner := 0;
            when 3 => Authority := 0;
            when 4 => Granted := False;
            when 5 => Request.words (0) := 0;
            when 6 => Request.words (0) := Unsigned_64 (Layout.Slot'Last) + 1;
            when 7 => Request.words (1) := 0;
            when 8 => Request.words (1) := 2 ** 32 - 1;
            when 9 => Request.words (2) := 8;
            when 10 => Request.words (3) := 0;
            when 11 => Request.tag.length := 3;
            when 12 => Request.tag.flags := 1;
            when 13 => Request.tag.reserved := 1;
            when 14 => Request.tag.label := Layout.Request_Label;
            when 15 => Request.words := [16, 2 ** 32 - 2, 7, 1];
            when others => null;
         end case;
         pragma Assert (Layout.Retirement_Authorized
           (Request.tag.label, Request.tag.length, Request.tag.flags, Request.tag.reserved,
            Layout.Budget_Words (Request.words), Sender, Authority, Owner, Granted) =
           (Fault = 0 or Fault = 15));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Retirement request gate PASS: sender, authority, arena, generation, shape");
   Mapping := Mmap (To_Address (Integer_Address (Layout.CPU_Base)), Span,
                    3, 16#100022#, -1, 0);
   if Mapping /= To_Address (Integer_Address (Layout.CPU_Base)) then
      raise Program_Error with "cannot reserve buffer fixture without replacement";
   end if;
   Reset;
   -- Every submission has a fresh identity, even supervisor retry responses.
   -- An old completion must not cancel or advance the current transaction.
   declare
      Object : Buffers.Pool;
      Started, Consumed : Boolean;
      Old_Token : Unsigned_64;
      Receipt : CompletionEntry;
   begin
      Buffers.Start (Object, 1, 1, Started);
      pragma Assert (Started);
      Old_Token := Last_Token;
      Receipt := (Old_Token, COMPLETION_OK,
        ((16#F002#, 0, 0, 0), [others => 0]));
      Buffers.Complete (Object, Receipt, Consumed);
      pragma Assert (Consumed and Last_Token > Old_Token and Submissions = 2);
      Receipt.status := 1;
      Buffers.Complete (Object, Receipt, Consumed);
      pragma Assert (not Consumed and Buffers.Pending (Object) and Submissions = 2);
      Receipt := (Last_Token, COMPLETION_OK, Response);
      Old_Token := Last_Token;
      Buffers.Complete (Object, Receipt, Consumed);
      pragma Assert (Consumed and Last_Token > Old_Token);
      Buffers.Complete (Object, Receipt, Consumed);
      pragma Assert (not Consumed and Buffers.Pending (Object));
      -- A4KiB allocation needs only the first committed2MiB extent.
      for I in 0 .. 0 loop
         Receipt := (Last_Token, COMPLETION_OK,
           ((16#F003#, 4, 0, 0),
            [Unsigned_64 (I), 16#0200_0000# + Unsigned_64 (I) * 4 * 1024 * 1024,
             Layout.CPU_Base + Unsigned_64 (I) * 2 * 1024 * 1024, 7]));
         Old_Token := Last_Token;
         Buffers.Complete (Object, Receipt, Consumed);
         pragma Assert (Consumed);
         pragma Assert (Last_Token = Old_Token); -- no eager suffix queries
         Buffers.Complete (Object, Receipt, Consumed);
         pragma Assert (not Consumed);
      end loop;
      pragma Assert (Buffers.Result (Object).Ready);
      Response.words := [Layout.CPU_Base + 8192, 4096, 7, Layout.Allocation_Key (2, 1)];
      Buffers.Start (Object, 2, 1, Started);
      pragma Assert (Started and Last_Token > Old_Token);
      Buffers.Complete (Object, Receipt, Consumed);
      pragma Assert (not Consumed and Buffers.Pending (Object));
      Receipt := (Last_Token, COMPLETION_OK, Response);
      Buffers.Complete (Object, Receipt, Consumed);
      pragma Assert (Consumed and Buffers.Result (Object).Ready);
      Check_Guards;
   end;
   Ada.Text_IO.Put_Line ("Buffer completion identity PASS: retry, duplicate extents, late prior-allocation replies");
   Reset;
   declare
      Object : Buffers.Pool;
      Started, Consumed : Boolean;
      Receipt : CompletionEntry;
   begin
      Buffers.Start (Object, 1, 1, Started);
      pragma Assert (Started and Buffers.Pending (Object));
      pragma Assert (Buffers.Last_Stage (Object) = Buffers.Awaiting_Reply);
      pragma Assert (Submissions = 1 and Polls = 0);
      pragma Assert (not Buffers.Result (Object).Ready);
      Buffers.Start (Object, 2, 1, Started);
      pragma Assert (not Started and Submissions = 1);
      Receipt := (16#9999#, COMPLETION_OK, Response);
      Buffers.Complete (Object, Receipt, Consumed);
      pragma Assert (not Consumed and Buffers.Pending (Object));
      for Word of RAM loop pragma Assert (Word = Sentinel); end loop;
      -- Unrelated activity can run between Start and this completion.
      Receipt.token := Last_Token;
      Buffers.Complete (Object, Receipt, Consumed);
      pragma Assert (Consumed and Buffers.Pending (Object));
      for Word of RAM loop pragma Assert (Word = Sentinel); end loop;
      for I in 0 .. 0 loop
         Receipt := (Last_Token, COMPLETION_OK,
           ((16#F003#, 4, 0, 0),
            [Unsigned_64 (I), 16#0200_0000# + Unsigned_64 (I) * 4 * 1024 * 1024,
             Layout.CPU_Base + Unsigned_64 (I) * 2 * 1024 * 1024, 7]));
         Buffers.Complete (Object, Receipt, Consumed);
      end loop;
      pragma Assert (Consumed and not Buffers.Pending (Object));
      pragma Assert (Buffers.Result (Object).Ready and Polls = 0);
      pragma Assert (Buffers.Last_Stage (Object) = Buffers.Granted);
      RAM (512) := Sentinel;
      Buffers.Complete (Object, Receipt, Consumed);
      pragma Assert (not Consumed and RAM (512) = Sentinel);
      Check_Guards;
   end;
   -- No partial backing map permits a write, even when a late reply fails.
   for Bad_At in 0 .. 15 loop
      for Fault in 1 .. 4 loop
         Reset;
         -- Force exactly Bad_At+1 required extents. Failure must occur before
         -- zeroing, so the synthetic far CPU range must never be accessed.
         Response.words (0) := Layout.CPU_Base + Unsigned_64 (Bad_At) * 2 * 1024 * 1024 + 4096;
         declare
            Object : Buffers.Pool;
            Started, Consumed : Boolean;
            Receipt : CompletionEntry;
         begin
            Buffers.Start (Object, 1, 1, Started);
            pragma Assert (Started);
            Receipt := (Last_Token, COMPLETION_OK, Response);
            Buffers.Complete (Object, Receipt, Consumed);
            for I in 0 .. Bad_At loop
               Receipt := (Last_Token, COMPLETION_OK,
                 ((16#F003#, 4, 0, 0),
                  [Unsigned_64 (I), 16#0200_0000# + Unsigned_64 (I) * 4 * 1024 * 1024,
                   Layout.CPU_Base + Unsigned_64 (I) * 2 * 1024 * 1024, 7]));
               if I = Bad_At then
                  case Fault is
                     when 1 => Receipt.msg.words (3) := 8;
                     when 2 => Receipt.msg.words (1) := 2 ** 32;
                     when 3 => Receipt.status := 1;
                     when 4 => Receipt.msg.tag.flags := 1;
                     when others => null;
                  end case;
               end if;
               Buffers.Complete (Object, Receipt, Consumed);
               pragma Assert (Consumed);
            end loop;
            pragma Assert (not Buffers.Pending (Object) and not Buffers.Result (Object).Ready);
            for Word of RAM loop pragma Assert (Word = Sentinel); end loop;
            Buffers.Start (Object, 2, 1, Started);
            pragma Assert (not Started);
         end;
      end loop;
   end loop;
   for Cancel_Kind in 1 .. 2 loop
      Reset;
      declare
         Object : Buffers.Pool;
         Started, Consumed : Boolean;
         Receipt : CompletionEntry;
      begin
         Buffers.Start (Object, 1, 1, Started);
         pragma Assert (Started);
         Receipt := (Last_Token, COMPLETION_OK, Response);
         if Cancel_Kind = 1 then
            Now := 30_000;
            Buffers.Tick (Object);
         else
            Buffers.Cancel (Object);
         end if;
         pragma Assert (not Buffers.Pending (Object));
         Buffers.Complete (Object, Receipt, Consumed);
         pragma Assert (not Consumed and not Buffers.Result (Object).Ready);
         for Word of RAM loop pragma Assert (Word = Sentinel); end loop;
         Buffers.Start (Object, 2, 1, Started);
         pragma Assert (not Started and Submissions = 1);
      end;
   end loop;
   Reset;
   declare Object : Buffers.Pool; begin
      Result := Buffers.Acquire (Object, 1, 1);
      pragma Assert (Result.Ready and then
        Intel_GPU_Buffer_Reply.Page_Address (Result, 0) = 16#0200_1000#);
      for I in 512 .. 1023 loop pragma Assert (RAM (I) = 0); end loop;
      RAM (512) := Sentinel;
      Result := Buffers.Acquire (Object, 1, 1);
      pragma Assert (not Result.Ready and Submissions = 1 and RAM (512) = Sentinel);
      Response.words := [Layout.CPU_Base + 8192, 4096, 7,
                         Layout.Allocation_Key (2, 1)];
      Result := Buffers.Acquire (Object, 2, 1);
      pragma Assert (Result.Ready and RAM (512) = Sentinel);
      for I in 1024 .. 1535 loop pragma Assert (RAM (I) = 0); end loop;
      Check_Guards;
   end;
   for Fault in 1 .. 9 loop
      Reset;
      declare Object : Buffers.Pool; Count : Natural; begin
         case Fault is
            when 1 => Bad_Token := True;
            when 2 => Bad_Status := True;
            when 3 => Submit_Fails := True;
            when 4 => Response.tag.flags := 1;
            when 5 => Now := Unsigned_64'Last;
            when 6 => Pending := True; Step := 1_000;
            when 7 => Pending := True;
            when 8 => Response.words (3) := Layout.Allocation_Key (2, 1);
            when 9 => Response.words (0) := 1;
            when others => null;
         end case;
         Result := Buffers.Acquire (Object, 1, 1);
         pragma Assert (not Result.Ready);
         for Word of RAM loop pragma Assert (Word = Sentinel); end loop;
         if Fault = 7 then pragma Assert (Polls = 30_000); end if;
         Count := Submissions;
         Result := Buffers.Acquire (Object, 2, 1);
         pragma Assert (not Result.Ready and Submissions = Count);
      end;
   end loop;
   for Fault in 1 .. 2 loop
      Reset;
      declare Object : Buffers.Pool; begin
         Result := Buffers.Acquire (Object, 1, 1);
         pragma Assert (Result.Ready);
         RAM := [others => Sentinel];
         Response.words (3) := Layout.Allocation_Key (2, 1);
         if Fault = 2 then
            Response.words (2) := 8;
            Response.words (0) := Layout.CPU_Base + 8192;
         end if;
         Result := Buffers.Acquire (Object, 2, 1);
         pragma Assert (not Result.Ready);
         for Word of RAM loop pragma Assert (Word = Sentinel); end loop;
         Result := Buffers.Acquire (Object, 3, 1);
         pragma Assert (not Result.Ready and Submissions = 2);
      end;
   end loop;
   Reset;
   declare Object : Buffers.Pool; begin
      Retry_First := True;
      Result := Buffers.Acquire (Object, 1, 1);
      pragma Assert (Result.Ready and Submissions = 2 and Polls = 3);
   end;
   Reset;
   declare Object : Buffers.Pool; begin
      Response := ((16#F001#, 0, 0, 0), [others => 0]);
      Result := Buffers.Acquire (Object, 1, 1);
      pragma Assert (not Result.Ready);
      Response := ((16#F004#, 4, 0, 0),
        [Layout.CPU_Base + 4096, 4096, 7, Layout.Allocation_Key (2, 1)]);
      Result := Buffers.Acquire (Object, 2, 1);
      pragma Assert (Result.Ready and Submissions = 2);
   end;
   for Failure in 2 .. 8 loop
      Reset;
      declare Object : Buffers.Pool; Count : Natural; begin
         Response.words (1) := 8192;
         Fail_Owner := Failure;
         Result := Buffers.Acquire (Object, 1, 2);
         pragma Assert (not Result.Ready);
         Check_Guards;
         Count := Submissions;
         Fail_Owner := 0;
         Result := Buffers.Acquire (Object, 2, 1);
         pragma Assert (not Result.Ready and Submissions = Count);
      end;
   end loop;
   -- Stale, zero and future generations must fail before zeroing client RAM.
   for Generation in Unsigned_32 range 0 .. 3 loop
      if Generation /= 1 then
         Reset;
         declare Object : Buffers.Pool; begin
            Response.words (3) := Layout.Allocation_Key (1, Generation);
            Result := Buffers.Acquire (Object, 1, 1);
            pragma Assert (not Result.Ready);
            for Word of RAM loop pragma Assert (Word = Sentinel); end loop;
         end;
      end if;
   end loop;
   Reset;
   declare Object : Buffers.Pool; begin
      Response.words (3) := 1; -- obsolete slot-only reply
      Result := Buffers.Acquire (Object, 1, 1);
      pragma Assert (not Result.Ready);
      for Word of RAM loop pragma Assert (Word = Sentinel); end loop;
   end;
   Reset;
   declare
      Object : Buffers.Pool;
      Started, Consumed : Boolean;
      Receipt : CompletionEntry;
      Previous_Retirement : CompletionEntry;
      Old_Token : Unsigned_64;
   begin
      for Generation in Unsigned_32 range 1 .. 128 loop
         Response := ((16#F004#, 4, 0, 0),
           [Layout.CPU_Base + 4096, 4096, 7, Layout.Allocation_Key (1, Generation)]);
         RAM (512 .. 1023) := [others => Sentinel];
         Result := Buffers.Acquire (Object, 1, 1);
         pragma Assert (Result.Ready);
         for I in 512 .. 1023 loop pragma Assert (RAM (I) = 0); end loop;
         Buffers.Retire (Object, 1, Generation, False, Started);
         pragma Assert (not Started);
         Buffers.Retire (Object, 1, Generation + 1, True, Started);
         pragma Assert (not Started);
         Buffers.Retire (Object, 1, Generation, True, Started);
         pragma Assert (Started and Buffers.Pending (Object) and not Buffers.Result (Object).Ready);
         Old_Token := Last_Token;
         if Generation > 1 then
            -- A delayed valid ack for the previous incarnation must neither
            -- retire this allocation nor poison the current transaction.
            Buffers.Complete (Object, Previous_Retirement, Consumed);
            pragma Assert (not Consumed and Buffers.Pending (Object));
            pragma Assert (not Buffers.Retirement_Confirmed (Object, 1, Generation - 1));
            pragma Assert (not Buffers.Retirement_Confirmed (Object, 1, Generation));
            pragma Assert (Last_Token = Old_Token);
         end if;
         Buffers.Start (Object, 1, 1, Started);
         pragma Assert (not Started and Last_Token = Old_Token);
         Receipt := (Old_Token, COMPLETION_OK,
           ((Layout.Retire_Request_Label, 4, 0, 0),
            [0, Layout.Allocation_Key (1, Generation), 7, 0]));
         Buffers.Complete (Object, Receipt, Consumed);
         pragma Assert (Consumed and not Buffers.Pending (Object));
         Previous_Retirement := Receipt;
         pragma Assert (Buffers.Last_Stage (Object) = Buffers.Retired);
         pragma Assert (Buffers.Retirement_Confirmed (Object, 1, Generation));
         pragma Assert (not Buffers.Retirement_Confirmed (Object, 2, Generation));
         pragma Assert (not Buffers.Retirement_Confirmed (Object, 1, Generation + 1));
         Buffers.Complete (Object, Receipt, Consumed);
         pragma Assert (not Consumed);
         Buffers.Retire (Object, 1, Generation, True, Started);
         pragma Assert (not Started);
         Check_Guards;
      end loop;
   end;
   for Fault in 1 .. 12 loop
      Reset;
      declare
         Object : Buffers.Pool;
         Started, Consumed : Boolean;
         Receipt : CompletionEntry;
      begin
         Result := Buffers.Acquire (Object, 1, 1);
         pragma Assert (Result.Ready);
         if Fault = 8 then Submit_Fails := True; end if;
         Buffers.Retire (Object, 1, 1, True, Started);
         pragma Assert (Started = (Fault /= 8));
         Receipt := (Last_Token, COMPLETION_OK,
           ((Layout.Retire_Request_Label, 4, 0, 0), [0, Layout.Allocation_Key (1, 1), 7, 0]));
         case Fault is
            when 1 => Receipt.msg.words (1) := Layout.Allocation_Key (1, 2);
            when 2 => Receipt.msg.words (2) := 8;
            when 3 => Receipt.msg.words (0) := 1;
            when 4 => Receipt.msg.tag := (16#F002#, 0, 0, 0); -- never retry retirement
            when 5 => Receipt.status := 1;
            when 6 => Now := 30_000;
            when 7 => Fail_Owner := Owner_Calls + 1;
            when 9 => Receipt.msg.tag.length := 3;
            when 10 => Receipt.msg.tag.flags := 1;
            when 11 => Receipt.msg.tag.reserved := 1;
            when 12 => Receipt.msg.words (3) := 1;
            when others => null;
         end case;
         Buffers.Complete (Object, Receipt, Consumed);
         pragma Assert (not Buffers.Pending (Object) and not Buffers.Result (Object).Ready);
         Fail_Owner := 0; Submit_Fails := False;
         Buffers.Start (Object, 1, 1, Started); pragma Assert (not Started);
         Buffers.Start (Object, 2, 1, Started); pragma Assert (not Started);
         pragma Assert (not Buffers.Retirement_Confirmed (Object, 1, 1));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Buffer retirement PASS:128 ack/reinitialize cycles, stale identity, quarantine on uncertain outcome");
   Reset;
   declare
      Object : Buffers.Pool;
      Metadata : Words := [others => Sentinel] with Alignment => 4096;
      Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
      Accepted, Started : Boolean;
      Capacity : Positive;
   begin
      Buffers.Extend_Records (Object, Base, 4096, Accepted);
      pragma Assert (Accepted and Buffers.Record_Capacity (Object) > 16);
      Capacity := Buffers.Record_Capacity (Object);
      Buffers.Start (Object, 1, 1, Started);
      pragma Assert (Started);
      Buffers.Extend_Records (Object, Base, 8192, Accepted);
      pragma Assert (not Accepted and Buffers.Record_Capacity (Object) = Capacity);
      Buffers.Cancel (Object);
      Buffers.Extend_Records (Object, Base, 8192, Accepted);
      pragma Assert (not Accepted and Buffers.Record_Capacity (Object) = Capacity);
      for I in 512 .. Metadata'Last loop
         pragma Assert (Metadata (I) = Sentinel);
      end loop;
   end;
   Reset;
   declare
      Object : Buffers.Pool;
      Metadata : Words := [others => Sentinel] with Alignment => 4096;
      Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
      Accepted : Boolean;
   begin
      Result := Buffers.Acquire (Object, 1, 1);
      pragma Assert (Result.Ready);
      Buffers.Extend_Records (Object, Base, 4096, Accepted);
      pragma Assert (Accepted and Buffers.Result (Object).Ready);
      pragma Assert (Buffers.Result (Object).CPU_Address = Result.CPU_Address);
      Fail_Owner := Owner_Calls + 1;
      Buffers.Extend_Records (Object, Base, 8192, Accepted);
      pragma Assert (not Accepted);
   end;
   Ada.Text_IO.Put_Line ("Backing record growth PASS: old result preserved; active, quarantined and revoked growth rejected");
   declare
      Target : constant Unsigned_64 := Layout.CPU_Base + 16 * 2 * 1024 * 1024;
      Extra : constant System.Address := Mmap
        (To_Address (Integer_Address (Target)), 4096, 3, 16#100022#, -1, 0);
   begin
      pragma Assert (Extra = To_Address (Integer_Address (Target)));
      for Fault in 0 .. 5 loop
         declare
            Object : Buffers.Pool;
            Receipt : aliased CompletionEntry;
            Started, Consumed : Boolean;
            Saved_Token : Unsigned_64;
         begin
            Reset; Meta_Fault := Fault; Reserves := 0; Commits := 0; Clears := 0;
            Buffers.Configure_Heap (Object, 64 * 1024 * 1024, 2 ** 32, 65536, Started);
            pragma Assert (Started and Reserves = 0);
            Response.words (0) := Target;
            Buffers.Start (Object, 1, 1, Started); pragma Assert (Started);
            pragma Assert (Poll (Receipt'Address) = 1);
            Buffers.Complete (Object, Receipt, Consumed); pragma Assert (Consumed);
            for Index in 0 .. 15 loop
               pragma Assert (Poll (Receipt'Address) = 1);
               pragma Assert (Receipt.msg.words (0) = Unsigned_64 (Index));
               Buffers.Complete (Object, Receipt, Consumed); pragma Assert (Consumed);
            end loop;
            pragma Assert (Buffers.Last_Stage (Object) = Buffers.Awaiting_Extent_Metadata);
            Saved_Token := Last_Token;
            Buffers.Complete (Object, Receipt, Consumed);
            pragma Assert (not Consumed and Last_Token = Saved_Token and Reserves = 0);
            if Fault = 4 then Step := 30_000; end if;
            if Fault = 5 then Fail_Owner := Owner_Calls + 1; end if;
            for Turn in 1 .. 6 loop
               exit when not Buffers.Pending (Object) or else Last_Token /= Saved_Token;
               Buffers.Tick (Object);
               pragma Assert (Submissions = 1);
            end loop;
            if Fault = 0 then
               pragma Assert (Last_Token /= Saved_Token and Reserves = 1 and Commits = 1 and Clears = 1);
               pragma Assert (Poll (Receipt'Address) = 1 and Receipt.msg.words (0) = 16);
               Buffers.Complete (Object, Receipt, Consumed);
               pragma Assert (Consumed and Buffers.Result (Object).Ready);
               pragma Assert (Buffers.Result (Object).CPU_Address = Target);
            else
               pragma Assert (not Buffers.Pending (Object) and not Buffers.Result (Object).Ready);
               pragma Assert (Last_Token = Saved_Token and Reserves = (if Fault <= 3 then 1 else 0));
               Buffers.Tick (Object);
               Buffers.Start (Object, 1, 1, Started);
               pragma Assert (not Started and Submissions = 1 and Reserves = (if Fault <= 3 then 1 else 0));
            end if;
         end;
      end loop;
      declare
         Object : Buffers.Pool;
         Accepted : Boolean;
      begin
         Reset; Meta_Fault := 0; Reserves := 0; Commits := 0; Clears := 0;
         Buffers.Configure_Heap (Object, 64 * 1024 * 1024, 2 ** 32, 65536, Accepted);
         pragma Assert (Accepted);
         Response.words (0) := Target;
         Result := Buffers.Acquire (Object, 1, 1);
         pragma Assert (Result.Ready and Result.CPU_Address = Target);
         pragma Assert (Submissions = 1 and Polls = 18 and Reserves = 1 and Commits = 1);
      end;
      pragma Assert (Munmap (Extra, 4096) = 0);
   end;
   Ada.Text_IO.Put_Line ("Extent metadata wait PASS: seventeenth extent, retained token, stale completion ignored, reserve/commit/clear faults, no allocation replay");
   for Fault in 0 .. 3 loop
      Reset;
      declare
         Object : Buffers.Pool;
         Retained : array (1 .. 2) of Replies.Backing;
         procedure Resolve_Page (Session, Ticket, Offset : Unsigned_64;
                                 CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
            Slot : constant Unsigned_64 := Ticket mod 2 ** 32;
         begin
            CPU := 0; DMA := 0; Accepted := False;
            if Session /= 42 or Slot not in 1 .. 2 or Offset /= 0 or
              Ticket /= Layout.Allocation_Key (Layout.Slot (Slot), 1) then return; end if;
            if not Retained (Natural (Slot)).Ready then return; end if;
            CPU := Retained (Natural (Slot)).CPU_Address;
            DMA := Replies.Page_Address (Retained (Natural (Slot)), 0); Accepted := True;
         end Resolve_Page;
         function Released (Session : Unsigned_64) return Boolean is (Session = 42);
         function May_Free (Session, Ticket : Unsigned_64) return Boolean is
           (Session = 42 and Ticket in Layout.Allocation_Key (1, 1) .. Layout.Allocation_Key (2, 1));
         function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
           (May_Free (Session, Ticket) and then Buffers.Retirement_Confirmed
             (Object, Layout.Slot (Ticket mod 2 ** 32), Unsigned_32 (Ticket / 2 ** 32)));
         package P renames Intel_GPU_Table_Provenance;
         package A is new P.Authority (Resolve_Page);
         package R is new P.Retirement (Released, May_Free, Confirmed);
         use type P.Retirement_Phase;
         Ledger : P.Ledger;
         OK, Consumed, Found : Boolean;
         Ticket : Unsigned_64;
         Next : Natural;
         Receipt : CompletionEntry;
         Sends : Natural;
      begin
         for Slot in 1 .. 2 loop
            Response := ((16#F004#, 4, 0, 0),
              [Layout.CPU_Base + Unsigned_64 (Slot) * 4096, 4096, 7,
               Layout.Allocation_Key (Slot, 1)]);
            Retained (Slot) := Buffers.Acquire (Object, Slot, 1);
            pragma Assert (Retained (Slot).Ready);
            A.Install (Ledger, 42, 1, Slot, Layout.Allocation_Key (Slot, 1), 0, OK);
            pragma Assert (OK);
         end loop;
         R.Start (Ledger, 42, 1, OK); pragma Assert (OK);
         for Turn in 1 .. 12 loop
            R.Step (Ledger);
            if R.Phase (Ledger) = P.Request_Ready then
               R.Take_Request (Ledger, 42, Ticket, OK); pragma Assert (OK);
               Sends := Submissions; Submit_Fails := Fault = 1;
               Buffers.Retire (Object, Layout.Slot (Ticket mod 2 ** 32),
                               Unsigned_32 (Ticket / 2 ** 32), True, OK);
               pragma Assert (OK = (Fault /= 1));
               pragma Assert (not Confirmed (42, Ticket));
               P.Scan_Ticket (Ledger, 42, Ticket, 1, Found, Next, OK);
               pragma Assert (OK and Found); -- submission never clears references
               if Fault /= 1 then
                  Receipt := (Last_Token, COMPLETION_OK,
                    ((Layout.Retire_Request_Label, 4, 0, 0),
                     [0, Ticket + (if Fault = 3 then 2 ** 32 else 0), 7, 0]));
                  if Fault = 2 then
                     declare Stale : CompletionEntry := Receipt; begin
                        Stale.token := 0;
                        Buffers.Complete (Object, Stale, Consumed);
                        pragma Assert (not Consumed and not Confirmed (42, Ticket));
                     end;
                  end if;
                  Buffers.Complete (Object, Receipt, Consumed); pragma Assert (Consumed);
               end if;
               R.Acknowledge (Ledger, 42, Ticket, OK);
               pragma Assert (OK = (Fault in 0 | 2));
               if not OK then Buffers.Cancel (Object); end if;
               pragma Assert (Submissions <= Sends + 1); -- never retry release
            end if;
            exit when R.Phase (Ledger) in P.Complete | P.Failed;
         end loop;
         pragma Assert ((R.Phase (Ledger) = P.Complete) = (Fault in 0 | 2));
         if Fault in 1 | 3 then
            P.Scan_Ticket (Ledger, 42, Layout.Allocation_Key (1, 1), 1, Found, Next, OK);
            pragma Assert (OK and Found and R.Phase (Ledger) = P.Failed);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Table retirement transport PASS: real Buffer_Memory state machine, 2 allocations, stale completion, wrong generation, send failure, no early reuse");
   pragma Assert (Munmap (Mapping, Span) = 0);
   Ada.Text_IO.Put_Line ("Buffer memory PASS: zeroing/guards, arena identity, overlap, transport failures, bounded waits, ownership quarantine (host fixture)");
end Buffer_Memory_Tests;
