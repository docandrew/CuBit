with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Memory;
procedure Buffer_Memory_Tests is
   package Layout renames Intel_GPU_Buffer_Backing;
   package Replies renames Intel_GPU_Buffer_Reply;
   Owner_Calls, Fail_Owner : Natural := 0;
   function Owner_Ready return Boolean is
   begin
      Owner_Calls := Owner_Calls + 1;
      return Owner_Calls /= Fail_Owner;
   end Owner_Ready;
   package Buffers is new Intel_GPU_Buffer_Memory (Owner_Ready);
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
            when 5 => Request.words (0) := 17;
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
            when 6 => Request.words (0) := 17;
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
      for I in 0 .. 15 loop
         Receipt := (Last_Token, COMPLETION_OK,
           ((16#F003#, 4, 0, 0),
            [Unsigned_64 (I), 16#0200_0000# + Unsigned_64 (I) * 4 * 1024 * 1024,
             Layout.CPU_Base + Unsigned_64 (I) * 2 * 1024 * 1024, 7]));
         Old_Token := Last_Token;
         Buffers.Complete (Object, Receipt, Consumed);
         pragma Assert (Consumed);
         if I < 15 then pragma Assert (Last_Token > Old_Token); end if;
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
      for I in 0 .. 15 loop
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
      pragma Assert (Result.Ready and Submissions = 2 and Polls = 18);
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
   pragma Assert (Munmap (Mapping, Span) = 0);
   Ada.Text_IO.Put_Line ("Buffer memory PASS: zeroing/guards, arena identity, overlap, transport failures, bounded waits, ownership quarantine (host fixture)");
end Buffer_Memory_Tests;
