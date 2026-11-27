pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Benchmark_Clock;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Process_Calls;
with CuBit.Waits;
with Startup;
with Thread_ABI;

package body Fair_Threads is

   package Clock renames CuBit.Benchmark_Clock;
   package Timing renames CuBit.Timing_Histograms;

   --  bench-ipc-server's echo: the value back, and the value xor a magic.
   OP_BENCH_ECHO : constant Unsigned_32 := 16#0910#;
   REPLY_OK      : constant Unsigned_32 := 16#F000#;
   XOR_MAGIC     : constant Unsigned_64 := 16#C0B1_7000_BE11#;

   Stack_Bytes : constant := 16 * 1024;
   Stack_Align : constant := 16;

   procedure Caller (Argument : Unsigned_64) is
      I : constant Thread_Index := Thread_Index (Argument);
      Value : Unsigned_64 := Shift_Left (Argument, 40);
      Msg : Message;
      Tag : MessageTag;
      Now, Started, Finished : Unsigned_64;
      pragma Unreferenced (Tag);
   begin
      loop
         Now := CuBit.Monotonic.Milliseconds;
         exit when Now >= Window_End;
         Value := Value + 1;
         Msg := NULL_MESSAGE;
         Msg.tag := (label => OP_BENCH_ECHO, length => 1, protocol => 0, ipc => <>,
                     reserved => 0);
         Msg.words (0) := Value;
         Started := Clock.Read_Counter;
         Tag := capCall (Startup.Ipc_Test, Msg, Wait_Forever);
         Finished := Clock.Read_Counter;
         if Msg.tag.label /= REPLY_OK or else Msg.words (0) /= Value
           or else Msg.words (1) /= (Value xor XOR_MAGIC)
         then
            Failed (I) := True;
            exit;
         end if;
         if Now >= Window_Start and then Finished >= Started then
            Timing.Add (Samples (I), Finished - Started);
            Calls (I) := Calls (I) + 1;
         end if;
      end loop;
      CuBit.Process_Calls.Exit_Thread;
   end Caller;

   function Start (I : Thread_Index) return Boolean is
      Base : constant Unsigned_64 := CuBit.Process_Calls.Grow_Heap (Stack_Bytes);
      Result : Unsigned_64;
   begin
      if Base = CuBit.Process_Calls.Heap_Refused or else Base = 0 then
         return False;
      end if;
      Running (I) := 1;
      Result := CuBit.Process_Calls.Create_Thread
        (Start          => Unsigned_64 (To_Integer (Caller'Address)),
         Stack          => ((Base + Stack_Bytes) and not (Stack_Align - 1)) - 8,
         Argument       => Unsigned_64 (I),
         Thread_Pointer => 0,
         Exit_Word      => Unsigned_64 (To_Integer (Running (I)'Address)));
      if Result = Thread_ABI.Refused then
         Running (I) := 0;
         return False;
      end if;
      return True;
   end Start;

   procedure Join (I : Thread_Index) is
      Seen : Unsigned_32;
      Ignore : Unsigned_64;
   begin
      loop
         Seen := Unsigned_32 (Running (I));
         exit when Seen = 0;
         Ignore := CuBit.Waits.Futex_Wait (Running (I)'Address, Unsigned_64 (Seen),
                                           CuBit.Waits.Forever);
      end loop;
   end Join;

end Fair_Threads;
