pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Benchmark_Clock;
with CuBit.Monotonic;
with CuBit.Process_Calls;

package body Notify_Check_Support is

   Stack_Size : constant := 32 * 1024;

   function Read_FS return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("rdfsbase %0", Outputs => Unsigned_64'Asm_Output ("=r", Value), Volatile => True);
      return Value;
   end Read_FS;

   function Spawn (Code : Entry_Point; Argument : Unsigned_64; Exit_Word : access Word)
     return Boolean
   is
      Base : constant Unsigned_64 := CuBit.Process_Calls.Grow_Heap (Stack_Size);
      Stack : Unsigned_64;
   begin
      if Base = Unsigned_64'Last or else Base = 0 then
         return False;
      end if;
      --  A SysV function expects RSP + 8 to be 16-byte aligned.
      Stack := ((Base + Stack_Size) and not 15) - 8;
      Exit_Word.all := 1;
      return CuBit.Process_Calls.Create_Thread
        (Unsigned_64 (To_Integer (Code.all'Address)), Stack, Argument, Read_FS,
         Unsigned_64 (To_Integer (Exit_Word.all'Address))) /= Unsigned_64'Last;
   end Spawn;

   procedure Join (Exit_Word : access Word) is
      V : Unsigned_32;
      Ignore : Unsigned_64;
   begin
      loop
         V := Unsigned_32 (Exit_Word.all);
         exit when V = 0;
         Ignore := N.Private_Wait (Exit_Word.all'Address, Unsigned_64 (V), N.Forever);
      end loop;
   end Join;

   function Now_Us return Unsigned_64 is (CuBit.Monotonic.Microseconds);

   procedure Wait_Once (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
   begin
      Waiting := True;
      Target_Result := N.Wait (Target, N.Forever);
      CuBit.Process_Calls.Exit_Thread;
   end Wait_Once;

   procedure Wake_Later (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
      Until_Us : constant Unsigned_64 := Now_Us + Delay_Us;
      Ignore : Unsigned_64;
   begin
      while Now_Us < Until_Us loop
         null;
      end loop;
      Ignore := N.Wake (By);
      CuBit.Process_Calls.Exit_Thread;
   end Wake_Later;

   procedure Close_Both (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
      Ignore : Boolean;
   begin
      Ignore := N.Close (Close_B);
      Ignore := N.Close (Close_A);
      CuBit.Process_Calls.Exit_Thread;
   end Close_Both;

   procedure Sleep_On_Gate (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
   begin
      Waiting := True;
      Gate_Result := N.Private_Wait (Gate'Address, 0, N.Forever);
      CuBit.Process_Calls.Exit_Thread;
   end Sleep_On_Gate;

   procedure Serve (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
      Bits : Unsigned_64;
      Ignore : Unsigned_64;
   begin
      loop
         Bits := N.Wait (Server_In, N.Forever);
         exit when N.ABI.Is_Status (Bits) or else (Bits and Stop_Badge) /= 0;
         Server_Turns := Server_Turns + 1;
         if (Bits and Victim_Badge) /= 0 then
            Ignore := N.Wake (Server_Reply);
         end if;
      end loop;
      CuBit.Process_Calls.Exit_Thread;
   end Serve;

   procedure Flood (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
      Count : Unsigned_64 := 0;
      Ignore : Unsigned_64;
   begin
      while Flooding loop
         Ignore := N.Wake (Hostile_Ping);
         Count := Count + 1;
      end loop;
      Hostile_Wakes := Hostile_Wakes + Count;
      CuBit.Process_Calls.Exit_Thread;
   end Flood;

   procedure Ping_Pong (Rounds : Positive; Samples : out CuBit.Timing_Histograms.Histogram;
                        Failures : out Natural)
   is
      Started, Finished : Unsigned_64;
      Bits : Unsigned_64;
   begin
      Samples := CuBit.Timing_Histograms.Empty;
      Failures := 0;
      for I in 1 .. Rounds loop
         Started := CuBit.Benchmark_Clock.Read_Counter;
         if N.Wake (Victim_Ping) /= N.ABI.Signalled then
            Failures := Failures + 1;
         end if;
         Bits := N.Wait (Victim_In, Now_Us + 1_000_000);
         Finished := CuBit.Benchmark_Clock.Read_Counter;
         if Bits /= Victim_Badge then
            Failures := Failures + 1;
         elsif Finished >= Started then
            CuBit.Timing_Histograms.Add (Samples, Finished - Started);
         end if;
      end loop;
   end Ping_Pong;

end Notify_Check_Support;
