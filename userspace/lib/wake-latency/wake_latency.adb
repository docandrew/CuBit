pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;
with CuBit.Monotonic;
with CuBit.Waits;
with CuBit.Process_Calls;

package body Wake_Latency is

   Ignore : Unsigned_64;

   function TSC return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      Asm ("lfence; rdtsc",
           Outputs => [Unsigned_32'Asm_Output ("=a", Low),
                       Unsigned_32'Asm_Output ("=d", High)],
           Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end TSC;

   procedure Futex_Wake (X : access Word) is
   begin
      Ignore := CuBit.Waits.Futex_Wake (CuBit.Waits.Private_Word (X.all'Address), 1);
   end Futex_Wake;

   procedure Futex_Wait (X : access Word; Expected : Unsigned_32) is
   begin
      Ignore := CuBit.Waits.Futex_Wait (X.all'Address, Unsigned_64 (Expected),
                                        CuBit.Waits.Forever);
   end Futex_Wait;

   procedure Waiter (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
      Seen : Unsigned_32 := 0;
      Now  : Unsigned_64;
   begin
      Ready := 1;
      Futex_Wake (Ready'Access);
      for R in Round_Index loop
         while Unsigned_32 (Go) = Seen loop
            Waiter_Wait (Go'Access, Seen);
         end loop;
         Now := TSC;
         Cycles (R) := Now - Sent;
         Seen := Unsigned_32 (Go);
         Ack := Word (Seen);
         Futex_Wake (Ack'Access);
      end loop;
      CuBit.Process_Calls.Exit_Thread;
   end Waiter;

   procedure Drive is
   begin
      for R in Round_Index loop
         --  Let the waiter block before each wake.
         CuBit.Waits.Sleep_Until (CuBit.Monotonic.Microseconds + 200);
         Sent := TSC;
         Go := Word (R);
         Futex_Wake (Go'Access);
         while Unsigned_32 (Ack) /= Unsigned_32 (R) loop
            Futex_Wait (Ack'Access, Unsigned_32 (Ack));
         end loop;
      end loop;
   end Drive;

   function Summarise return Summary is
      Sorted : array (Round_Index) of Unsigned_64 := [for R in Round_Index => Cycles (R)];
      T0 : constant Unsigned_64 := TSC;
      U0 : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
      Per_Us : Unsigned_64;
      Swap : Unsigned_64;
      Gap : Natural := Rounds / 2;
   begin
      CuBit.Waits.Sleep_For (Calibrate_Ms);
      Per_Us := (TSC - T0) / Unsigned_64'Max (1, CuBit.Monotonic.Microseconds - U0);
      Per_Us := Unsigned_64'Max (1, Per_Us);
      --  Shell sort (no heap, no secondary stack).
      while Gap > 0 loop
         for I in Gap + 1 .. Rounds loop
            Swap := Sorted (I);
            declare
               J : Natural := I;
            begin
               while J > Gap and then Sorted (J - Gap) > Swap loop
                  Sorted (J) := Sorted (J - Gap);
                  J := J - Gap;
               end loop;
               Sorted (J) := Swap;
            end;
         end loop;
         Gap := Gap / 2;
      end loop;
      return (P50_Us => Sorted (Rounds / 2) / Per_Us,
              P99_Us => Sorted (Rounds - Rounds / 100) / Per_Us,
              Max_Us => Sorted (Rounds) / Per_Us);
   end Summarise;

end Wake_Latency;
