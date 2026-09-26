with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;

package body Nonces is
   Have_RDRAND : Boolean := False;
   Fallback_State : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;

   procedure Initialize (Hardware : out Boolean) is
      EAX, EBX, ECX, EDX : Unsigned_32;
   begin
      Asm ("cpuid",
           Outputs => [Unsigned_32'Asm_Output ("=a", EAX),
                       Unsigned_32'Asm_Output ("=b", EBX),
                       Unsigned_32'Asm_Output ("=c", ECX),
                       Unsigned_32'Asm_Output ("=d", EDX)],
           Inputs => [Unsigned_32'Asm_Input ("a", 1),
                      Unsigned_32'Asm_Input ("c", 0)],
           Volatile => True);
      Have_RDRAND := (ECX and 16#4000_0000#) /= 0; -- CPUID.01H:ECX bit 30
      Hardware := Have_RDRAND;
   end Initialize;

   function TSC return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      Asm ("rdtsc",
           Outputs => [Unsigned_32'Asm_Output ("=a", Low),
                       Unsigned_32'Asm_Output ("=d", High)],
           Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end TSC;

   function Next return Unsigned_64 is
      Value : Unsigned_64;
      Carry : Unsigned_8;
   begin
      if Have_RDRAND then
         --  RDRAND can transiently fail (CF clear); Intel recommends 10 tries.
         for Attempt in 1 .. 10 loop
            Asm ("rdrand %0; setc %1",
                 Outputs => [Unsigned_64'Asm_Output ("=r", Value),
                             Unsigned_8'Asm_Output ("=qm", Carry)],
                 Volatile => True);
            if Carry = 1 and then Value /= 0 then
               return Value;
            end if;
         end loop;
      end if;
      --  SplitMix64 step over TSC and monotonic time.
      Fallback_State := Fallback_State + 16#9E37_79B9_7F4A_7C15# +
        (TSC xor syscall (SYSCALL_GETTIME));
      Value := Fallback_State;
      Value := (Value xor Shift_Right (Value, 30)) * 16#BF58_476D_1CE4_E5B9#;
      Value := (Value xor Shift_Right (Value, 27)) * 16#94D0_49BB_1331_11EB#;
      Value := Value xor Shift_Right (Value, 31);
      return (if Value = 0 then 1 else Value);
   end Next;
end Nonces;
