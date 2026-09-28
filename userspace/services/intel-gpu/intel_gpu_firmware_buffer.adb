with CuBit.Messages; use CuBit.Messages;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code; use System.Machine_Code;
package body Intel_GPU_Firmware_Buffer is
   use Interfaces;
   use type System.Address;
   Attempted : Boolean := False;
   Capacity : constant Unsigned_64 := 1024 * 1024;
   Virtual : constant Unsigned_64 := 16#6100_0000#;
   Prepared_View : Prepared_Buffer := (Ready => False);
   function Prepared return Prepared_Buffer is (Prepared_View);
   function Flush_Retained_Buffer return Boolean is
      A, B, C, D : Unsigned_32;
      Line_Bytes, Offset : Unsigned_64;
   begin
      -- x86 adapter, not a platform-neutral DMA coherency guarantee. The
      -- retained pages are mapped and exclusively CPU-written by this service.
      -- CuBit must schedule it only on CPUs with compatible CLFLUSH support.
      Asm ("cpuid",
        Inputs => (Unsigned_32'Asm_Input ("a", 1), Unsigned_32'Asm_Input ("c", 0)),
        Outputs => (Unsigned_32'Asm_Output ("=a", A), Unsigned_32'Asm_Output ("=b", B),
                    Unsigned_32'Asm_Output ("=c", C), Unsigned_32'Asm_Output ("=d", D)),
        Volatile => True);
      if (D and 16#0008_0000#) = 0 then return False; end if;
      Line_Bytes := Unsigned_64 (Shift_Right (B, 8) and 255) * 8;
      if Line_Bytes = 0 or else Line_Bytes > 4096 or else
        (Line_Bytes and (Line_Bytes - 1)) /= 0 or else
        Virtual mod Line_Bytes /= 0 or else Capacity mod Line_Bytes /= 0
      then return False; end if;
      Asm ("mfence", Clobber => "memory", Volatile => True);
      Offset := 0;
      while Offset < Capacity loop
         Asm ("clflush (%0)", Inputs => Unsigned_64'Asm_Input ("r", Virtual + Offset),
              Clobber => "memory", Volatile => True);
         Offset := Offset + Line_Bytes;
      end loop;
      Asm ("mfence", Clobber => "memory", Volatile => True);
      return True;
   end Flush_Retained_Buffer;
   function Prepare (Source : System.Address; Bytes : Unsigned_64) return String is
      Token : constant Unsigned_64 := 16#4947_0005#;
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Start : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now, Physical : Unsigned_64;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if Attempted then return "already-attempted"; end if;
      Attempted := True;
      if Source = System.Null_Address or else Bytes = 0 or else Bytes > Capacity or else
        Unsigned_64 (To_Integer (Source)) > Unsigned_64'Last - Bytes
      then return "invalid-source"; end if;
      Msg.tag := (16#022C#, 0, 0, 0);
      if not capSubmit (15, Msg, Token) then return "submit-failed"; end if;
      loop
         Now := syscall (SYSCALL_GETTIME);
         if Now < Start or else Now - Start >= 30_000 then return "timeout"; end if;
         if Poll_Completion (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               if not capSubmit (15, Msg, Token) then return "retry-failed"; end if;
            else
               if Receipt.status /= COMPLETION_OK or else
                 Receipt.msg.tag /= (16#F000#, 3, 0, 0) or else
                 Receipt.msg.words (1 .. 3) /= [Virtual, Capacity, 0]
               then return "allocation-denied"; end if;
               Physical := Receipt.msg.words (0);
               if Physical = 0 or else Physical mod 4096 /= 0 or else
                 Physical > 2 ** 32 - Capacity
               then return "invalid-DMA-address"; end if;
               exit;
            end if;
         end if;
         Activity := Wait_For_Activity_Until
           (if Now < Unsigned_64'Last then Now + 1 else Now);
      end loop;
      declare
         type Buffer is array (Unsigned_64 range <>) of Unsigned_8;
         Input : Buffer (0 .. Bytes - 1) with Import, Address => Source;
         Output : Buffer (0 .. Capacity - 1) with Import, Volatile,
           Address => To_Address (Integer_Address (Virtual));
      begin
         for I in Output'Range loop
            Output (I) := (if I < Bytes then Input (I) else 0);
         end loop;
         for I in Output'Range loop
            if Output (I) /= (if I < Bytes then Input (I) else 0) then
               return "readback-failed";
            end if;
         end loop;
      end;
      if not Flush_Retained_Buffer then return "cache-flush-unavailable (retained)"; end if;
      Prepared_View := (Ready => True, DMA_Address => Physical,
        CPU_Address => Virtual, Allocation_Bytes => Capacity, Content_Bytes => Bytes);
      return "prepared-retained (NOT GPU-published)";
   end Prepare;
end Intel_GPU_Firmware_Buffer;
