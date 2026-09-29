with CuBit.Messages; use CuBit.Messages;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_DMA_Cache;
package body Intel_GPU_Firmware_Buffer is
   use Interfaces;
   use type System.Address;
   Attempted : Boolean := False;
   Capacity : constant Unsigned_64 := 1024 * 1024;
   Virtual : constant Unsigned_64 := 16#6100_0000#;
   Prepared_View : Prepared_Buffer := (Ready => False);
   function Prepared return Prepared_Buffer is (Prepared_View);
   function Prepared_Submission (Part : Intel_GPU_Submission_Backing.Region)
     return Prepared_Submission_Buffer is
   begin
      if not Prepared_View.Ready or else not Intel_GPU_Submission_Backing.Valid_Layout
      then return (Ready => False); end if;
      return (Ready => True,
        DMA_Address => Prepared_View.DMA_Address + Intel_GPU_Submission_Backing.Offsets (Part),
        CPU_Address => Prepared_View.CPU_Address + Intel_GPU_Submission_Backing.Offsets (Part),
        Region_Bytes => Intel_GPU_Submission_Backing.Sizes (Part));
   end Prepared_Submission;
   function Prepared_CT return Prepared_CT_Buffer is
   begin
      if not Prepared_View.Ready then return (Ready => False); end if;
      return (Ready => True,
        DMA_Address => Prepared_View.DMA_Address + CT_Region_Offset,
        CPU_Address => Prepared_View.CPU_Address + CT_Region_Offset,
        Region_Bytes => CT_Region_Bytes);
   end Prepared_CT;
   function Prepared_Log return Prepared_Log_Buffer is
   begin
      if not Prepared_View.Ready then return (Ready => False); end if;
      return (Ready => True,
        DMA_Address => Prepared_View.DMA_Address + Log_Region_Offset,
        CPU_Address => Prepared_View.CPU_Address + Log_Region_Offset,
        Region_Bytes => Log_Region_Bytes);
   end Prepared_Log;
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
      if Source = System.Null_Address or else Bytes = 0 or else
        Bytes > Firmware_Region_Bytes or else
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
      if not Intel_GPU_DMA_Cache.Flush_Range (Virtual, Capacity) then
         return "cache-flush-unavailable (retained)";
      end if;
      Prepared_View := (Ready => True, DMA_Address => Physical,
        CPU_Address => Virtual, Allocation_Bytes => Capacity, Content_Bytes => Bytes);
      return "prepared-retained (NOT GPU-published)";
   end Prepare;
end Intel_GPU_Firmware_Buffer;
