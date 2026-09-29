with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_ADS_Backing; use Intel_GPU_ADS_Backing;
with Intel_GPU_ADS_Initialization;
with Intel_GPU_ADS_Materialize;
with Intel_GPU_Native_Reset;
with Intel_GPU_DMA_Cache;
with Intel_GPU_GuC_Parameters;
package body Intel_GPU_ADS_Buffer is
   Attempted : Boolean := False;
   View : Prepared_Backing;
   Initialization_Attempted : Boolean := False;
   Initialized_Address : Unsigned_64 := 0;
   function Initialized_GPU_Start return Unsigned_64 is (Initialized_Address);
   function Prepared return Prepared_Backing is (View);
   procedure Initialize (GPU_Start, Bytes : Unsigned_64; Success : out Boolean) is
      Limit : constant Unsigned_64 := Intel_GPU_GuC_Parameters.Runtime_GGTT_Limit;
   begin
      Success := False;
      if Initialization_Attempted then return; end if;
      Initialization_Attempted := True;
      if not View.Ready or else View.CPU_Address /= CPU_Address or else
        View.Capacity /= Capacity or else not Valid_Physical (View.DMA_Address) or else
        not Intel_GPU_Native_Reset.ADS_Observed or else Bytes /= Capacity or else
        GPU_Start = 0 or else GPU_Start mod 4096 /= 0 or else
        GPU_Start >= Limit or else Bytes > Limit - GPU_Start
      then return; end if;
      declare
         Image : constant Intel_GPU_ADS_Initialization.Prepared_Image :=
           Intel_GPU_ADS_Initialization.Prepare
             (Intel_GPU_Native_Reset.ADS_Inventory,
              Intel_GPU_Native_Reset.ADS_Topology,
              Intel_GPU_Native_Reset.ADS_Doorbell,
              Intel_GPU_Native_Reset.ADS_Doorbell, 16#1000000#,
              GPU_Start, Bytes, 16#801000#);
         Buffer : Intel_GPU_ADS_Materialize.Bytes (0 .. Natural (Capacity) - 1)
           with Import, Address => To_Address (Integer_Address (CPU_Address));
      begin
         Intel_GPU_ADS_Materialize.Write (Image, Buffer, Success);
      end;
      if not Success then return; end if;
      Success := Intel_GPU_DMA_Cache.Flush_Range (CPU_Address, Capacity);
      if Success then Initialized_Address := GPU_Start; end if;
   end Initialize;
   function Prepare return String is
      Token : constant Unsigned_64 := 16#4947_0006#;
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Start : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Last_Time : Unsigned_64 := Start;
      Now : Unsigned_64;
      Received : Boolean := False;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if Attempted then return "already-attempted"; end if;
      Attempted := True;
      if Start = Unsigned_64'Last then return "clock-unavailable"; end if;
      Msg.tag := (Request_Label, 0, 0, 0);
      if not capSubmit (15, Msg, Token) then return "submit-failed"; end if;
      for Poll in 1 .. 30_000 loop
         Now := syscall (SYSCALL_GETTIME);
         if Now = Unsigned_64'Last or else Now < Last_Time or else Now - Start >= 30_000 then
            return "timeout-or-invalid-clock";
         end if;
         Last_Time := Now;
         if Poll_Completion (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               if not capSubmit (15, Msg, Token) then return "retry-failed"; end if;
            else
               if Receipt.status /= COMPLETION_OK or else
                 Receipt.msg.tag /= (16#F000#, 3, 0, 0) or else
                 Receipt.msg.words (1 .. 3) /= [CPU_Address, Capacity, 0] or else
                 not Valid_Physical (Receipt.msg.words (0))
               then return "allocation-denied"; end if;
               Received := True;
               exit;
            end if;
         end if;
         Activity := Wait_For_Activity_Until (Now + 1);
      end loop;
      if not Received then return "completion-poll-exhausted"; end if;
      declare
         type Storage is array (Unsigned_64 range 0 .. Capacity / 8 - 1) of Unsigned_64;
         Buffer : Storage with Import, Volatile,
           Address => To_Address (Integer_Address (CPU_Address));
      begin
         for I in Buffer'Range loop Buffer (I) := 0; end loop;
         for I in Buffer'Range loop
            if Buffer (I) /= 0 then return "readback-failed (retained)"; end if;
         end loop;
      end;
      View := (True, Receipt.msg.words (0), CPU_Address, Capacity);
      return "16MiB zeroed-retained (NOT initialized ADS; NOT GPU-published)";
   end Prepare;
end Intel_GPU_ADS_Buffer;
