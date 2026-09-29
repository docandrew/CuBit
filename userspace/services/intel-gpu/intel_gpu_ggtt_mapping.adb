with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_GGTT_Access;
package body Intel_GPU_GGTT_Mapping is
   Attempted, Mapped : Boolean := False;
   Mapped_Bytes : Unsigned_64 := 0;
   function Ready return Boolean is (Mapped);
   function Bytes return Unsigned_64 is (Mapped_Bytes);

   function Prepare
     (Owner, Reset_Complete : Boolean;
      Register_Base, Table_Bytes : Unsigned_64) return String
   is
      Token : constant Unsigned_64 := 16#4947_0033#;
      Chunk_Bytes : constant Unsigned_64 := 2 * 1024 * 1024;
      Physical, Started, Previous, Now : Unsigned_64;
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Granted : Boolean := False;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if Attempted then return "already-attempted"; end if;
      Attempted := True;
      if not Owner or else not Reset_Complete or else Register_Base = 0 or else
        Register_Base mod 4096 /= 0 or else
        Register_Base > Unsigned_64'Last - 16#100_0000# or else
        Table_Bytes not in 2_097_152 | 4_194_304 | 8_388_608
      then return "owner-reset-or-extent-invalid"; end if;
      Physical := Register_Base + 16#80_0000#;
      Started := syscall (SYSCALL_GETTIME);
      if Started = Unsigned_64'Last then return "clock-unavailable"; end if;
      Previous := Started;
      Msg.tag := (Intel_GPU_GGTT_Access.Write_Request_Label, 0, 0, 0);
      if not capSubmit (15, Msg, Token) then return "submit-failed"; end if;
      for Poll in 1 .. 30_000 loop
         Now := syscall (SYSCALL_GETTIME);
         if Now = Unsigned_64'Last or else Now < Previous or else
           Now - Started >= 30_000
         then return "grant-timeout-or-invalid-clock"; end if;
         Previous := Now;
         if Poll_Completion (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               -- Only explicit startup-busy is retryable. Denial, malformed
               -- replies, timeout or lost replies never retry a consumed grant.
               if not capSubmit (15, Msg, Token) then return "retry-failed"; end if;
            else
               Granted := Receipt.status = COMPLETION_OK and then
                 Receipt.msg.tag = (16#F000#, 2, 0, 0) and then
                 Receipt.msg.words = [Physical, Table_Bytes, 0, 0];
               exit;
            end if;
         end if;
         Activity := Wait_For_Activity_Until (Now + 1);
      end loop;
      if not Granted then return "grant-denied-or-poll-exhausted"; end if;
      -- UC/NX MAP_DEVICE aliases, distinct from the read-only inspection view.
      -- Every supported size is a multiple of 2MiB. Keep calls below the
      -- kernel's 1024-page limit and retain partial maps if a later call fails.
      for Chunk in Unsigned_64 range 0 .. Table_Bytes / Chunk_Bytes - 1 loop
         if syscall (SYSCALL_MAP_DEVICE, Physical + Chunk * Chunk_Bytes,
           Virtual_Base + Chunk * Chunk_Bytes, 512, 0) /= 0
         then return "map-denied (partial mappings retained)"; end if;
      end loop;
      Mapped_Bytes := Table_Bytes;
      Mapped := True;
      return "ready (NO PTE writes)";
   end Prepare;
end Intel_GPU_GGTT_Mapping;
