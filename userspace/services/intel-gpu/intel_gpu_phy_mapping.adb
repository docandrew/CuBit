with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_PHY_Pages;
package body Intel_GPU_PHY_Mapping is
   Attempted, Mapped : Boolean := False;
   function Ready return Boolean is (Mapped);
   function Prepare (Owner : Boolean; Register_Base : Unsigned_64) return String is
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Started, Previous, Now : Unsigned_64;
      Granted : Boolean;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if Attempted then return "already-attempted"; end if;
      Attempted := True;
      if not Owner or else Register_Base = 0 or else Register_Base mod 4096 /= 0 or else
        Register_Base > Unsigned_64'Last - 16#163000#
      then return "owner-or-BAR-invalid"; end if;
      Started := syscall (SYSCALL_GETTIME);
      if Started = Unsigned_64'Last then return "clock-unavailable"; end if;
      Previous := Started;
      for Index in Intel_GPU_PHY_Pages.Page_Index loop
         declare
            Token : constant Unsigned_64 := 16#4947_0050# + Unsigned_64 (Index);
         begin
            Msg.tag := (Intel_GPU_PHY_Pages.Request_Label, 1, 0, 0);
            Msg.words := [Unsigned_64 (Index), 0, 0, 0];
            if not capSubmit (15, Msg, Token) then return "submit-failed"; end if;
            Granted := False;
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
                     -- Broker's explicit startup deferral means no grant yet.
                     if not capSubmit (15, Msg, Token) then return "retry-failed"; end if;
                  else
                     Granted := Receipt.status = COMPLETION_OK and then
                       Receipt.msg.tag = (16#F000#, 0, 0, 0) and then
                       Receipt.msg.words = [0, 0, 0, 0];
                     exit;
                  end if;
               end if;
               Activity := Wait_For_Activity_Until (Now + 1);
            end loop;
            if not Granted then return "grant-denied-or-poll-exhausted"; end if;
            if syscall (SYSCALL_MAP_DEVICE,
                Register_Base + Intel_GPU_PHY_Pages.Offset (Index),
                Intel_GPU_PHY_Pages.Virtual_Base + Unsigned_64 (Index) * 4096, 1, 0) /= 0
            then return "map-denied (partial mappings retained)"; end if;
         end;
      end loop;
      Mapped := True;
      return "ready (NOT PHY restore)";
   end Prepare;
end Intel_GPU_PHY_Mapping;
