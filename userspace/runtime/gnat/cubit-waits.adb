pragma Ada_2022;
with CuBit.Kernel_ABI;
with Notification_ABI;
with CuBit.Kernel_Calls;
with CuBit.Monotonic;
with CuBit.System_Facts;
with System.Machine_Code;

package body CuBit.Waits is

   package NA renames Notification_ABI;

   Microseconds_Per_Millisecond : constant := 1_000;

   function Wait (Sources, Deadline : Unsigned_64; A, B, C : Unsigned_64 := 0)
     return Unsigned_64 is
     (CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Wait, Sources, Deadline, A, B, C));

   procedure Sleep_Until (Instant : Unsigned_64) is
      Ignore : constant Unsigned_64 := Wait (W.Timer_Sources, W.Forever, Instant);
   begin
      null;
   end Sleep_Until;

   function Sleep_For (Milliseconds : Unsigned_64) return Unsigned_64 is
      Now_Us : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      if Milliseconds = 0 or else Now_Us = CuBit.Monotonic.Unavailable_Us
        or else Milliseconds > (Unsigned_64'Last - Now_Us) / Microseconds_Per_Millisecond
      then
         return 0;
      end if;
      Sleep_Until (Now_Us + Milliseconds * Microseconds_Per_Millisecond);
      return 0;
   end Sleep_For;

   procedure Sleep_For (Milliseconds : Unsigned_64) is
      Ignore : constant Unsigned_64 := Sleep_For (Milliseconds);
   begin
      null;
   end Sleep_For;

   --  A private word waits in its process's own table (IPC revision 2,
   --  Notification_ABI): the Wait and Wake calls, deadline in microseconds.
   function To_Microseconds (Deadline : Unsigned_64) return Unsigned_64 is
     (if Deadline = Now or else Deadline = Forever then Deadline
      elsif Deadline > (Forever - 1) / Microseconds_Per_Millisecond then Forever
      else Deadline * Microseconds_Per_Millisecond);

   function Private_Result (Result : Unsigned_64) return Unsigned_64 is
     (if Result = NA.Woken then W.Futex_Woken
      elsif Result = NA.Retry then W.Futex_Retry
      elsif Result = NA.Timed_Out then W.Futex_Timed_Out
      else W.Futex_Fault);

   function Private_Wait (Word, Expected, Deadline : Unsigned_64) return Unsigned_64 is
     (Private_Result
        (CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Wait_One, NA.Private_Word, Word, Expected,
                                  To_Microseconds (Deadline))));

   function Futex_Wait (Word : System.Address; Expected, Deadline : Unsigned_64)
     return Unsigned_64 is
     (Private_Wait (Unsigned_64 (System.Storage_Elements.To_Integer (Word)), Expected, Deadline));

   --  The kernel's limit, read once (the facts page does not change it
   --  after boot but to withdraw it).
   Pause_Limit : Unsigned_64 := Unsigned_64'Last;

   C0_1 : constant Unsigned_32 := 1;   --  UMWAIT control: the lighter C0.1

   function Read_TSC return Unsigned_64;
   function Read_TSC return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      System.Machine_Code.Asm ("rdtsc", Outputs => (Unsigned_32'Asm_Output ("=a", Low),
                                                    Unsigned_32'Asm_Output ("=d", High)),
                               Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end Read_TSC;

   --  Wait in user mode, at most Cycles, until the word at Where may have
   --  changed. True when it no longer holds Expected.
   function Paused_Change (Where : System.Address; Expected, Cycles : Unsigned_64) return Boolean;
   function Paused_Change (Where : System.Address; Expected, Cycles : Unsigned_64) return Boolean is
      Word : Unsigned_32 with Import, Volatile, Async_Writers, Address => Where;
      Until_TSC : constant Unsigned_64 := Read_TSC + Cycles;
   begin
      System.Machine_Code.Asm ("umonitor %0", Inputs => System.Address'Asm_Input ("r", Where),
                               Clobber => "memory", Volatile => True);
      if Unsigned_64 (Word) /= Expected then
         return True;
      end if;
      System.Machine_Code.Asm
        ("umwait %0",
         Inputs => (Unsigned_32'Asm_Input ("r", C0_1),
                    Unsigned_32'Asm_Input ("a", Unsigned_32 (Until_TSC and 16#FFFF_FFFF#)),
                    Unsigned_32'Asm_Input ("d", Unsigned_32 (Shift_Right (Until_TSC, 32)))),
         Clobber => "memory, cc", Volatile => True);
      return Unsigned_64 (Word) /= Expected;
   end Paused_Change;

   function Futex_Wait (Name : Futex_Name; Expected, Deadline : Unsigned_64)
     return Unsigned_64 is
   begin
      if Pause_Limit = Unsigned_64'Last then
         Pause_Limit := CuBit.System_Facts.User_Wait_Cycles;
      end if;
      if Pause_Limit /= 0 and then Deadline /= Now
        and then System."/=" (Name.Where, System.Null_Address)
        and then Paused_Change (Name.Where, Expected, Unsigned_64'Min (Pause_Cycles, Pause_Limit))
      then
         return W.Futex_Retry;
      end if;
      if not Name.Shared then
         return Private_Wait (Name.Word, Expected, Deadline);
      end if;
      return Wait (W.Futex_Sources + W.Shared_Futex, Deadline, Name.Word, Expected);
   end Futex_Wait;

   function Futex_Wake (Name : Futex_Name; Count : Unsigned_64) return Unsigned_64 is
     (if Name.Shared
      then CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Futex_Wake, Name.Word, Count, FA.Shared_Space)
      else CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Wake, NA.Private_Word, Name.Word, Count));

   function Hand_Off (Peer, Mine : Futex_Name; Expected, Deadline : Unsigned_64)
     return Unsigned_64 is
     (CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Wait,
         W.Futex_Sources + W.Wake_First
           + (if Mine.Shared then W.Shared_Futex else 0)
           + (if Peer.Shared then W.Wake_Shared else 0),
         Deadline, Mine.Word, Expected, 0, Peer.Word));

   function Wake_Peer
     (Grant : GR.Reference; Sync : Boolean := False; Urgent : Boolean := False) return Boolean is
     (CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Futex_Wake, GR.Encode (Grant), 1, FA.Peer_Space,
         (if Sync then FA.Sync_Hint else 0) or (if Urgent then FA.Urgent_Hint else 0))
      /= FA.Wake_Refused);

end CuBit.Waits;
