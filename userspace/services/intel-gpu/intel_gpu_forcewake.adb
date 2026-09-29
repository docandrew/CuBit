package body Intel_GPU_Forcewake is
   use Interfaces;
   Set_Request : constant Unsigned_32 := 16#0001_0001#;
   Clear_Request : constant Unsigned_32 := 16#0001_0000#;
   function State (Object : Lease) return Ownership_State is (Object.Current);

   procedure Await_Ack
     (Expected : Unsigned_32; Poll_Limit : Positive;
      Started, Timeout : Unsigned_64; Last_Time : in out Unsigned_64;
      Status : out Result)
   is
      Value : Unsigned_32;
      function Within_Budget return Boolean is
         Now : constant Unsigned_64 := Now_Milliseconds;
      begin
         if Now < Last_Time then
            Status := Invalid_Clock;
            return False;
         end if;
         Last_Time := Now;
         if Now - Started >= Timeout then
            Status := Timed_Out;
            return False;
         end if;
         return True;
      end Within_Budget;
   begin
      Status := Poll_Exhausted;
      for Attempt in 1 .. Poll_Limit loop
         if not Within_Budget then
            return;
         end if;
         Value := Read_32 (Ack_Register);
         --  A delayed MMIO access must not turn an expired wait into success.
         if not Within_Budget then
            return;
         end if;
         if Value = Unsigned_32'Last then
            Status := Invalid_MMIO;
            return;
         elsif (Value and 1) = Expected then
            Status := Ready;
            return;
         end if;
         if Attempt < Poll_Limit then
            Pause;
         end if;
      end loop;
   end Await_Ack;

   procedure Try_Recovery (Expected : Unsigned_32; Status : in out Result) is
      Recovered : Boolean := False;
   begin
      if Status in Timed_Out | Poll_Exhausted then
         Recover_Ack (Expected, Recovered);
         if Recovered then Status := Ready; end if;
      end if;
   end Try_Recovery;

   procedure Acquire
     (Object : in out Lease; Poll_Limit : Positive; Status : out Result;
      Timeout_Milliseconds : Unsigned_64 := 50)
   is
      Started, Last_Time : Unsigned_64;
   begin
      if Object.Current /= Idle then
         Status := Invalid_State;
         return;
      end if;
      --  Latch before any callback. Even a callback exception cannot leave
      --  a reusable lease whose hardware ownership is uncertain.
      Object.Current := Faulted;
      Started := Now_Milliseconds;
      Last_Time := Started;
      Await_Ack (0, Poll_Limit, Started, Timeout_Milliseconds, Last_Time, Status);
      Try_Recovery (0, Status);
      if Status /= Ready then
         return;
      end if;
      Write_32 (Request_Register, Set_Request);
      Await_Ack (1, Poll_Limit, Started, Timeout_Milliseconds, Last_Time, Status);
      Try_Recovery (1, Status);
      if Status /= Ready then
         Write_32 (Request_Register, Clear_Request);
      else
         Object.Current := Held;
      end if;
   end Acquire;

   procedure Release
     (Object : in out Lease; Poll_Limit : Positive; Status : out Result;
      Timeout_Milliseconds : Unsigned_64 := 50)
   is
      Started, Last_Time : Unsigned_64;
   begin
      if Object.Current /= Held then
         Status := Invalid_State;
         return;
      end if;
      Object.Current := Faulted;
      Started := Now_Milliseconds;
      Last_Time := Started;
      Write_32 (Request_Register, Clear_Request);
      Await_Ack (0, Poll_Limit, Started, Timeout_Milliseconds, Last_Time, Status);
      Try_Recovery (0, Status);
      if Status = Ready then
         Object.Current := Idle;
      end if;
   end Release;
end Intel_GPU_Forcewake;
