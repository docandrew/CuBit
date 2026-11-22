with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
with Intel_GPU_Display_Enable;
with Intel_GPU_Display_Lease;
procedure Display_Enable_Tests is
   procedure Composed_Reuse (W : Request_Well; Inherited : Boolean) is
      type Single_Well is (Selected);
      Control : Unsigned_32 :=
        (if Inherited then Request_Mask (W) or State_Mask (W) else 0);
      Hardware_Calls, Saved_Calls : Natural := 0;
      function Read (Offset : Unsigned_32) return Unsigned_32 is
      begin
         Hardware_Calls := Hardware_Calls + 1;
         return (case Offset is when 16#45404# => Control,
           when 16#42000# => 16#0FFF_FFFF#, when 16#46430# => 0,
           when others => raise Program_Error);
      end Read;
      procedure Write (Offset, Value : Unsigned_32; Success : out Boolean) is
      begin
         Hardware_Calls := Hardware_Calls + 1;
         if Offset = 16#45404# then Control := Value or State_Mask (W);
         else pragma Assert (Offset = 16#46430#); end if;
         Success := True;
      end Write;
      function Now return Unsigned_64 is (0);
      procedure Pause is null;
      procedure Hook (Success : out Boolean) is
      begin Hardware_Calls := Hardware_Calls + 1; Success := True; end Hook;
      package Enable is new Intel_GPU_Display_Enable (W, Read, Write, Now, Pause, Hook, Hook);
      use type Enable.Result;
      function Valid (Mask : Unsigned_64) return Boolean is (Mask = 1);
      procedure Hold (Item : Single_Well; Added, Success : out Boolean) is
         Status : Enable.Result;
      begin
         pragma Assert (Item = Selected);
         Enable.Execute (True, 3, Added, Status);
         Success := Status = Enable.Ready;
      end Hold;
      procedure Drop (Item : Single_Well; Added : Boolean; Success : out Boolean) is
         Status : Enable.Result;
      begin
         pragma Assert (Item = Selected and Added /= Inherited);
         Enable.Release (Status);
         Success := Status = Enable.Released;
      end Drop;
      package Lease is new Intel_GPU_Display_Lease (Single_Well, Valid, Hold, Drop);
      OK : Boolean;
   begin
      for Cycle in 1 .. 3 loop
         Lease.Acquire (1, OK);
         pragma Assert (OK, "per-well software reference was not released");
         Saved_Calls := Hardware_Calls;
         Lease.Release (OK);
         pragma Assert (OK);
         if Inherited then pragma Assert (Hardware_Calls = Saved_Calls); end if;
         pragma Assert (((Control and Request_Mask (W)) /= 0) = Inherited);
      end loop;
   end Composed_Reuse;
   type Release_Fault is (Clean, Pre_Error, Read_Error, Store_Error,
                         Readback_Error, Stuck_Request, Lost_Request);
   procedure Release_Run (W : Request_Well; Inherited : Boolean; Bad : Release_Fault) is
      Control : Unsigned_32 := 16#0100_0000# or
        (if Inherited then Request_Mask (W) or State_Mask (W) else 0);
      Releasing : Boolean := False;
      Reads, Writes, Pre_Calls : Natural := 0;
      function Read (Offset : Unsigned_32) return Unsigned_32 is
      begin
         if Releasing then
            pragma Assert (Offset = 16#45404#);
            Reads := Reads + 1;
            if (Reads = 1 and Bad = Read_Error) or (Reads = 2 and Bad = Readback_Error) then
               return Unsigned_32'Last;
            end if;
            if Reads = 1 and Bad = Lost_Request then return Control and not Request_Mask (W); end if;
         end if;
         return (case Offset is when 16#45404# => Control,
           when 16#42000# => 16#0FFF_FFFF#, when 16#46430# => 0,
           when others => raise Program_Error);
      end Read;
      procedure Write (Offset, Value : Unsigned_32; Success : out Boolean) is
      begin
         if Releasing then
            pragma Assert (Offset = 16#45404# and Value = (Control and not Request_Mask (W)));
            Writes := Writes + 1;
            if Bad /= Stuck_Request then Control := Value; end if;
            Success := Bad /= Store_Error;
         else
            if Offset = 16#45404# then Control := Value or State_Mask (W);
            else pragma Assert (Offset = 16#46430# and Value = 16#8000#); end if;
            Success := True;
         end if;
      end Write;
      function Now return Unsigned_64 is (0);
      procedure Pause is null;
      procedure Post (Success : out Boolean) is
      begin Success := True; end Post;
      procedure Pre (Success : out Boolean) is
      begin
         pragma Assert (Releasing and not Inherited);
         Pre_Calls := Pre_Calls + 1;
         Success := Bad /= Pre_Error;
      end Pre;
      package Enable is new Intel_GPU_Display_Enable (W, Read, Write, Now, Pause, Post, Pre);
      use type Enable.Result;
      Status, Expected : Enable.Result;
      Added : Boolean;
      Count : Natural;
   begin
      Enable.Release (Status);
      pragma Assert (Status = Enable.Rejected and Pre_Calls = 0);
      Enable.Execute (True, 3, Added, Status);
      pragma Assert (Status = Enable.Ready and Added /= Inherited);
      Releasing := True;
      Enable.Release (Status);
      Expected := (if Inherited then Enable.Released else
        (case Bad is when Clean => Enable.Released, when Pre_Error => Enable.Pre_Disable_Failed,
         when Read_Error | Readback_Error => Enable.Invalid_MMIO,
         when Store_Error => Enable.Write_Failed,
         when Stuck_Request | Lost_Request => Enable.Request_Changed));
      pragma Assert (Status = Expected);
      if Inherited then pragma Assert (Reads = 0 and Writes = 0 and Pre_Calls = 0); end if;
      if Status = Enable.Released then
         -- Our request can be gone while the STATE bit remains asserted.
         pragma Assert ((Control and State_Mask (W)) /= 0);
         Releasing := False;
         Enable.Execute (True, 3, Added, Status);
         pragma Assert (Status = Enable.Ready and Added /= Inherited);
      else
         Count := Reads + Writes + Pre_Calls;
         Enable.Release (Status);
         pragma Assert (Status = Enable.Rejected);
         Enable.Execute (True, 3, Added, Status);
         pragma Assert (Status = Enable.Rejected and Reads + Writes + Pre_Calls = Count);
      end if;
   end Release_Run;
   type Fault is (None, Sentinel, Write_Error, Ack_Deadline, Ack_Stalled,
                  Fuse_Stalled, Clock_Missing, Post_Error, Changed_Request,
                  PG0_Stalled, Clock_Backwards, Ack_Read_Expired, Late_Sentinel);
   procedure Run (W : Request_Well; Inherited : Boolean; Bad : Fault) is
      Control : Unsigned_32 := 16#0100_0000# or
        (if Inherited then Request_Mask (W) or State_Mask (W) else 0);
      Calls, Control_Reads, Request_Writes, Post_Calls : Natural := 0;
      Workaround_Done, PG0_Checked : Boolean := False;
      Clock : Unsigned_64 := 0;
      Clock_Reads : Natural := 0;
      function Read (Offset : Unsigned_32) return Unsigned_32 is
      begin
         Calls := Calls + 1;
         if Bad = Sentinel then return Unsigned_32'Last; end if;
         case Offset is
            when 16#45404# =>
               Control_Reads := Control_Reads + 1;
               if Control_Reads = 2 and Bad = Changed_Request then
                  Control := Control xor Request_Mask (W);
               end if;
               if Control_Reads >= 3 and Bad in Ack_Deadline | Ack_Stalled then
                  return Control and not State_Mask (W);
               end if;
               if Control_Reads >= 3 and Bad = Ack_Read_Expired then
                  Clock := Clock + 1_000;
               end if;
               return Control;
            when 16#46430# =>
               pragma Assert (W = PW1);
               return 16#102#;
            when 16#42000# =>
               if Bad = Late_Sentinel then return Unsigned_32'Last; end if;
               if W = PW1 and not PG0_Checked then
                  pragma Assert (Workaround_Done);
                  if Bad = PG0_Stalled then return 0; end if;
                  PG0_Checked := True;
                  return 16#0800_0000#;
               end if;
               if Bad = Fuse_Stalled then return 0; end if;
               -- Return ONLY this well's distribution bit, not a blanket
               -- all-ready value that could conceal a wrong implementation mask.
               return Shift_Left (Unsigned_32'(1), 26 -
                 (case W is when PW1 => 0, when PW2 => 1, when PWA => 5,
                  when PWB => 6, when PWC => 7, when PWD => 8));
            when others => raise Program_Error;
         end case;
      end Read;
      procedure Write (Offset, Value : Unsigned_32; Success : out Boolean) is
      begin
         Calls := Calls + 1;
         case Offset is
            when 16#46430# =>
               pragma Assert (W = PW1 and Value = 16#8102#);
               Workaround_Done := True;
            when 16#45404# =>
               pragma Assert (not Inherited and (W /= PW1 or PG0_Checked));
               pragma Assert (Value = (Control or Request_Mask (W)));
               Request_Writes := Request_Writes + 1;
               Control := Value or State_Mask (W);
            when others => raise Program_Error;
         end case;
         Success := Bad /= Write_Error;
      end Write;
      function Now return Unsigned_64 is
      begin
         Clock_Reads := Clock_Reads + 1;
         return (if Bad = Clock_Missing then Unsigned_64'Last
           elsif Bad = Clock_Backwards then 100 - Unsigned_64 (Clock_Reads)
           else Clock);
      end Now;
      procedure Pause is
      begin
         if Bad = Ack_Deadline then Clock := Clock + 1_000; end if;
      end Pause;
      procedure Post (Success : out Boolean) is
      begin
         Calls := Calls + 1;
         Post_Calls := Post_Calls + 1;
         Success := Bad /= Post_Error;
      end Post;
      procedure Pre (Success : out Boolean) is
      begin
         pragma Assert (False, "enable-only test must not invoke Pre_Disable");
         Success := False;
      end Pre;
      package Enable is new Intel_GPU_Display_Enable (W, Read, Write, Now, Pause, Post, Pre);
      use type Enable.Result;
      Added : Boolean;
      Status : Enable.Result;
      Before : Natural;
      Expected : Enable.Result;
   begin
      Enable.Execute (False, 3, Added, Status);
      pragma Assert (Status = Enable.Rejected and Calls = 0 and not Added);
      Enable.Execute (True, 3, Added, Status);
      Expected := (case Bad is
         when None => Enable.Ready,
         when Sentinel | Late_Sentinel => Enable.Invalid_MMIO,
         when Write_Error =>
           (if Inherited and W /= PW1 then Enable.Ready else Enable.Write_Failed),
         when Ack_Deadline | Ack_Read_Expired => Enable.Deadline_Expired,
         when Ack_Stalled | Fuse_Stalled => Enable.Poll_Exhausted,
         when Clock_Missing | Clock_Backwards => Enable.Invalid_Clock,
         when PG0_Stalled => (if W = PW1 then Enable.Poll_Exhausted else Enable.Ready),
         when Post_Error => Enable.Post_Enable_Failed,
         when Changed_Request => Enable.Request_Changed);
      pragma Assert (Status = Expected);
      pragma Assert (Added = (Request_Writes /= 0));
      pragma Assert (not Inherited or Request_Writes = 0);
      pragma Assert (Post_Calls = (if Status in Enable.Ready | Enable.Post_Enable_Failed then 1 else 0));
      Before := Calls;
      Enable.Execute (True, 3, Added, Status);
      pragma Assert (Status = Enable.Rejected and Calls = Before and not Added);
   end Run;
begin
   for W in Well loop
      if W /= DC_Off then
         for Inherited in Boolean loop
            Composed_Reuse (W, Inherited);
            for Bad in Fault loop Run (W, Inherited, Bad); end loop;
            for Bad in Release_Fault loop Release_Run (W, Inherited, Bad); end loop;
         end loop;
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("Display enable PASS: 156 well/inheritance/failure combinations");
   Ada.Text_IO.Put_Line ("Display release PASS: 84 well/inheritance/failure combinations");
end Display_Enable_Tests;
