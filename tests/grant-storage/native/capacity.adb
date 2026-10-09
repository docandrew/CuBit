with Interfaces; use Interfaces;
with System; use type System.Address;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;

procedure Capacity is
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   use type R.Reference;
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Ignore : Unsigned_64;
   -- Spread aliases over 128 pages: the independent per-frame pin ceiling
   -- is 127, so one page cannot exercise a 4096-record namespace.
   type Bytes is array (0 .. 128 * 4096 - 1) of Unsigned_8;
   Buffer : Bytes := (others => 16#A3#) with Alignment => 4096, Volatile;
   References : array (0 .. 4095) of G.Grant_Reference;
   Addresses : array (References'Range) of System.Address;
   Old, Fresh : G.Grant_Reference;
   OK : Boolean;
   procedure Check (Value : Boolean; Detail : String) is
   begin
      if not Value then
         debugPrint ("TEST: FAIL grant capacity: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
begin
   debugPrint ("grant capacity: lazy native records 4096 slots (NO GPU)" & ASCII.LF);
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 1, PID, 0, 3, 15)
     /= Unsigned_64'Last, "self endpoint");
   for I in References'Range loop
      G.Create_Via_Capability (15, Buffer ((I mod 128) * 4096)'Address,
        1, False, References (I), OK);
      if not OK then debugPrint ("grant capacity: failed index=" & Natural'Image (I) & ASCII.LF); end if;
      Check (OK, "create through metadata growth");
      --  Slots are global (KERN-003 step 2): each grant its own, never 0.
      Check (References (I).slot /= 0 and then
             (for all J in References'First .. I - 1 =>
                References (J).slot /= References (I).slot), "distinct slots");
      Check (R.Decode (R.Encode (References (I))) = References (I), "wire roundtrip");
      G.Acquire_Via_Capability (15, References (I), 0, 4096,
        G.Read_Access, Addresses (I), OK);
      Check (OK, "acquire through metadata growth");
   end loop;
   G.Create_Via_Capability (15, Buffer'Address, 1, False, Fresh, OK);
   Check (not OK, "the owner's grant quota fails closed");
   for I in References'Range loop
      declare Alias : Unsigned_8 with Import, Address => Addresses (I), Volatile; begin
         Check (Alias = 16#A3#, "all acquisitions survive growth and exhaustion");
      end;
   end loop;
   debugPrint ("grant capacity: 4096 live readers and exhaustion PASS" & ASCII.LF);
   -- Retire a slot exactly at a record-block boundary while all others live.
   Old := References (64);
   G.Revoke (Old, OK); Check (OK, "revoke boundary");
   G.Create_Via_Capability (15, Buffer'Address, 1, False, Fresh, OK);
   Check (not OK, "held retiring slot is not free capacity");
   G.Return_Acquisition (Old, OK); Check (OK, "return boundary");
   G.Create_Via_Capability (15, Buffer'Address, 1, False, Fresh, OK);
   Check (OK and then Fresh.slot = Old.slot and then Fresh.generation /= Old.generation,
     "retired boundary capacity reused with new identity");
   G.Acquire_Via_Capability (15, Fresh, 0, 4096, G.Read_Access, Addresses (64), OK);
   Check (OK, "replacement acquisition");
   G.Return_Acquisition (Old, OK); Check (not OK, "old return denied");
   References (64) := Fresh;
   for I in reverse References'Range loop
      G.Revoke (References (I), OK); Check (OK, "revoke all");
      G.Return_Acquisition (References (I), OK); Check (OK, "return all");
      Check (G.Retirement_Confirmed (References (I)), "retirement confirmed");
   end loop;
   debugPrint ("TEST: PASS grant capacity 4096 readers exhausted boundary reused all retired (NO GPU)" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end Capacity;
