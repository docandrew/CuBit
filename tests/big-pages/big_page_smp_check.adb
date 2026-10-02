with Interfaces; use Interfaces;
with System;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Virtmem; use Virtmem;
with BuddyAllocator;
with TLB_Shootdown;
with TLB_Reclamation;
with Time;
with x86;
with TextIO;
package body Big_Page_SMP_Check is
   use type System.Address;
   VA : constant VirtAddress := 16#0000_6000_0000_0000#;
   Flags : constant Unsigned_64 := PG_PRESENT or PG_WRITABLE or PG_NXE;
   Root_Address : PhysAddress := 0;
   Old_Block, New_Block : System.Address;
   type Marks is array (TLB_Reclamation.CPU_Index) of Boolean with Atomic_Components;
   Armed, Done : Marks := (others => False);
   Replaced : Boolean := False with Atomic;
   procedure Map_Big is new mapBigPage (BuddyAllocator.allocFrame);
   procedure Check (OK : Boolean) is
   begin
      if not OK then raise Program_Error with "big-page SMP check failed"; end if;
   end Check;
   procedure Pause (Started : Unsigned_64) is
   begin
      if x86.rdtsc - Started > Time.tscFrequencyHz * 5 then
         raise Program_Error with "big-page SMP check timeout";
      end if;
      System.Machine_Code.Asm ("pause", Volatile => True);
   end Pause;
   procedure Prepare is
      Source : P4 with Import, Address => To_Address (P2V (x86.getCR3));
      OK : Boolean;
   begin
      BuddyAllocator.allocFrame (Root_Address);
      BuddyAllocator.alloc (9, Old_Block);
      BuddyAllocator.alloc (9, New_Block);
      Check (Root_Address /= 0 and Old_Block /= System.Null_Address and
             New_Block /= System.Null_Address);
      declare
         Root : P4 with Import, Address => To_Address (P2V (Root_Address));
      begin
         for I in PageTableIndex loop Root (I) := Source (I); end loop;
         Check (not Root (getP4Index (VA)).present);
         for I in 0 .. 511 loop
            declare
               A : Unsigned_64 with Import, Volatile,
                 Address => To_Address (To_Integer (Old_Block) + Integer_Address (I) * 4096);
               B : Unsigned_64 with Import, Volatile,
                 Address => To_Address (To_Integer (New_Block) + Integer_Address (I) * 4096);
            begin
               A := 16#1000# + Unsigned_64 (I);
               B := 16#2000# + Unsigned_64 (I);
            end;
         end loop;
         Map_Big (V2P (Old_Block), VA, Flags, Root, OK); Check (OK);
      end;
   end Prepare;
   procedure Secondary (CPU : Positive) is
      Saved : constant PhysAddress := x86.getCR3;
      Started : constant Unsigned_64 := x86.rdtsc;
   begin
      setActiveP4 (Root_Address);
      for I in 0 .. 511 loop
         declare
            Value : Unsigned_64 with Import, Volatile,
              Address => To_Address (VA + Integer_Address (I) * 4096);
         begin
            Check (Value = 16#1000# + Unsigned_64 (I));
         end;
      end loop;
      Armed (CPU) := True;
      while not Replaced loop
         -- AP startup still masks IRQs. Exercise the production cooperative
         -- service/ACK path (also used by lock waiters), not IPI dispatch.
         TLB_Shootdown.Service (CPU);
         Pause (Started);
      end loop;
      for I in 0 .. 511 loop
         declare
            Value : Unsigned_64 with Import, Volatile,
              Address => To_Address (VA + Integer_Address (I) * 4096);
         begin
            Check (Value = 16#2000# + Unsigned_64 (I));
         end;
      end loop;
      setActiveP4 (Saved);
      Done (CPU) := True;
   end Secondary;
   procedure Finish (Count : Natural) is
      Root : P4 with Import, Address => To_Address (P2V (Root_Address));
      Started : constant Unsigned_64 := x86.rdtsc;
      OK : Boolean;
   begin
      Check (Count > 0 and Count <= TLB_Reclamation.CPU_Index'Last);
      for CPU in 1 .. Count loop
         while not Armed (CPU) loop Pause (Started); end loop;
      end loop;
      unmapPage (VA, Root, OK); Check (OK);
      Map_Big (V2P (New_Block), VA, Flags, Root, OK); Check (OK);
      TLB_Shootdown.Invalidate_All;
      Replaced := True;
      for CPU in 1 .. Count loop
         while not Done (CPU) loop Pause (Started); end loop;
      end loop;
      -- Retain test backing; no production driver allocation is changed.
      TextIO.println ("BIG-PAGE SMP PASS: remote replacement after production ACK");
   end Finish;
end Big_Page_SMP_Check;
