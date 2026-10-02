with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Virtmem; use Virtmem;
with BuddyAllocator;
with TextIO;
with System;
with x86;
procedure Native_Walk_Check is
   function Allocate_Root return PhysAddress is
      Address : PhysAddress;
   begin
      BuddyAllocator.allocFrame (Address);
      if Address = 0 then raise Program_Error; end if;
      return Address;
   end Allocate_Root;
   Root_Address : constant PhysAddress := Allocate_Root;
   Root : P4 with Import, Address => To_Address (P2V (Root_Address));
   procedure Clear is new zeroize (P4);
   Calls : Natural := 0;
   OK : Boolean;
   procedure Allocate (Address : out PhysAddress) is
   begin
      Calls := Calls + 1;
      BuddyAllocator.allocFrame (Address);
   end Allocate;
   procedure Map_Big is new mapBigPage (Allocate);
   procedure Map_Small is new mapPage (Allocate);
   function Next_3 is new getNextTable (P4);
   function Next_2 is new getNextTable (P3);
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "big-page native check failed"; end if;
   end Check;
begin
   Clear (Root);
   -- Offline page tables in native CuBit: never install Root into CR3 or
   -- access its synthetic data addresses. Only table frames are allocated.
   Map_Big (16#200001#, 16#400000#, PG_USERDATA, Root, OK);
   Check (not OK and Calls = 0);
   Map_Big (16#200000#, 16#400001#, PG_USERDATA, Root, OK);
   Check (not OK and Calls = 0);
   Map_Big (16#200000#, 16#400000#, PG_USERDATA or PG_PAT, Root, OK);
   Check (OK);
   Check (tableWalk (16#400000#, Root) = 0);
   for I in 0 .. 511 loop
      -- Grant resolution uses the default walker, including interior pages.
      Check (tableWalk (16#400000# + Integer_Address (I) * 4096, Root) = 0);
      Check (tableWalk (16#400000# + Integer_Address (I) * 4096 + 123,
                       Root, Allow_Big => True) =
             16#200000# + Integer_Address (I) * 4096);
   end loop;
   Map_Big (16#600000#, 16#400000#, PG_USERDATA, Root, OK);
   Check (not OK and tableWalk (16#400000#, Root, True) = 16#200000#);
   declare
      Table_3 : P3 with Import, Address => To_Address (P2V (Next_3 (Root, getP4Index (16#400000#))));
      Table_2 : P2 with Import, Address => To_Address (P2V (Next_2 (Table_3, getP3Index (16#400000#))));
   begin
      Table_2 (2).pgNum := Table_2 (2).pgNum or 2;
      Check (tableWalk (16#400000#, Root, True) = 0);
      Table_2 (2).pgNum := Table_2 (2).pgNum and not PFN'(2);
      Table_2 (2).present := False;
      Check (tableWalk (16#400000#, Root, True) = 0);
      Map_Big (16#600000#, 16#400000#, PG_USERDATA, Root, OK);
      Check (not OK); -- retired leaf remains unavailable
   end;
   Map_Small (16#800000#, 16#A00000#, PG_USERDATA, Root, OK);
   Check (OK and tableWalk (16#A0007B#, Root) = 16#800000#);
   Map_Big (16#C00000#, 16#A00000#, PG_USERDATA, Root, OK);
   Check (not OK and tableWalk (16#A00000#, Root) = 16#800000#);
   declare
      P3_Address : constant PhysAddress := Next_3 (Root, 0);
      Table_3 : P3 with Import, Address => To_Address (P2V (P3_Address));
      P2_Address : constant PhysAddress := Next_2 (Table_3, 0);
      Table_2 : P2 with Import, Address => To_Address (P2V (P2_Address));
      function Next_1 is new getNextTable (P2);
      P1_Address : constant PhysAddress := Next_1 (Table_2, 5);
      Released : Natural := 0;
      procedure Record_Free (Address : PhysAddress) is
      begin
         -- Only page tables may be released, never the synthetic leaf backing.
         case Released is
            when 0 => Check (Address = P1_Address);
            when 1 => Check (Address = P2_Address);
            when 2 => Check (Address = P3_Address);
            when others => Check (False);
         end case;
         Released := Released + 1;
      end Record_Free;
      procedure Delete_Root is new deleteP4 (Record_Free);
   begin
      -- Cover both present and retired large leaves during table teardown.
      Map_Big (16#E00000#, 16#C00000#, PG_USERDATA, Root, OK);
      Check (OK);
      Delete_Root (Root);
      Check (Released = 3);
      TextIO.println ("BIG-PAGE TEARDOWN PASS: table frames only; default walker rejects512 subpages");
   end;
   TextIO.println ("BIG-PAGE WALK PASS:512 offsets PAT reserved nonpresent occupied alignment");
   declare
      use type System.Address;
      First, Second : System.Address;
      Saved_CR3 : constant PhysAddress := x86.getCR3;
      Saved_Root : P4 with Import, Address => To_Address (P2V (Saved_CR3));
      Test_CR3 : constant PhysAddress := Allocate_Root;
      Live : P4 with Import, Address => To_Address (P2V (Test_CR3));
      VA : constant VirtAddress := 16#0000_6000_0000_0000#;
      -- Nonglobal kernel-only mapping: local CR3 reload must invalidate it.
      Flags : constant Unsigned_64 := PG_PRESENT or PG_WRITABLE or PG_NXE;
   begin
      for Index in PageTableIndex loop Live (Index) := Saved_Root (Index); end loop;
      BuddyAllocator.alloc (9, First);
      BuddyAllocator.alloc (9, Second);
      Check (First /= System.Null_Address and Second /= System.Null_Address);
      Check (tableWalk (VA, Live, True) = 0);
      Map_Big (V2P (First), VA, Flags, Live, OK); Check (OK);
      setActiveP4 (Test_CR3);
      for I in 0 .. 511 loop
         declare
            Offset : constant Integer_Address := Integer_Address (I) * 4096;
            CPU : Unsigned_64 with Import, Volatile, Address => To_Address (VA + Offset);
            Physical : Unsigned_64 with Import, Volatile,
              Address => To_Address (To_Integer (First) + Offset);
            Replacement : Unsigned_64 with Import, Volatile,
              Address => To_Address (To_Integer (Second) + Offset);
         begin
            CPU := Unsigned_64 (I) + 16#1000#;
            Check (Physical = Unsigned_64 (I) + 16#1000#);
            Replacement := Unsigned_64 (I) + 16#2000#;
         end;
      end loop;
      unmapPage (VA, Live, OK); Check (OK);
      flushTLB;
      Map_Big (V2P (Second), VA, Flags, Live, OK); Check (OK);
      flushTLB;
      for I in 0 .. 511 loop
         declare
            CPU : Unsigned_64 with Import, Volatile,
              Address => To_Address (VA + Integer_Address (I) * 4096);
         begin
            Check (CPU = Unsigned_64 (I) + 16#2000#);
         end;
      end loop;
      unmapPage (VA, Live, OK); Check (OK);
      flushTLB;
      -- Test-only retained frames; never free while alias lifecycle is unproved.
      setActiveP4 (Saved_CR3);
      TextIO.println ("BIG-PAGE CPU PASS:512 writes remap local TLB invalidation");
   end;
end Native_Walk_Check;
