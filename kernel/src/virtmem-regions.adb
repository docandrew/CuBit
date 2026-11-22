with Region_PTE;
package body Virtmem.Regions with SPARK_Mode => Off is
   procedure Visit (Root : P4; Address : VirtAddress;
                         Expected_Frame : PFN; Mode : Access_Mode;
                         Write_Enabled : Boolean;
                         Success : out Boolean) is
      function Get_P3 is new getNextTable (P4);
      function Get_P2 is new getNextTable (P3);
      function Get_P1 is new getNextTable (P2);
      P3_Address, P2_Address, P1_Address : PhysAddress;
      function Traversable (Entry_Value : PageTableEntry) return Boolean is
        (Entry_Value.present and Entry_Value.user and Entry_Value.writable and
         not Entry_Value.size and not Entry_Value.NX);
   begin
      Success := False;
      if Address = 0 or Address >= 2 ** 47 or Address mod PAGE_SIZE /= 0 or
        Expected_Frame = 0 then return; end if;
      if not Traversable (Root (getP4Index (Address))) then return; end if;
      P3_Address := Get_P3 (Root, getP4Index (Address));
      if P3_Address = 0 then return; end if;
      declare
         Table_3 : P3 with Import, Address => To_Address (P2V (P3_Address));
      begin
         if not Traversable (Table_3 (getP3Index (Address))) then return; end if;
         P2_Address := Get_P2 (Table_3, getP3Index (Address));
      end;
      if P2_Address = 0 then return; end if;
      declare
         Table_2 : P2 with Import, Address => To_Address (P2V (P2_Address));
      begin
         if not Traversable (Table_2 (getP2Index (Address))) then return; end if;
         P1_Address := Get_P1 (Table_2, getP2Index (Address));
      end;
      if P1_Address = 0 then return; end if;
      declare
         -- A single aligned full-word store, not multiple bitfield writes.
         Word : Unsigned_64 with Import, Volatile_Full_Access,
           Address => To_Address (P2V (P1_Address) +
             Integer_Address (getP1Index (Address)) * 8);
         Plan : constant Region_PTE.Decision := Region_PTE.Plan
           (Word, Unsigned_64 (Expected_Frame) * PAGE_SIZE,
            (case Mode is
               when Inaccessible => Region_PTE.Inaccessible,
               when Read_Only => Region_PTE.Read_Only,
               when Read_Write => Region_PTE.Read_Write,
               when Read_Execute => Region_PTE.Read_Execute));
      begin
         if not Plan.Allowed then return; end if;
         if Write_Enabled then Word := Plan.Value; end if;
         Success := True;
      end;
   end Visit;

   procedure Set_Access (Root : P4; Address : VirtAddress;
                         Expected_Frame : PFN; Mode : Access_Mode;
                         Success : out Boolean) is
   begin
      Visit (Root, Address, Expected_Frame, Mode, True, Success);
   end Set_Access;

   function Matches (Root : P4; Address : VirtAddress;
                     Expected_Frame : PFN) return Boolean is
      OK : Boolean;
   begin
      Visit (Root, Address, Expected_Frame, Inaccessible, False, OK);
      return OK;
   end Matches;
end Virtmem.Regions;
