with Virtmem; use Virtmem;
with Virtmem.Regions; use Virtmem.Regions;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO;
procedure Adapter_Tests is
   Root : P4;
   L3 : P3;
   L2 : P2;
   L1 : P1;
   VA : constant VirtAddress := 16#1234_5678_9000#;
   I4 : constant PageTableIndex := getP4Index (VA);
   I3 : constant PageTableIndex := getP3Index (VA);
   I2 : constant PageTableIndex := getP2Index (VA);
   I1 : constant PageTableIndex := getP1Index (VA);
   OK : Boolean;
   Original : PageTableEntry;
   procedure Reject (Address : VirtAddress; Frame : PFN; Mode : Access_Mode) is
   begin
      Original := L1 (I1);
      Set_Access (Root, Address, Frame, Mode, OK);
      pragma Assert (not OK and L1 (I1) = Original);
   end Reject;
begin
   Root (I4) := (present | writable | user => True,
      pgNum => Frame_Bits (To_Integer (L3'Address) / 4096), others => <>);
   L3 (I3) := (present | writable | user => True,
      pgNum => Frame_Bits (To_Integer (L2'Address) / 4096), others => <>);
   L2 (I2) := (present | writable | user => True,
      pgNum => Frame_Bits (To_Integer (L1'Address) / 4096), others => <>);
   L1 (I1) := (present | writable | user | NX => True, pgNum => 123, others => <>);
   Reject (VA + 1, 123, Inaccessible);
   Reject (VA, 124, Inaccessible);
   Reject (VA, 123, Read_Execute);
   L3 (I3).size := True;
   Reject (VA, 123, Inaccessible);
   L3 (I3).size := False;
   L2 (I2).size := True;
   Reject (VA, 123, Inaccessible);
   L2 (I2).size := False;
   Root (I4).NX := True;
   Reject (VA, 123, Inaccessible);
   Root (I4).NX := False;
   Set_Access (Root, VA, 123, Inaccessible, OK);
   pragma Assert (OK and not L1 (I1).present and not L1 (I1).writable);
   pragma Assert (Matches (Root, VA, 123));
   pragma Assert (not Matches (Root, VA, 124));
   pragma Assert (not L1 (I1).present);
   Set_Access (Root, VA, 123, Read_Only, OK);
   pragma Assert (OK and L1 (I1).present and L1 (I1).NX and not L1 (I1).writable);
   pragma Assert (Matches (Root, VA, 123));
   Reject (VA, 123, Read_Write);
   Set_Access (Root, VA, 123, Inaccessible, OK);
   pragma Assert (OK);
   Set_Access (Root, VA, 123, Read_Execute, OK);
   pragma Assert (OK and L1 (I1).present and not L1 (I1).NX and not L1 (I1).writable);
   Reject (VA, 123, Read_Write);
   Set_Access (Root, VA, 123, Inaccessible, OK);
   pragma Assert (OK);
   Set_Access (Root, VA, 123, Read_Write, OK);
   pragma Assert (OK and L1 (I1).present and L1 (I1).NX and L1 (I1).writable);
   pragma Assert (L1 ((I1 + 1) mod 512).pgNum = 0);
   Ada.Text_IO.Put_Line ("PASS: actual region adapter on hosted synthetic tables (no real TLB test)");
end Adapter_Tests;
