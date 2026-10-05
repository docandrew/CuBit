with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Table_Provenance.Backing;
procedure Table_Backing_Tests is
   package B renames Intel_GPU_Buffer_Reply;
   Live, Found, Revoke, Stale : Boolean := True;
   Owner : Unsigned_64 := 42;
   First : Unsigned_64 := 8192;
   Selected : B.Backing;
   Calls : Natural := 0;
   function Ready return Boolean is (Live);
   function Session_Of (Ticket : Unsigned_64) return Unsigned_64 is
     (if Ticket = 17 then Owner else 0);
   procedure Choose
     (Session, Ticket : Unsigned_64; Result : out B.Backing;
      Allocation_Offset : out Unsigned_64; Accepted : out Boolean)
   is
   begin
      pragma Assert (Session = 42 and Ticket = 17);
      Calls := Calls + 1;
      Result := Selected;
      Allocation_Offset := First;
      Accepted := Found;
      if Revoke then Live := False; end if;
      if Stale then Owner := 0; end if;
   end Choose;
   package Resolver is new Intel_GPU_Table_Provenance.Backing
     (Ready, Session_Of, Choose);
   procedure Check (Offset : Unsigned_64; Expected : Boolean;
                    Session : Unsigned_64 := 42; Ticket : Unsigned_64 := 17)
   is
      CPU, DMA : Unsigned_64 := Unsigned_64'Last;
      OK : Boolean := True;
   begin
      Resolver.Resolve_Owned_Page (Session, Ticket, Offset, CPU, DMA, OK);
      pragma Assert (OK = Expected);
      if Expected then
         pragma Assert (CPU = B.Layout.CPU_Base + Offset - First);
         pragma Assert (DMA = 16#200000# + Offset - First);
      else
         pragma Assert (CPU = 0 and DMA = 0);
      end if;
   end Check;
begin
   Revoke := False; Stale := False;
   Selected := B.From_Linear (16#200000#, B.Layout.CPU_Base, 8192, 16#200000#);
   pragma Assert (B.Valid (Selected));
   Check (8192, True); Check (12288, True);
   Check (4096, False); Check (16384, False);
   Check (8193, False); Check (Unsigned_64'Last - 4095, False);
   Check (8192, False, 0); Check (8192, False, 43);
   Check (8192, False, Ticket => 0); Check (8192, False, Ticket => 18);
   pragma Assert (Calls = 5);
   Found := False; Check (8192, False); Found := True;
   First := 1; Check (8192, False); First := 8192;
   Revoke := True; Check (8192, False); Revoke := False;
   Check (8192, False); Live := True;
   Stale := True; Check (8192, False); Stale := False; Owner := 42;
   Selected.Bytes := 4096; Check (8192, False);
   Selected := B.From_Linear (16#200000#, B.Layout.CPU_Base, 8192, 16#200000#);
   First := 0;
   Check (0, True); Check (4096, True); Check (8192, False);
   Selected := (Ready => False); Check (8192, False);
   Ada.Text_IO.Put_Line ("table backing resolver: PASS");
end Table_Backing_Tests;
