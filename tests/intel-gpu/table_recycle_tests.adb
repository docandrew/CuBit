with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.Retirement;
procedure Table_Recycle_Tests is
   package P renames Intel_GPU_Table_Provenance;
   Current_Ticket, Confirmed_Ticket : Unsigned_64 := 0;
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
      CPU, DMA : out Unsigned_64; OK : out Boolean) is
   begin
      CPU := 16#10000000# + Offset; DMA := 16#200000# + Offset;
      OK := Session = 42 and Ticket = Current_Ticket;
   end Resolve;
   function Released (Session : Unsigned_64) return Boolean is (Session = 42);
   function May_Free (Session, Ticket : Unsigned_64) return Boolean is
     (Session = 42 and Ticket = Current_Ticket);
   function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
     (Session = 42 and Ticket = Confirmed_Ticket);
   package A is new P.Authority (Resolve);
   package R is new P.Retirement (Released, May_Free, Confirmed);
   use type P.Retirement_Phase;
   Object : P.Ledger;
   Metadata : array (1 .. 4096) of Unsigned_8 := [others => 0] with Alignment => 4096;
   OK : Boolean;
   Original_Capacity : Positive;
begin
   P.Extend (Object, Unsigned_64 (To_Integer (Metadata'Address)), 4096, OK);
   pragma Assert (OK);
   Original_Capacity := P.Capacity (Object);
   for Cycle in Unsigned_64 range 1 .. 256 loop
      Current_Ticket := Cycle * 65536 + 17;
      for I in 1 .. 64 loop
         A.Install (Object, 42, Cycle, I, Current_Ticket, Unsigned_64 (I - 1) * 4096, OK);
         pragma Assert (OK);
      end loop;
      R.Recycle_Confirmed (Object, 42, Cycle, Current_Ticket, OK);
      pragma Assert (not OK and R.Phase (Object) = P.Open and P.Count (Object) = 64);
      Confirmed_Ticket := Current_Ticket;
      R.Recycle_Confirmed (Object, 42, Cycle - 1, Current_Ticket, OK);
      pragma Assert (not OK);
      R.Recycle_Confirmed (Object, 43, Cycle, Current_Ticket, OK); pragma Assert (not OK);
      R.Recycle_Confirmed (Object, 42, Cycle, Current_Ticket + 1, OK); pragma Assert (not OK);
      R.Recycle_Confirmed (Object, 42, Cycle, Current_Ticket, OK); pragma Assert (OK);
      pragma Assert (P.Count (Object) = 0 and P.Generation (Object) = Cycle + 1
                     and P.Capacity (Object) = Original_Capacity);
      pragma Assert (A.Lookup (Object, 42, Cycle, 1).Ticket = 0);
      R.Recycle_Confirmed (Object, 42, Cycle, Current_Ticket, OK); pragma Assert (not OK);
   end loop;
   -- Larger or mixed-allocation ledgers cannot use the single-receipt adapter.
   for I in 1 .. 65 loop
      A.Install (Object, 42, 257, I, Current_Ticket, Unsigned_64 (I - 1) * 4096, OK);
      pragma Assert (OK);
   end loop;
   R.Recycle_Confirmed (Object, 42, 257, Current_Ticket, OK);
   pragma Assert (not OK and P.Count (Object) = 65 and R.Phase (Object) = P.Open);
   declare
      Mixed : P.Ledger;
   begin
      Current_Ticket := 17;
      A.Install (Mixed, 42, 1, 1, 17, 0, OK); pragma Assert (OK);
      Current_Ticket := 18;
      A.Install (Mixed, 42, 1, 2, 18, 4096, OK); pragma Assert (OK);
      Confirmed_Ticket := 18;
      R.Recycle_Confirmed (Mixed, 42, 1, 18, OK);
      pragma Assert (not OK and P.Count (Mixed) = 2 and R.Phase (Mixed) = P.Open);
   end;
   Ada.Text_IO.Put_Line ("Table recycle PASS:256 exact-ack generations, fixed metadata capacity, stale/unconfirmed/oversized rejection");
end Table_Recycle_Tests;
