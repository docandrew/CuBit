with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.Retirement;
with Intel_GPU_Table_Provenance.IO;
procedure Table_Generation_IO_Tests is
   package P renames Intel_GPU_Table_Provenance;
   Page : array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64 := [others => 77]
     with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Page'Address));
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                      CPU, DMA : out Unsigned_64; OK : out Boolean) is
   begin
      CPU := Base; DMA := 16#200000#;
      OK := Session = 42 and Ticket = 17 and Offset = 0;
   end Resolve;
   function Released (Session : Unsigned_64) return Boolean is (Session = 42);
   function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
     (Session = 42 and Ticket = 17);
   function Exclusive return Boolean is (True);
   Flushes : Natural := 0;
   function Flush (CPU : Unsigned_64) return Boolean is
   begin
      pragma Assert (CPU = Base);
      Flushes := Flushes + 1; return True;
   end Flush;
   package A is new P.Authority (Resolve);
   package R is new P.Retirement (Released, Confirmed, Confirmed);
   package IO is new P.IO (A, Exclusive, Flush);
   use type P.Retirement_Phase;
   Object : P.Ledger;
   OK : Boolean;
   Ticket, Value : Unsigned_64;
begin
   A.Install (Object, 42, 1, 1, 17, 0, OK); pragma Assert (OK);
   IO.Write_Word (Object, 42, 1, 1, 16#200000#, 0, 88, OK);
   pragma Assert (OK and Page (0) = 88);
   R.Start (Object, 42, 1, OK); pragma Assert (OK);
   R.Step (Object);
   R.Take_Request (Object, 42, Ticket, OK); pragma Assert (OK and Ticket = 17);
   R.Acknowledge (Object, 42, Ticket, OK); pragma Assert (OK);
   R.Step (Object); R.Step (Object);
   pragma Assert (R.Phase (Object) = P.Complete);
   R.Reopen (Object, 42, 1, OK); pragma Assert (OK);
   A.Install (Object, 42, 2, 1, 17, 0, OK); pragma Assert (OK);
   IO.Write_Word (Object, 42, 1, 1, 16#200000#, 0, 99, OK);
   pragma Assert (not OK and Page (0) = 88);
   IO.Read_Word (Object, 42, 1, 1, 16#200000#, 0, Value, OK);
   pragma Assert (not OK and Value = 0);
   pragma Assert (not IO.Flush (Object, 42, 1, 1, 16#200000#) and Flushes = 0);
   IO.Write_Word (Object, 42, 2, 1, 16#200000#, 0, 99, OK);
   pragma Assert (OK and Page (0) = 99);
   pragma Assert (IO.Flush (Object, 42, 2, 1, 16#200000#) and Flushes = 1);
   Ada.Text_IO.Put_Line ("Table generation IO PASS: stale reads/writes/flush rejected after acknowledged metadata reuse");
end Table_Generation_IO_Tests;
