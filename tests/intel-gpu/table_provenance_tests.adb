with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
procedure Table_Provenance_Tests is
   Revoke, Move : Boolean := False;
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                      CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := not Revoke and Session = 42 and Ticket in 1 .. 80 and Offset = 0;
      CPU := 16#10000000# + Ticket * 4096;
      DMA := Ticket * 4096 + (if Move then 4096 else 0);
   end Resolve;
   package P renames Intel_GPU_Table_Provenance;
   package A is new P.Authority (Resolve);
   use type P.Mapping;
   Object : P.Ledger;
   type Bytes is array (1 .. 8192) of Unsigned_8;
   Metadata : aliased Bytes := [others => 0] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
   OK, Found : Boolean;
   Next : Natural;
   Saved : P.Mapping;
begin
   for I in 1 .. 16 loop
      A.Install (Object, 42, 1, I, Unsigned_64 (I), 0, OK); pragma Assert (OK);
   end loop;
   Saved := A.Lookup (Object, 42, 1, 1);
   A.Install (Object, 42, 1, 17, 17, 0, OK); pragma Assert (not OK);
   P.Extend (Object, Base, 4096, OK); pragma Assert (OK and P.Capacity (Object) >= 80);
   for I in 17 .. 80 loop
      A.Install (Object, 42, 1, I, Unsigned_64 (I), 0, OK); pragma Assert (OK);
   end loop;
   P.Extend (Object, Base, 8192, OK); pragma Assert (OK);
   pragma Assert (A.Lookup (Object, 42, 1, 1) = Saved and P.Count (Object) = 80);
   A.Install (Object, 42, 1, 1, 2, 0, OK); pragma Assert (not OK); -- immutable ID
   A.Install (Object, 43, 1, 81, 1, 0, OK); pragma Assert (not OK);
   pragma Assert (A.Lookup (Object, 43, 1, 1).Ticket = 0);
   Revoke := True; pragma Assert (A.Lookup (Object, 42, 1, 1).Ticket = 0);
   Revoke := False; Move := True; pragma Assert (A.Lookup (Object, 42, 1, 1).Ticket = 0);
   Move := False; pragma Assert (A.Lookup (Object, 42, 1, 1) = Saved);
   P.Scan_Ticket (Object, 42, 80, 1, Found, Next, OK);
   pragma Assert (OK and not Found and Next = 65);
   P.Scan_Ticket (Object, 42, 80, Next, Found, Next, OK);
   pragma Assert (OK and Found and Next = 0);
   -- Revocation must NOT make retained provenance vanish from retirement census.
   Revoke := True;
   P.Scan_Ticket (Object, 42, 1, 1, Found, Next, OK);
   pragma Assert (OK and Found and Next = 65);
   Ada.Text_IO.Put_Line ("Table provenance PASS: 80 stable IDs across metadata growth; ticket/address authentication; bounded retained-reference census");
end Table_Provenance_Tests;
