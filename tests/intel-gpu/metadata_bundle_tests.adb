with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Bundle;
procedure Metadata_Bundle_Tests is
   type Table is (Tickets, Handles, Backing, Replacements, Retirement, Updates);
   type Counts is array (Table) of Natural;
   type Bytes_Array is array (Table) of Unsigned_64;
begin
   for Fault in 0 .. 8 loop
      declare
         Caps : Counts := [others => 16];
         Committed, Initialized : Bytes_Array := [others => 0];
         Reservations, Commits, Publications, Admissions : Natural := 0;
         Admitted : Positive := 16;
         Owner : Boolean := True;
         function Ready return Boolean is (Owner);
         function Base (T : Table) return Unsigned_64 is
           (16#1000_0000# + Unsigned_64 (Table'Pos (T)) * 16#1000_0000#);
         function Find (Address : Unsigned_64) return Table is
           (Table'Val ((Address - 16#1000_0000#) / 16#1000_0000#));
         function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
            T : constant Table := Table'Val (Reservations);
         begin
            pragma Assert (Bytes = 131072);
            Reservations := Reservations + 1;
            return Base (T);
         end Reserve;
         function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
            T : constant Table := Find (Address);
         begin
            Commits := Commits + 1;
            pragma Assert (Address = Base (T) and Offset = Committed (T) and Bytes <= 65536);
            pragma Assert (Bytes = Unsigned_64'Min
              (Unsigned_64'Min (65536, Unsigned_64'Max (4096, Offset)),
               131072 - Offset));
            if Fault = 7 and T = Backing then return False; end if;
            Committed (T) := Offset + Bytes;
            return True;
         end Commit;
         function Clear (Address, Bytes : Unsigned_64) return Boolean is
            T : constant Table := Find (Address);
         begin
            pragma Assert (Address = Base (T) + Initialized (T));
            Initialized (T) := Initialized (T) + Bytes;
            return True;
         end Clear;
         package M is new Intel_GPU_Metadata_Arena (Reserve, Commit, Clear);
         function Capacity (T : Table) return Natural is (Caps (T));
         procedure Extend (T : Table; Address, Bytes : Unsigned_64; OK : out Boolean) is
         begin
            Publications := Publications + 1;
            pragma Assert (Address = Base (T) and Bytes <= Initialized (T));
            OK := Fault /= Table'Pos (T) + 1;
            if OK then Caps (T) := 16 + Natural (Bytes / (16 + Unsigned_64 (Table'Pos (T)) * 8)); end if;
         end Extend;
         procedure Admit (Count : Positive; OK : out Boolean) is
         begin
            Admissions := Admissions + 1;
            for T in Table loop pragma Assert (Caps (T) >= Count); end loop;
            Admitted := Count; OK := True;
         end Admit;
         package G is new Intel_GPU_Metadata_Bundle (Table, M, Ready, Capacity, Extend, Admit);
         use type G.Phase;
         Object : G.Bundle;
         OK : Boolean;
         Before : Natural;
      begin
         G.Request (Object, 1000, 999, 131072, OK); pragma Assert (not OK);
         G.Request (Object, 1000, 2000, 131072, OK); pragma Assert (OK);
         for Turn in 1 .. 500 loop
            Before := Reservations + Commits + Publications + Admissions;
            if Fault = 8 and Publications = 2 then Owner := False; end if;
            G.Step (Object);
            pragma Assert (Reservations + Commits + Publications + Admissions <= Before + 1);
            exit when G.State (Object) in G.Idle | G.Failed;
            pragma Assert (Admitted = 16);
         end loop;
         if Fault = 0 then
            pragma Assert (G.State (Object) = G.Idle and Admitted = 1000 and Reservations = 6);
            G.Request (Object, 1500, 2000, 131072, OK); pragma Assert (OK);
            for Turn in 1 .. 500 loop
               G.Step (Object);
               exit when G.State (Object) in G.Idle | G.Failed;
            end loop;
            pragma Assert (G.State (Object) = G.Idle and Admitted = 1500 and Reservations = 6);
         else
            pragma Assert (G.State (Object) = G.Failed and Admitted = 16 and Admissions = 0);
            Before := Reservations + Commits + Publications;
            for Turn in 1 .. 10 loop G.Step (Object); end loop;
            pragma Assert (Reservations + Commits + Publications = Before);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Metadata bundle PASS: heterogeneous capacities, two growth rounds, six publication faults, commit/owner failure, atomic admission, no replay");
end Metadata_Bundle_Tests;
