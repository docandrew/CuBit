with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Bundle;
procedure Metadata_Demand_Tests is
   type Table is (Mirrors, Descriptors, Provenance);
   type Counts is array (Table) of Natural;
   type Sizes is array (Table) of Unsigned_64;
   Caps : Counts := [others => 4];
   Targets : Counts := [6, 12, 24];
   Limits : constant Sizes := [8192, 4096, 2 ** 40];
   Widths : constant Sizes := [4096, 16, 64];
   Committed, Initialized : Sizes := [others => 0];
   Reserves, Commits, Publications, Admissions : Natural := 0;
   function Ready return Boolean is (True);
   function Base (T : Table) return Unsigned_64 is
     (16#100000# + Unsigned_64 (Table'Pos (T)) * 16#100000#);
   function Find (Address : Unsigned_64) return Table is
     (Table'Val ((Address - 16#100000#) / 16#100000#));
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
      T : constant Table := Table'Val (Reserves);
   begin
      pragma Assert (Bytes = Limits (T));
      Reserves := Reserves + 1;
      return Base (T);
   end Reserve;
   function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
      T : constant Table := Find (Address);
   begin
      pragma Assert (Offset = Committed (T) and Offset + Bytes <= Limits (T));
      Committed (T) := Offset + Bytes; Commits := Commits + 1;
      return True;
   end Commit;
   function Clear (Address, Bytes : Unsigned_64) return Boolean is
      T : constant Table := Find (Address);
   begin
      pragma Assert (Address = Base (T) + Initialized (T));
      Initialized (T) := Initialized (T) + Bytes;
      return True;
   end Clear;
   package Storage is new Intel_GPU_Metadata_Arena (Reserve, Commit, Clear);
   function Capacity (T : Table) return Natural is (Caps (T));
   procedure Extend (T : Table; Address, Bytes : Unsigned_64; OK : out Boolean) is
   begin
      pragma Assert (Address = Base (T) and Bytes <= Initialized (T));
      Caps (T) := 4 + Natural (Bytes / Widths (T));
      Publications := Publications + 1; OK := True;
   end Extend;
   procedure Admit (Count : Positive; OK : out Boolean) is
   begin
      pragma Assert (Count = 42);
      for T in Table loop pragma Assert (Caps (T) >= Targets (T)); end loop;
      Admissions := Admissions + 1; OK := True;
   end Admit;
   package G is new Intel_GPU_Metadata_Bundle (Table, Storage, Ready, Capacity, Extend, Admit);
   use type G.Phase;
   Object : G.Bundle;
   Demand : G.Requirements;
   OK : Boolean;
   Before : Natural;
   procedure Finish is
   begin
      for Turn in 1 .. 100 loop
         Before := Reserves + Commits + Publications + Admissions;
         G.Step (Object);
         pragma Assert (Reserves + Commits + Publications + Admissions <= Before + 1);
         exit when G.State (Object) in G.Idle | G.Failed;
      end loop;
      pragma Assert (G.State (Object) = G.Idle);
   end Finish;
begin
   for T in Table loop Demand (T) := (Targets (T), 1024, Limits (T)); end loop;
   -- Last-store invalidity cannot reserve or mutate a prefix of the bundle.
   Demand (Provenance).Record_Quota := 23;
   G.Request (Object, 42, Demand, OK);
   pragma Assert (not OK and G.State (Object) = G.Idle and Reserves = 0);
   Demand (Provenance).Record_Quota := 1024;
   G.Request (Object, 42, Demand, OK); pragma Assert (OK);
   G.Request (Object, 99, Demand, OK); pragma Assert (not OK);
   Finish;
   pragma Assert (Admissions = 1 and Reserves = 3 and Caps (Mirrors) = 6);
   -- A terabyte VA quota does not eagerly commit even a64KiB chunk when one
   -- metadata page already satisfies demand. Mirrors need exactly two pages.
   pragma Assert (Committed = Sizes'[8192, 4096, 4096]);
   pragma Assert (Initialized = Committed and Commits = 4);
   -- A larger admission token never forces every store to reach that count.
   -- Existing committed prefixes and reservations are reused unchanged.
   G.Request (Object, 42, Demand, OK); pragma Assert (OK); Finish;
   pragma Assert (Admissions = 2 and Reserves = 3 and Publications = 4);
   Demand (Descriptors).Byte_Quota := 8192;
   G.Request (Object, 42, Demand, OK);
   pragma Assert (not OK and G.State (Object) = G.Idle and Reserves = 3);
   Ada.Text_IO.Put_Line ("Metadata demand PASS: distinct targets/quotas, atomic admission, bounded steps, stable reservations, invalid/busy rejection");
end Metadata_Demand_Tests;
