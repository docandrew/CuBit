with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Record_Growth;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
procedure Table_Metadata_Growth_Tests is
   package P renames Intel_GPU_Table_Provenance;
   use type P.Mapping;
   type RAM is array (Natural range 0 .. 131071) of Unsigned_64;
   Memory : RAM := [others => 16#CAFE#] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory'Address));
   Ledger : P.Ledger;
   Reserves, Commits, Clears, Publications, Resolves : Natural := 0;
   Committed, Initialized : Unsigned_64 := 0;
   function Capacity return Positive is (P.Capacity (Ledger));
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
   begin
      Reserves := Reserves + 1;
      pragma Assert (Bytes = 1024 * 1024 and Reserves = 1);
      return Base;
   end Reserve;
   function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
   begin
      pragma Assert (Address = Base and Offset = Committed and Bytes = 65536);
      Commits := Commits + 1;
      Committed := Offset + Bytes;
      return True;
   end Commit;
   function Clear (Address, Bytes : Unsigned_64) return Boolean is
   begin
      pragma Assert (Address = Base + Initialized and Initialized + Bytes <= Committed);
      Clears := Clears + 1;
      Initialized := Initialized + Bytes;
      return Intel_GPU_Metadata_Initialize.Clear (Address, Bytes);
   end Clear;
   procedure Publish (Address, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      pragma Assert (Address = Base and Bytes <= Initialized);
      Publications := Publications + 1;
      P.Extend (Ledger, Address, Bytes, Accepted);
   end Publish;
   package M is new Intel_GPU_Metadata_Arena (Reserve, Commit, Clear);
   package G is new Intel_GPU_Record_Growth (M, Capacity, Publish);
   use type G.Phase;
   Controller : G.Controller;
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                      CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
   begin
      Resolves := Resolves + 1;
      Accepted := Session = 7 and Ticket in 55 .. 57 and Offset mod 4096 = 0;
      CPU := (if Accepted then Ticket * 16#1000000# + Offset else 0);
      DMA := (if Accepted then CPU + 16#10000000# else 0);
   end Resolve;
   package A is new P.Authority (Resolve);
   use type A.Append_Phase;
   Append : A.Append_State;
   OK : Boolean;
   Initial_Capacity, Prior_IO, Prior_Resolve : Natural;
   Saved : P.Mapping;
   procedure Grow (Needed : Positive) is
      Before : Natural;
   begin
      G.Request (Controller, Needed, OK); pragma Assert (OK);
      for Turn in 1 .. 20 loop
         Before := Reserves + Commits + Publications;
         G.Step (Controller);
         pragma Assert (Reserves + Commits + Publications <= Before + 1);
         exit when G.Snapshot (Controller).State in G.Idle | G.Failed;
      end loop;
      pragma Assert (G.Snapshot (Controller).State = G.Idle and Capacity >= Needed);
   end Grow;
   procedure Add (Ticket : Unsigned_64; Pages : Positive) is
      First : constant Positive := P.Count (Ledger) + 1;
      Before : Natural;
   begin
      if A.Status (Append) = A.Appended then
         A.Rearm (Append, Ledger, OK); pragma Assert (OK);
      end if;
      A.Begin_Append (Append, Ledger, 7, 1, Ticket, 0, Pages, OK);
      pragma Assert (OK);
      for Page in 1 .. Pages loop
         Before := Resolves;
         A.Step (Append, Ledger);
         pragma Assert (A.Status (Append) /= A.Rejected);
         if Resolves /= Before + 1 then
            Ada.Text_IO.Put_Line ("append failed ticket=" & Unsigned_64'Image (Ticket) &
              " page=" & Positive'Image (Page) & " state=" & A.Append_Phase'Image (A.Status (Append)));
         end if;
         pragma Assert (Resolves = Before + 1);
      end loop;
      pragma Assert (A.Status (Append) = A.Appended and A.First_ID (Append) = First);
   end Add;
begin
   G.Configure (Controller, 1024 * 1024, 32768, OK); pragma Assert (OK);
   Grow (64);
   Initial_Capacity := Capacity;
   pragma Assert (Commits = 1 and Publications = 1 and Capacity >= 124);
   Add (55, 64);
   Saved := A.Lookup (Ledger, 7, 1, 64);
   pragma Assert (Saved.Ticket = 55 and Saved.Offset = 63 * 4096);
   Prior_IO := Reserves + Commits + Clears + Publications;
   Grow (124);
   Add (56, 60);
   pragma Assert (P.Count (Ledger) = 124 and P.Generation (Ledger) = 1);
   pragma Assert (Reserves + Commits + Clears + Publications = Prior_IO);
   pragma Assert (A.Lookup (Ledger, 7, 1, 64) = Saved);
   pragma Assert (A.Lookup (Ledger, 7, 1, 65).Ticket = 56);
   -- Cross the actual typed capacity, not a hard-coded record-size estimate.
   Grow (Initial_Capacity + 1);
   pragma Assert (Commits = 2 and Publications = 2 and Reserves = 1);
   Add (57, Initial_Capacity + 1 - 124);
   pragma Assert (P.Count (Ledger) = Initial_Capacity + 1);
   pragma Assert (A.Lookup (Ledger, 7, 1, 64) = Saved);
   pragma Assert (A.Lookup (Ledger, 7, 1, Initial_Capacity + 1).Ticket = 57);
   Prior_Resolve := Resolves;
   pragma Assert (A.Lookup (Ledger, 8, 1, 64) = (0, 0, 0, 0));
   pragma Assert (A.Lookup (Ledger, 7, 2, 64) = (0, 0, 0, 0));
   pragma Assert (Resolves = Prior_Resolve);
   for I in Natural (Committed / 8) .. Memory'Last loop
      pragma Assert (Memory (I) = 16#CAFE#);
   end loop;
   Ada.Text_IO.Put_Line ("Table metadata growth PASS: first capacity=" &
     Natural'Image (Initial_Capacity) & "; bootstrap64 + incremental60 without new commit; " &
     "capacity crossing preserves IDs, owner and generation");
end Table_Metadata_Growth_Tests;
