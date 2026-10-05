with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Record_Growth;
with Intel_GPU_Table_Provenance;
procedure Table_Growth_Tests is
   package P renames Intel_GPU_Table_Provenance;
   type Bytes is array (1 .. 131072) of Unsigned_8;
   Metadata : aliased Bytes := [others => 0] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
   procedure Run (Fault : Natural) is
      Object : P.Ledger;
      Live : Boolean := True;
      Reservations, Commits : Natural := 0;
      Published : Unsigned_64 := 0;
      function Reserve (Size : Unsigned_64) return Unsigned_64 is
      begin
         pragma Assert (Size = Metadata'Length);
         Reservations := Reservations + 1;
         return (if Fault = 1 then 0 else Base);
      end Reserve;
      function Commit (Address, Offset, Size : Unsigned_64) return Boolean is
      begin
         pragma Assert (Address = Base and Size <= 65536 and Offset + Size <= Metadata'Length);
         Commits := Commits + 1;
         return Fault /= 2;
      end Commit;
      function Clear (Address, Size : Unsigned_64) return Boolean is
      begin
         pragma Assert (Address >= Base and Address + Size <= Base + Metadata'Length);
         if Fault = 3 then return False; end if;
         for I in Address - Base + 1 .. Address - Base + Size loop
            Metadata (Positive (I)) := 0;
         end loop;
         return True;
      end Clear;
      package Storage is new Intel_GPU_Metadata_Arena (Reserve, Commit, Clear);
      function Capacity return Positive is (P.Capacity (Object));
      procedure Publish (Address, Size : Unsigned_64; OK : out Boolean) is
      begin
         OK := False;
         if not Live then return; end if;
         P.Extend (Object, Address, Size, OK);
         if OK then Published := Size; end if;
      end Publish;
      package Growth is new Intel_GPU_Record_Growth (Storage, Capacity, Publish);
      use type Growth.Phase;
      Controller : Growth.Controller;
      procedure Resolve (Session, Ticket, Offset : Unsigned_64;
         CPU, DMA : out Unsigned_64; OK : out Boolean) is
      begin
         OK := Live and Session = 42 and Ticket = 17 and Offset < 4096 * 5000;
         CPU := 16#10000000# + Offset; DMA := 16#200000# + Offset;
      end Resolve;
      package Authority is new P.Authority (Resolve);
      use type P.Mapping;
      Saved : P.Mapping;
      OK : Boolean;
      procedure Finish is
         Before : Natural;
      begin
         for Tick in 1 .. 20 loop
            exit when Growth.Snapshot (Controller).State in Growth.Idle | Growth.Failed;
            Before := Commits;
            if Fault = 4 and Growth.Snapshot (Controller).State = Growth.Publishing then
               Live := False;
            end if;
            Growth.Step (Controller);
            pragma Assert (Commits <= Before + 1);
         end loop;
         pragma Assert (Growth.Snapshot (Controller).State in Growth.Idle | Growth.Failed);
      end Finish;
   begin
      for I in 1 .. 16 loop
         Authority.Install (Object, 42, 1, I, 17, Unsigned_64 (I - 1) * 4096, OK);
         pragma Assert (OK);
      end loop;
      Saved := Authority.Lookup (Object, 42, 1, 1);
      Growth.Configure (Controller, 131072, 4096, OK); pragma Assert (OK);
      Growth.Request (Controller, 64, OK); pragma Assert (OK);
      Finish;
      if Fault /= 0 then
         pragma Assert (Growth.Snapshot (Controller).State = Growth.Failed);
         pragma Assert (P.Capacity (Object) = 16 and P.Count (Object) = 16 and Published = 0);
         Growth.Step (Controller);
         pragma Assert (Reservations = 1 and Commits <= 1);
         return;
      end if;
      pragma Assert (Growth.Snapshot (Controller).State = Growth.Idle and Capacity >= 64);
      for I in 17 .. 64 loop
         Authority.Install (Object, 42, 1, I, 17, Unsigned_64 (I - 1) * 4096, OK);
         pragma Assert (OK);
      end loop;
      Growth.Request (Controller, 3000, OK); pragma Assert (OK);
      Finish;
      pragma Assert (Capacity >= 3000 and Published = 131072 and Reservations = 1 and Commits = 2);
      pragma Assert (Authority.Lookup (Object, 42, 1, 1) = Saved and P.Count (Object) = 64);
      Growth.Request (Controller, 4097, OK); pragma Assert (not OK);
      pragma Assert (Growth.Snapshot (Controller).State = Growth.Idle);
   end Run;
begin
   for Fault in 0 .. 4 loop Run (Fault); end loop;
   Ada.Text_IO.Put_Line ("table growth: PASS stable records, two commits, quota and failure retention");
end Table_Growth_Tests;
