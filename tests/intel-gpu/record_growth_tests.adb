with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Record_Store;
with Intel_GPU_Record_Growth;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
procedure Record_Growth_Tests is
   type Entry_Record is record
      Identity : Unsigned_64 := 73;
      Retained : Boolean := True;
   end record;
   package R is new Intel_GPU_Record_Store (Entry_Record, (others => <>));
   type RAM is array (Natural range 0 .. 32767) of Unsigned_64;
begin
   -- 0 success; failures at reserve, commit, clear, typed publication, quota.
   for Fault in 0 .. 5 loop
      declare
         Memory : RAM := [others => 16#CAFE#] with Alignment => 4096;
         Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory'Address));
         Registry : R.Store;
         Reserves, Commits, Clears, Publications : Natural := 0;
         Committed, Initialized : Unsigned_64 := 0;
         function Capacity return Positive is (R.Capacity (Registry));
         function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
         begin
            Reserves := Reserves + 1;
            pragma Assert (Bytes = (if Fault = 5 then 65536 else 196608));
            return (if Fault = 1 then 0 else Base);
         end Reserve;
         function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
         begin
            Commits := Commits + 1;
            pragma Assert (Address = Base and Offset = Committed and Bytes <= 65536);
            if Fault = 2 and Commits = 2 then return False; end if;
            Committed := Offset + Bytes;
            return True;
         end Commit;
         function Clear (Address, Bytes : Unsigned_64) return Boolean is
         begin
            Clears := Clears + 1;
            pragma Assert (Address = Base + Initialized and Initialized + Bytes <= Committed);
            if Fault = 3 and Clears = 2 then return False; end if;
            Initialized := Initialized + Bytes;
            return Intel_GPU_Metadata_Initialize.Clear (Address, Bytes);
         end Clear;
         procedure Publish (Address, Bytes : Unsigned_64; Accepted : out Boolean) is
         begin
            Publications := Publications + 1;
            pragma Assert (Address = Base and Bytes <= Initialized);
            if Fault = 4 and Publications = 2 then Accepted := False; return; end if;
            R.Extend (Registry, Address, Bytes, Accepted);
         end Publish;
         package M is new Intel_GPU_Metadata_Arena (Reserve, Commit, Clear);
         package G is new Intel_GPU_Record_Growth (M, Capacity, Publish);
         use type G.Phase, G.View, G.Failure;
         C : G.Controller;
         Old : G.View;
         Count, Prior_Capacity : Natural;
         OK : Boolean;
      begin
         R.Put (Registry, 1, (1234, False));
         G.Configure (C, 1, 20000, OK); pragma Assert (not OK);
         G.Configure (C, (if Fault = 5 then 65536 else 196608), 20000, OK);
         pragma Assert (OK and Reserves = 0);
         G.Request (C, 16, OK); pragma Assert (OK and G.Snapshot (C).State = G.Idle);
         Old := G.Snapshot (C);
         G.Request (C, 20001, OK); pragma Assert (not OK and G.Snapshot (C) = Old);
         G.Request (C, (if Fault = 0 then 5000 else 10000), OK);
         pragma Assert (OK);
         for Turn in 1 .. 30 loop
            Old := G.Snapshot (C);
            Count := Reserves + Commits + Publications;
            Prior_Capacity := Capacity;
            if Old.State not in G.Idle | G.Failed then
               G.Request (C, 17, OK); pragma Assert (not OK and G.Snapshot (C) = Old);
            end if;
            G.Step (C);
            pragma Assert (Reserves + Commits + Publications <= Count + 1);
            if Old.State /= G.Publishing then pragma Assert (Capacity = Prior_Capacity); end if;
            pragma Assert (R.Get (Registry, 1) = (1234, False));
            if Capacity > 16 then pragma Assert (R.Get (Registry, Capacity) = (73, True)); end if;
            exit when G.Snapshot (C).State in G.Idle | G.Failed;
         end loop;
         if Fault = 0 then
            pragma Assert (G.Snapshot (C).State = G.Idle and Capacity >= 5000);
            pragma Assert (Reserves = 1 and Commits = 2 and Publications = 2);
            R.Put (Registry, 5000, (5678, False));
            G.Request (C, 10000, OK); pragma Assert (OK);
            for Turn in 1 .. 10 loop
               G.Step (C);
               exit when G.Snapshot (C).State in G.Idle | G.Failed;
            end loop;
            pragma Assert (G.Snapshot (C).State = G.Idle and Capacity >= 10000);
            pragma Assert (Reserves = 1 and Commits = 3 and Publications = 3);
            pragma Assert (R.Get (Registry, 5000) = (5678, False));
            G.Request (C, Capacity, OK); pragma Assert (OK);
            Old := G.Snapshot (C);
            Count := Reserves + Commits + Clears + Publications;
            G.Request (C, Capacity + 1, OK);
            pragma Assert (not OK and G.Snapshot (C) = Old);
            for Turn in 1 .. 10 loop G.Step (C); end loop;
            pragma Assert (G.Snapshot (C) = Old and
              Reserves + Commits + Clears + Publications = Count);
            G.Request (C, 5000, OK);
            pragma Assert (OK and G.Snapshot (C).State = G.Idle);
            pragma Assert (R.Get (Registry, 5000) = (5678, False));
         else
            pragma Assert (G.Snapshot (C).State = G.Failed);
            if Fault = 5 then pragma Assert (G.Snapshot (C).Error = G.Byte_Quota_Exhausted); end if;
            Old := G.Snapshot (C);
            Count := Reserves + Commits + Clears + Publications;
            G.Request (C, 17, OK); pragma Assert (not OK);
            for Turn in 1 .. 10 loop G.Step (C); end loop;
            pragma Assert (G.Snapshot (C) = Old and Reserves + Commits + Clears + Publications = Count);
         end if;
         for I in 24576 .. Memory'Last loop pragma Assert (Memory (I) = 16#CAFE#); end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Record growth PASS: real typed storage >10000 records, bounded phases, stable prefix, five fault paths, no replay");
end Record_Growth_Tests;
