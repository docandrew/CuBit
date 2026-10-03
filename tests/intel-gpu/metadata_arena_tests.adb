with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
with System.Storage_Elements; use System.Storage_Elements;
procedure Metadata_Arena_Tests is
   Base : constant Unsigned_64 := 16#1000_0000#;
   Reserved, Committed, Initialized : Unsigned_64 := 0;
   Reserves, Commits, Clears : Natural := 0;
   Fail_Commit, Fail_Clear : Natural := 0;
   Fail_Reserve : Boolean := False;
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
   begin
      Reserves := Reserves + 1; Reserved := Bytes;
      return (if Fail_Reserve then Unsigned_64'Last else Base);
   end Reserve;
   function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
   begin
      Commits := Commits + 1;
      pragma Assert (Address = Base and Offset = Committed and Bytes > 0 and
        Bytes <= 65536 and Bytes mod 4096 = 0 and Offset + Bytes <= Reserved);
      if Commits = Fail_Commit then return False; end if;
      Committed := Committed + Bytes; return True;
   end Commit;
   function Initialize (Address, Bytes : Unsigned_64) return Boolean is
   begin
      Clears := Clears + 1;
      pragma Assert (Address = Base + Initialized and Initialized + Bytes <= Committed);
      if Clears = Fail_Clear then return False; end if;
      Initialized := Initialized + Bytes; return True;
   end Initialize;
   package M is new Intel_GPU_Metadata_Arena (Reserve, Commit, Initialize);
   use type M.State, M.View;
   procedure Reset is
   begin
      Reserved := 0; Committed := 0; Initialized := 0;
      Reserves := 0; Commits := 0; Clears := 0;
      Fail_Commit := 0; Fail_Clear := 0; Fail_Reserve := False;
   end Reset;
   OK : Boolean;
begin
   for Failure in 0 .. 10 loop
      Reset;
      declare
         A : M.Arena;
         Old : M.View;
         Count : Natural;
      begin
         M.Open (A, 0, OK); pragma Assert (not OK and Reserves = 0);
         M.Open (A, 1, OK); pragma Assert (not OK and Reserves = 0);
         M.Open (A, 2 ** 40, OK); pragma Assert (OK and Reserves = 1 and Commits = 0);
         -- A TiB reservation in the fake backend is arithmetic evidence only,
         -- NOT evidence of native TiB allocation or available physical memory.
         M.Request (A, 1, OK); pragma Assert (OK);
         M.Step (A);
         pragma Assert (M.Snapshot (A).Published = 4096 and M.Address (A, 0, 4096) = Base);
         Old := M.Snapshot (A);
         M.Request (A, 2 ** 40 + 1, OK); pragma Assert (not OK and M.Snapshot (A) = Old);
         M.Open (A, 4096, OK); pragma Assert (not OK and Reserves = 1);
         if Failure in 1 .. 5 then Fail_Commit := Failure + 1;
         elsif Failure in 6 .. 10 then Fail_Clear := Failure - 4; end if;
         M.Request (A, 4096 + 5 * 65536, OK); pragma Assert (OK);
         Old := M.Snapshot (A);
         M.Request (A, 8192, OK); pragma Assert (not OK and M.Snapshot (A) = Old);
         for Chunk in 1 .. 5 loop
            Count := Commits;
            M.Step (A);
            pragma Assert (Commits <= Count + 1);
            pragma Assert (M.Address (A, 0, 4096) = Base);
            if Chunk < 5 or Failure /= 0 then
               pragma Assert (M.Address (A, 4096, 1) = 0);
            end if;
         end loop;
         if Failure = 0 then
            pragma Assert (M.Snapshot (A).Phase = M.Ready and
              M.Address (A, 4096, 5 * 65536) = Base + 4096);
         else
            pragma Assert (M.Snapshot (A).Phase = M.Failed and M.Snapshot (A).Published = 4096);
            Old := M.Snapshot (A); Count := Commits;
            M.Step (A); M.Request (A, 8192, OK);
            pragma Assert (not OK and M.Snapshot (A) = Old and Commits = Count);
         end if;
         pragma Assert (M.Address (A, Unsigned_64'Last, 1) = 0);
      end;
   end loop;
   Reset; Fail_Reserve := True;
   declare A : M.Arena; begin
      M.Open (A, 4096, OK); pragma Assert (not OK and M.Snapshot (A).Phase = M.Failed);
      M.Open (A, 4096, OK); pragma Assert (not OK and Reserves = 1);
   end;
   Ada.Text_IO.Put_Line ("Metadata arena PASS: stable prefix, bounded growth, 64-bit quota, ten injected commit/clear failures, no replay");
   declare
      type Words is array (Natural range 0 .. 3 * 8192 - 1) of Unsigned_64;
      Memory : Words := [others => 16#CAFE_BABE#] with Alignment => 4096;
      Address : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory'Address));
   begin
      pragma Assert (not Intel_GPU_Metadata_Initialize.Clear (0, 4096));
      pragma Assert (not Intel_GPU_Metadata_Initialize.Clear (Address + 1, 4096));
      pragma Assert (not Intel_GPU_Metadata_Initialize.Clear (Address, 0));
      pragma Assert (not Intel_GPU_Metadata_Initialize.Clear (Address, 65537));
      pragma Assert (not Intel_GPU_Metadata_Initialize.Clear (Unsigned_64'Last - 4095, 4096));
      pragma Assert (Intel_GPU_Metadata_Initialize.Clear (Address + 65536, 65536));
      for I in Memory'Range loop
         pragma Assert (Memory (I) = (if I in 8192 .. 16383 then 0 else 16#CAFE_BABE#));
      end loop;
      Ada.Text_IO.Put_Line ("Metadata initialization PASS: actual 64KiB clear, neighboring sentinels preserved, invalid spans rejected");
   end;
end Metadata_Arena_Tests;
