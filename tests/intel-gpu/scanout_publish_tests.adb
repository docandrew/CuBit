with Ada.Text_IO; with Interfaces;
with Intel_GPU_GGTT_Publish; with Intel_GPU_GGTT_Reservations;
with Intel_GPU_Scanout_Inventory;
with Intel_GPU_Display_Presence;
procedure Scanout_Publish_Tests is
   use Interfaces;
   use type Intel_GPU_Scanout_Inventory.Outcome;
   procedure Run (Mode : Natural) is
      P : Intel_GPU_Scanout_Inventory.Planes := (others => (Collected => True, others => <>));
      C : Intel_GPU_Scanout_Inventory.Cursors := (others => (Collected => True, others => <>));
      Inventory : Intel_GPU_Scanout_Inventory.Inventory;
      Presence : Intel_GPU_Display_Presence.Snapshot :=
        (True, [others => Intel_GPU_Display_Presence.Present]);
      Table : array (0 .. 8) of Unsigned_64 := (others => 0);
      Reads, Writes, Prepares, Flushes : Natural := 0;
      function Allowed (First, Bytes : Unsigned_64) return Boolean is
        (Intel_GPU_Scanout_Inventory.No_Scanout_Overlap (Inventory, (True, First, Bytes)));
      procedure Prepare (First, Bytes : Unsigned_64; OK : out Boolean) is
      begin
         pragma Assert (First = 8192 and Bytes = 4096);
         Prepares := Prepares + 1; OK := True;
         if Mode in 2 | 4 | 5 | 6 | 8 then
            if Mode = 2 then C (4).Collected := False;
            elsif Mode = 4 then Presence.Known := False;
            elsif Mode = 5 then
               -- A valid updated snapshot can now overlap the reservation.
               -- Completeness alone must not permit publication.
               P (1).Before.Surface := 8192;
               P (1).Before.Live_Surface := 8192;
               P (1).After := P (1).Before;
            elsif Mode = 8 then
               C (4).Before.Control := 16#08#;
               C (4).After := C (4).Before;
            else
               -- A previously disabled plane becomes enabled on that page.
               P (2).Before := (16#84000000#, 1, 15, 0, 8192, 8192);
               P (2).After := P (2).Before;
            end if;
            Inventory := Intel_GPU_Scanout_Inventory.Collect (P, C, 2_097_152, Presence);
            if Mode in 5 | 6 then
               pragma Assert (Inventory.Status = Intel_GPU_Scanout_Inventory.Complete);
               pragma Assert (Inventory.Count = (if Mode = 5 then 1 else 2));
            end if;
         end if;
      end Prepare;
      procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64; OK : out Boolean) is
      begin
         pragma Assert (Index = 2); -- Protected page1 must not even be searched.
         Reads := Reads + 1; Value := Table (Natural (Index)); OK := True;
      end Read_PTE;
      procedure Write_PTE (Index, Value : Unsigned_64; OK : out Boolean) is
      begin
         pragma Assert (Index = 2 and Allowed (8192, 4096));
         Writes := Writes + 1; Table (Natural (Index)) := Value; OK := True;
      end Write_PTE;
      procedure Flush (OK : out Boolean) is
      begin Flushes := Flushes + 1; OK := True; end Flush;
      package Publisher is new Intel_GPU_GGTT_Publish (Allowed, Prepare, Read_PTE, Write_PTE, Flush);
      use type Publisher.Result;
      Ledger : Intel_GPU_GGTT_Reservations.Ledger;
      Attempt : Publisher.Attempt;
      Status : Publisher.Result;
      OK : Boolean;
      Selected : Unsigned_64;
   begin
      P (1).Before := (16#84000000#, 1, 15, 0, 4096, 4096);
      P (1).After := P (1).Before;
      if Mode = 1 then C (4).Collected := False; end if;
      if Mode = 3 then Presence.Known := False; end if;
      if Mode = 7 then
         C (4).Before.Control := 16#10#;
         C (4).After := C (4).Before;
      end if;
      Inventory := Intel_GPU_Scanout_Inventory.Collect (P, C, 2_097_152, Presence);
      Intel_GPU_GGTT_Reservations.Admit (Ledger, 2_097_152, 4096, 8 * 4096, OK);
      pragma Assert (OK);
      if Mode in 1 | 3 | 7 then
         Publisher.Publish (Attempt, Ledger, 8192, 16#100000#, 4096, Status);
         pragma Assert (Status = Publisher.Protected_Range and Reads = 0 and Prepares = 0);
         pragma Assert (Writes = 0 and Flushes = 0);
      else
         Publisher.Publish_Available (Attempt, Ledger, 16#100000#, 4096, 4096, Selected, Status);
         pragma Assert (Selected = 8192 and Prepares = 1);
         pragma Assert (Intel_GPU_GGTT_Reservations.Count (Ledger) = 1);
         if Mode = 0 then
            pragma Assert (Status = Publisher.Published and Writes = 1 and Flushes = 1 and Reads = 2);
         else
            pragma Assert (Status = Publisher.Protected_Range and Writes = 0 and Flushes = 0);
         end if;
      end if;
      pragma Assert (Table (1) = 0);
   end Run;
begin
   for Mode in 0 .. 8 loop Run (Mode); end loop;
   Ada.Text_IO.Put_Line ("scanout/publication PASS: protected page skipped, incomplete inventory rejected, preparation invalidation retains claim without writes");
end Scanout_Publish_Tests;
