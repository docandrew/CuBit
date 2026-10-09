--  CuAlloc's process heap on Linux memory (Linux_Provider): every size path,
--  growth past one arena, alignment, realloc, zeroing, huge blocks given
--  back, running out of quota without losing live blocks, and a random
--  stress whose shadow model checks that live blocks never overlap and keep
--  their contents.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Heap;
with Linux_Provider;

procedure Main is
   Failures, Checks : Natural := 0;
   procedure Check (Good : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Good then
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   function Addr (V : Unsigned_64) return System.Address is (To_Address (Integer_Address (V)));
   procedure Fill (Item, Bytes : Unsigned_64; Value : Storage_Element) is
      B : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => Addr (Item);
   begin
      B := [others => Value];
   end Fill;
   function Holds (Item, Bytes : Unsigned_64; Value : Storage_Element) return Boolean is
      B : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => Addr (Item);
   begin
      return (for all X of B => X = Value);
   end Holds;

   A, B, C : Unsigned_64;
begin
   --  Each path.
   A := Heap.Allocate (24, 16);
   Check (A /= 0 and then A mod 16 = 0 and then Heap.Usable_Size (A) = 32, "small: 24 bytes from the 32 class");
   B := Heap.Allocate (10_000, 16);
   Check (B /= 0 and then B mod 4096 = 0 and then Heap.Usable_Size (B) = 12_288, "medium: whole pages");
   C := Heap.Allocate (3_000_000, 16);
   Check (C /= 0 and then Heap.Usable_Size (C) >= 3_000_000, "huge: its own reservation");
   Fill (A, 24, 1); Fill (B, 10_000, 2); Fill (C, 3_000_000, 3);
   Check (Holds (A, 24, 1) and Holds (B, 10_000, 2) and Holds (C, 3_000_000, 3), "each holds what was written");
   declare
      Reserved : constant Unsigned_64 := Heap.Reserved_Bytes;
   begin
      Heap.Free (C);
      Check (Heap.Reserved_Bytes < Reserved and then Heap.Usable_Size (C) = 0, "a freed huge block goes back");
   end;
   Heap.Free (A); Heap.Free (B);
   Check (Heap.Allocate (24, 16) = A, "a freed small block is reused");
   Check (Heap.Allocate (10_000, 16) = B, "a freed medium run is reused");

   --  Alignment.
   for Shift in 4 .. 30 loop
      declare
         Align : constant Unsigned_64 := 2 ** Shift;
         Item : constant Unsigned_64 := Heap.Allocate (100, Align);
      begin
         Check (Item /= 0 and then Item mod Align = 0, "alignment" & Align'Image);
         Heap.Free (Item);
      end;
   end loop;
   Check (Heap.Allocate (16, 48) = 0, "a non-power-of-two alignment is refused");

   --  Growth past one arena: 40 MiB of 1 KiB blocks.
   declare
      Count : constant := 40 * 1024;
      Items : array (1 .. Count) of Unsigned_64;
      Ok : Boolean := True;
   begin
      for I in Items'Range loop
         Items (I) := Heap.Allocate (1000, 16);
         Ok := Ok and Items (I) /= 0;
      end loop;
      Check (Ok and then Heap.Arena_Count >= 4, "40 MiB of small blocks spread over new arenas");
      for I in Items'Range loop Heap.Free (Items (I)); end loop;
   end;

   --  Realloc and zeroing.
   A := Heap.Allocate (100, 16);
   Fill (A, 100, 9);
   B := Heap.Reallocate (A, 100_000);
   Check (B /= 0 and then B /= A and then Holds (B, 100, 9) and then Heap.Usable_Size (A) = 0,
          "realloc grows into a new block, contents kept, old freed");
   Check (Heap.Reallocate (B, 50) = B, "a smaller realloc stays");
   C := Heap.Reallocate (B, 5_000_000);
   Check (C /= 0 and then Holds (C, 100, 9), "realloc into a huge block");
   Heap.Free (C);
   A := Heap.Allocate (64, 16); Fill (A, 64, 16#FF#); Heap.Free (A);
   B := Heap.Allocate_Zeroed (64, 16);
   Check (B = A and then Holds (B, 64, 0), "a reused block comes back zeroed");
   Heap.Free (B);

   --  Invalid frees are ignored.
   Heap.Free (12345); Heap.Free (A + 8); Heap.Free (0);
   Check (Heap.Usable_Size (12345) = 0, "an unknown address holds nothing");

   --  Running out: refusals leave live blocks alone.
   declare
      Kept : constant Unsigned_64 := Heap.Allocate (512, 16);
   begin
      Fill (Kept, 512, 5);
      Linux_Provider.Quota := Linux_Provider.Committed;
      Check (Heap.Allocate (8_000_000, 16) = 0, "a huge block beyond the quota is refused");
      declare
         Item : Unsigned_64;
         Refused : Boolean := False;
      begin
         for I in 1 .. 100_000 loop
            Item := Heap.Allocate (4000, 16);
            if Item = 0 then Refused := True; exit; end if;
         end loop;
         Check (Refused, "small blocks run out at the quota");
      end;
      Check (Holds (Kept, 512, 5), "a live block survives the refusals");
      Linux_Provider.Quota := Unsigned_64'Last;
      Check (Heap.Allocate (8_000_000, 16) /= 0, "with quota again, it allocates");
   end;

   --  Random stress against a shadow model.
   declare
      LIVE : constant := 2_000;
      type Slot is record
         Item, Size : Unsigned_64 := 0;
         Value : Storage_Element := 0;
      end record;
      Slots : array (1 .. LIVE) of Slot;
      subtype Pick is Positive range 1 .. LIVE;
      package Picks is new Ada.Numerics.Discrete_Random (Pick);
      subtype Sizes is Positive range 1 .. 3_000_000;
      package Size_Picks is new Ada.Numerics.Discrete_Random (Sizes);
      G : Picks.Generator;
      Z : Size_Picks.Generator;
      Corrupt, Overlap, Unaligned : Boolean := False;
   begin
      Picks.Reset (G, 11); Size_Picks.Reset (Z, 13);
      for Step in 1 .. 300_000 loop
         declare
            K : constant Pick := Picks.Random (G);
            R : constant Unsigned_64 := Unsigned_64 (Size_Picks.Random (Z));
         begin
            if Slots (K).Item /= 0 then
               Corrupt := Corrupt or else not Holds (Slots (K).Item, Slots (K).Size, Slots (K).Value);
               Heap.Free (Slots (K).Item);
               Slots (K).Item := 0;
            else
               Slots (K).Size :=
                 (if Step mod 997 = 0 then R elsif Step mod 31 = 0 then R mod 200_000 + 1 else R mod 2_000 + 1);
               Slots (K).Item := Heap.Allocate (Slots (K).Size, 16);
               Slots (K).Value := Storage_Element (Step mod 251);
               if Slots (K).Item /= 0 then
                  Unaligned := Unaligned or else Slots (K).Item mod 16 /= 0;
                  Fill (Slots (K).Item, Slots (K).Size, Slots (K).Value);
               end if;
            end if;
         end;
      end loop;
      for I in Slots'Range loop
         if Slots (I).Item /= 0 then
            Corrupt := Corrupt or else not Holds (Slots (I).Item, Slots (I).Size, Slots (I).Value);
            for J in I + 1 .. Slots'Last loop
               if Slots (J).Item /= 0 then
                  Overlap := Overlap or else
                    (Slots (I).Item < Slots (J).Item + Slots (J).Size and then
                     Slots (J).Item < Slots (I).Item + Slots (I).Size);
               end if;
            end loop;
         end if;
      end loop;
      Check (not Corrupt, "300,000 random steps: no block corrupted");
      Check (not Overlap, "no two live blocks overlap");
      Check (not Unaligned, "every block 16-byte aligned");
   end;
   Check (Linux_Provider.Contract_Violations = 0, "the provider's contract is never broken");

   Put_Line ("cualloc:" & Checks'Image & " checks," & Failures'Image & " failures; arenas"
             & Heap.Arena_Count'Image & ", reserved" & Heap.Reserved_Bytes'Image
             & ", committed" & Heap.Committed_Bytes'Image);
   if Failures > 0 then Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure); end if;
end Main;
