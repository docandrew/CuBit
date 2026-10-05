package body Compositor_Glyph_Cache with SPARK_Mode is
   --  Proved free of run-time errors; tests/ui-raster/run.sh re-proves every
   --  unit carrying this pragma and fails on any unproved check.
   pragma Suppress (All_Checks);
   -- Inductive accounting lemmas over a single changed slot. All storage and
   -- reader tables are fixed-size; no heap or foreign calls occur here.
   procedure Changed_Bytes (Before, After : Items; I : Slot) with Ghost,
     Pre => (for all J in Slot => (if J /= I then Before (J) = After (J))),
     Post => Prefix (After, Slot'Last) + Before (I).Bytes =
             Prefix (Before, Slot'Last) + After (I).Bytes
   is
   begin
      for N in Search_Result loop
         pragma Loop_Invariant
           (Prefix (After, N) + (if I <= N then Before (I).Bytes else 0) =
            Prefix (Before, N) + (if I <= N then After (I).Bytes else 0));
      end loop;
   end Changed_Bytes;
   procedure Changed_Readers (Before, After : Readers; I : Reader_Slot) with Ghost,
     Pre => (for all J in Reader_Slot => (if J /= I then Before (J) = After (J))),
     Post => Read_Prefix (After, Reader_Slot'Last) + (if Before (I).Identity = 0 then 0 else 1) =
             Read_Prefix (Before, Reader_Slot'Last) + (if After (I).Identity = 0 then 0 else 1)
   is
   begin
      for N in 0 .. Reader_Slot'Last loop
         pragma Loop_Invariant
           (Read_Prefix (After, N) + (if I <= N and then Before (I).Identity /= 0 then 1 else 0) =
            Read_Prefix (Before, N) + (if I <= N and then After (I).Identity /= 0 then 1 else 0));
      end loop;
   end Changed_Readers;
   function Find (S : State; K : Key) return Search_Result is
   begin
      for I in Slot loop
         if Matches (S, I, K) then return I; end if;
         pragma Loop_Invariant (for all J in Slot'First .. I => not Matches (S, J, K));
      end loop;
      return 0;
   end Find;
   function Victim (S : State) return Search_Result is
      I : Slot;
   begin
      for Offset in 0 .. Slot'Last - 1 loop
         I := (S.Cursor - 1 + Offset) mod Slot'Last + 1;
         if S.Masks (I).Stage = Ready and then S.Masks (I).Identity > 0 and then not Pinned (S, I) then return I; end if;
      end loop;
      return 0;
   end Victim;
   procedure Reserve (S : in out State; K : Key; T : out Token) is
      Size : constant Positive := L.Plan (K.Scale).Bytes;
      Before : constant Items := S.Masks with Ghost;
   begin
      T := No_Token;
      if S.Issued = Last_Identity or else Find (S, K) /= 0 or else Size > S.Budget - Charged (S) then return; end if;
      for I in Slot loop
         if S.Masks (I).Stage = Free then
            S.Issued := Compositor_Identity.Next (S.Issued, Last_Identity);
            S.Masks (I) := (Building, S.Issued, K, Size);
            Changed_Bytes (Before, S.Masks, I);
            T := (I, S.Issued);
            return;
         end if;
      end loop;
   end Reserve;
   procedure Publish (S : in out State; T : Token; Raster_Completed : Boolean) is
      Before : constant Items := S.Masks with Ghost;
   begin
      if Current (S, T) and then S.Masks (T.Position).Stage = Building and then Raster_Completed then
         S.Masks (T.Position).Stage := Ready;
         Changed_Bytes (Before, S.Masks, T.Position);
      end if;
   end Publish;
   procedure Acquire (S : in out State; T : Token; R : out Lease) is
      Before : constant Readers := S.Reading with Ghost;
   begin
      R := No_Lease;
      if not Current (S, T) or else S.Masks (T.Position).Stage /= Ready or else S.Read_Issued = Last_Identity then return; end if;
      for J in Reader_Slot loop
         if S.Reading (J).Identity = 0 then
            S.Read_Issued := Compositor_Identity.Next (S.Read_Issued, Last_Identity);
            S.Reading (J) := (S.Read_Issued, T);
            Changed_Readers (Before, S.Reading, J);
            R := (J, S.Read_Issued);
            return;
         end if;
      end loop;
   end Acquire;
   procedure Complete (S : in out State; R : Lease; Quiescent : Boolean) is
      Before : constant Readers := S.Reading with Ghost;
   begin
      if Active (S, R) and then Quiescent then
         S.Reading (R.Position) := (0, No_Token);
         Changed_Readers (Before, S.Reading, R.Position);
      end if;
   end Complete;
   procedure Begin_Retirement (S : in out State; T : Token; Accepted : out Boolean) is
      Before : constant Items := S.Masks with Ghost;
   begin
      Accepted := Current (S, T) and then S.Masks (T.Position).Stage in Building | Ready and then not Pinned (S, T.Position);
      if Accepted then
         S.Masks (T.Position).Stage := Retiring;
         S.Cursor := T.Position mod Slot'Last + 1;
         Changed_Bytes (Before, S.Masks, T.Position);
      end if;
   end Begin_Retirement;
   procedure Retired (S : in out State; T : Token; Confirmed : Boolean) is
      Before : constant Items := S.Masks with Ghost;
   begin
      if Current (S, T) and then S.Masks (T.Position).Stage = Retiring and then Confirmed then
         S.Masks (T.Position) := (others => <>);
         Changed_Bytes (Before, S.Masks, T.Position);
      end if;
   end Retired;
end Compositor_Glyph_Cache;
