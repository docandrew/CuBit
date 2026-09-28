------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Chunk_Pool with SPARK_Mode is

   --  Changing one chunk's owner moves one unit between two counts.
   procedure Lemma_Count_Update (A, B : Owner_Array; C : Chunk_Id; N : Chunk_Total) with
     Ghost, Global => null,
     Pre  => N <= Chunk_Count and then (for all D in Chunk_Id => (if D /= C then A (D) = B (D))),
     Post => (for all O in Maybe_Owner =>
                Count (B, O, N) =
                  (if C > N then Count (A, O, N)
                   elsif A (C) = B (C) then Count (A, O, N)
                   elsif B (C) = O then Count (A, O, N) + 1
                   elsif A (C) = O then Count (A, O, N) - 1
                   else Count (A, O, N))),
     Subprogram_Variant => (Decreases => N);
   procedure Lemma_Count_Update (A, B : Owner_Array; C : Chunk_Id; N : Chunk_Total) is
   begin
      if N > 0 then
         Lemma_Count_Update (A, B, C, N - 1);
      end if;
   end Lemma_Count_Update;

   --  A count of all chunks of one value, when every chunk has it.
   procedure Lemma_Count_All (A : Owner_Array; O : Maybe_Owner; N : Chunk_Total) with
     Ghost, Global => null,
     Pre  => N <= Chunk_Count and then (for all D in Chunk_Id => A (D) = O),
     Post => Count (A, O, N) = N and then
             (for all O2 in Maybe_Owner => (if O2 /= O then Count (A, O2, N) = 0)),
     Subprogram_Variant => (Decreases => N);
   procedure Lemma_Count_All (A : Owner_Array; O : Maybe_Owner; N : Chunk_Total) is
   begin
      if N > 0 then
         Lemma_Count_All (A, O, N - 1);
      end if;
   end Lemma_Count_All;

   --  A chunk with another value keeps the count below N.
   procedure Lemma_Count_Below (A : Owner_Array; O : Maybe_Owner; C : Chunk_Id; N : Chunk_Total) with
     Ghost, Global => null,
     Pre  => N <= Chunk_Count and then C <= N and then A (C) /= O,
     Post => Count (A, O, N) <= N - 1,
     Subprogram_Variant => (Decreases => N);
   procedure Lemma_Count_Below (A : Owner_Array; O : Maybe_Owner; C : Chunk_Id; N : Chunk_Total) is
   begin
      if C < N then
         Lemma_Count_Below (A, O, C, N - 1);
      end if;
   end Lemma_Count_Below;

   procedure Lemma_Equal (A, B : Pool) is null;

   procedure Initialize (P : out Pool) is
   begin
      P := (Store   => [others => [others => 0]],
            Owners  => [others => Free],
            Stack   => [for K in Chunk_Id => K],
            Pos     => [for K in Chunk_Id => K],
            Top     => Chunk_Count,
            Counter => [others => 0]);
      Lemma_Count_All (P.Owners, Free, Chunk_Count);
   end Initialize;

   procedure Allocate (P : in out Pool; O : Owner_Id; C : out Chunk_Id; OK : out Boolean) is
      Old : constant Pool := P with Ghost;
   begin
      if P.Top = 0 then
         C := 1;
         OK := False;
         return;
      end if;
      OK := True;
      C := P.Stack (P.Top);
      pragma Assert (P.Owners (C) = Free and then P.Pos (C) = P.Top);
      P.Top := P.Top - 1;
      P.Pos (C) := 0;
      P.Owners (C) := O;
      Lemma_Count_Update (Old.Owners, P.Owners, C, Chunk_Count);
      P.Counter (O) := P.Counter (O) + 1;
   end Allocate;

   procedure Release (P : in out Pool; O : Owner_Id; C : Chunk_Id) is
      Old : constant Pool := P with Ghost;
   begin
      Lemma_Count_Below (P.Owners, Free, C, Chunk_Count);
      P.Store (C) := [others => 0];
      P.Top := P.Top + 1;
      P.Stack (P.Top) := C;
      P.Pos (C) := P.Top;
      P.Owners (C) := Free;
      Lemma_Count_Update (Old.Owners, P.Owners, C, Chunk_Count);
      P.Counter (O) := P.Counter (O) - 1;
   end Release;

   procedure Write (P : in out Pool; O : Owner_Id; C : Chunk_Id; I : Chunk_Index;
                    V : Unsigned_8)
   is
   begin
      P.Store (C) (I) := V;
   end Write;

   procedure Write_Slice (P : in out Pool; O : Owner_Id; C : Chunk_Id; At_Index : Chunk_Index;
                          Bytes : Byte_Array)
   is
   begin
      --  One block copy (the slice slides from Bytes' bounds).
      P.Store (C) (At_Index .. At_Index + Bytes'Length - 1) := Bytes;
   end Write_Slice;

   procedure Read_Slice (P : Pool; O : Owner_Id; C : Chunk_Id; At_Index : Chunk_Index;
                         Into : out Byte_Array)
   is
   begin
      Into := P.Store (C) (At_Index .. At_Index + Into'Length - 1);
   end Read_Slice;

   function Read (P : Pool; O : Owner_Id; C : Chunk_Id; I : Chunk_Index) return Unsigned_8 is
     (P.Store (C) (I));
end Chunk_Pool;
