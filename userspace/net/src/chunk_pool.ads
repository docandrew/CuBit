------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A pool of fixed-size chunks for network buffers, shared by every
--  connection's queues (docs/netstack-redesign.md, "Scale and denial of
--  service"). Allocation and release take constant time.
--
--  Proved (tests/net-tcp):
--  - ownership: every chunk is free or owned by exactly one owner; only
--    its owner writes, reads or releases it (preconditions, so misuse is
--    a proof failure at the call site, not a runtime check);
--  - no leaks between owners: a free chunk holds only zeros, so an
--    allocated chunk starts zeroed whatever it held before;
--  - accounting: Held (O) is exactly the number of chunks O owns, and
--    Free_Count the number free (the basis of per-scope quotas);
--  - frame: an operation on one chunk changes no other chunk's owner or
--    bytes.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

generic
   Chunk_Count : Positive;   --  chunks in the pool
   Chunk_Size  : Positive;   --  bytes per chunk
   Owner_Count : Positive;   --  owners are 1 .. Owner_Count
package Chunk_Pool with SPARK_Mode is

   subtype Chunk_Id is Positive range 1 .. Chunk_Count;
   --  How many chunks: free, or held by one owner.
   subtype Chunk_Total is Natural range 0 .. Chunk_Count;

   --  A chunk's owner, or Free.
   subtype Maybe_Owner is Natural range 0 .. Owner_Count;
   Free : constant Maybe_Owner := 0;
   subtype Owner_Id is Maybe_Owner range 1 .. Owner_Count;

   subtype Chunk_Index is Positive range 1 .. Chunk_Size;
   type Byte_Array is array (Positive range <>) of Unsigned_8;
   subtype Block is Byte_Array (Chunk_Index);

   type Pool is private;

   function Valid (P : Pool) return Boolean with Ghost;
   function Owner (P : Pool; C : Chunk_Id) return Maybe_Owner;
   function Contents (P : Pool; C : Chunk_Id) return Block;
   function Data (P : Pool; C : Chunk_Id; I : Chunk_Index) return Unsigned_8 is
     (Contents (P, C) (I));
   function Free_Count (P : Pool) return Chunk_Total;
   function Held (P : Pool; O : Owner_Id) return Chunk_Total;

   --  The same owners, and the same bytes, except in chunk C.
   function Same_Except (A, B : Pool; C : Chunk_Id) return Boolean is
     (for all D in Chunk_Id =>
        (if D /= C then Owner (A, D) = Owner (B, D) and then Contents (A, D) = Contents (B, D)));

   --  Equal pools agree on every chunk (the prover needs this spelled out
   --  for copies of a private type).
   procedure Lemma_Equal (A, B : Pool) with
     Ghost, Global => null,
     Pre  => A = B,
     Post => (for all C in Chunk_Id =>
                Owner (A, C) = Owner (B, C) and then Contents (A, C) = Contents (B, C)) and then
             Free_Count (A) = Free_Count (B) and then
             (for all O in Owner_Id => Held (A, O) = Held (B, O));

   procedure Initialize (P : out Pool) with
     Post => Valid (P) and then Free_Count (P) = Chunk_Count and then
             (for all C in Chunk_Id => Owner (P, C) = Free) and then
             (for all O in Owner_Id => Held (P, O) = 0);

   procedure Allocate (P : in out Pool; O : Owner_Id; C : out Chunk_Id; OK : out Boolean) with
     Pre  => Valid (P),
     Post => Valid (P) and then OK = (Free_Count (P'Old) > 0) and then
             (if OK then
                Owner (P'Old, C) = Free and then Owner (P, C) = O and then
                (for all D in Chunk_Id => (if D /= C then Owner (P, D) = Owner (P'Old, D))) and then
                (for all I in Chunk_Index => Data (P, C, I) = 0) and then
                Same_Except (P, P'Old, C) and then
                Free_Count (P) = Free_Count (P'Old) - 1 and then
                Held (P, O) = Held (P'Old, O) + 1 and then
                (for all O2 in Owner_Id =>
                   (if O2 /= O then Held (P, O2) = Held (P'Old, O2)))
              else P = P'Old);

   --  O returns chunk C; its bytes are cleared.
   procedure Release (P : in out Pool; O : Owner_Id; C : Chunk_Id) with
     Pre  => Valid (P) and then Owner (P, C) = O,
     Post => Valid (P) and then Owner (P, C) = Free and then
             Same_Except (P, P'Old, C) and then
             Free_Count (P) = Free_Count (P'Old) + 1 and then
             Held (P, O) = Held (P'Old, O) - 1 and then
             (for all O2 in Owner_Id =>
                (if O2 /= O then Held (P, O2) = Held (P'Old, O2)));

   procedure Write (P : in out Pool; O : Owner_Id; C : Chunk_Id; I : Chunk_Index;
                    V : Unsigned_8)
   with
     Pre  => Valid (P) and then Owner (P, C) = O,
     Post => Valid (P) and then Data (P, C, I) = V and then
             (for all J in Chunk_Index => (if J /= I then Data (P, C, J) = Data (P'Old, C, J))) and then
             Owner (P, C) = O and then Same_Except (P, P'Old, C) and then
             Free_Count (P) = Free_Count (P'Old) and then
             (for all O2 in Owner_Id => Held (P, O2) = Held (P'Old, O2));

   --  Copy Bytes into chunk C from index At_Index onward.
   procedure Write_Slice (P : in out Pool; O : Owner_Id; C : Chunk_Id; At_Index : Chunk_Index;
                          Bytes : Byte_Array)
   with
     Pre  => Valid (P) and then Owner (P, C) = O and then
             Bytes'Length <= Chunk_Size - At_Index + 1,
     Post => Valid (P) and then Owner (P, C) = O and then Same_Except (P, P'Old, C) and then
             (for all D in Chunk_Id => Owner (P, D) = Owner (P'Old, D)) and then
             (for all Z in At_Index .. At_Index + Bytes'Length - 1 =>
                Data (P, C, Z) = Bytes (Bytes'First + (Z - At_Index))) and then
             (for all J in Chunk_Index =>
                (if J < At_Index or else J >= At_Index + Bytes'Length then
                   Data (P, C, J) = Data (P'Old, C, J))) and then
             Free_Count (P) = Free_Count (P'Old) and then
             (for all O2 in Owner_Id => Held (P, O2) = Held (P'Old, O2));

   --  Copy Into'Length bytes of chunk C from index At_Index onward.
   procedure Read_Slice (P : Pool; O : Owner_Id; C : Chunk_Id; At_Index : Chunk_Index;
                         Into : out Byte_Array)
   with
     Pre  => Valid (P) and then Owner (P, C) = O and then
             Into'Length <= Chunk_Size - At_Index + 1,
     Post => (for all Z in Into'Range => Into (Z) = Data (P, C, At_Index + (Z - Into'First)));

   function Read (P : Pool; O : Owner_Id; C : Chunk_Id; I : Chunk_Index) return Unsigned_8
   with Pre  => Valid (P) and then Owner (P, C) = O,
        Post => Read'Result = Data (P, C, I);

private
   type Chunk_Store is array (Chunk_Id) of Block;
   type Owner_Array is array (Chunk_Id) of Maybe_Owner;
   type Id_Stack is array (Chunk_Id) of Chunk_Id;
   type Position_Array is array (Chunk_Id) of Chunk_Total;   --  0: not free
   type Counter_Array is array (Owner_Id) of Chunk_Total;

   type Pool is record
      Store   : Chunk_Store;
      Owners  : Owner_Array;
      Stack   : Id_Stack;         --  free chunks: Stack (1 .. Top)
      Pos     : Position_Array;   --  a free chunk's place in Stack, else 0
      Top     : Chunk_Total;
      Counter : Counter_Array;    --  chunks held per owner
   end record;

   --  Chunks among 1 .. N owned by O.
   function Count (A : Owner_Array; O : Maybe_Owner; N : Chunk_Total) return Chunk_Total is
     (if N = 0 then 0 else Count (A, O, N - 1) + (if A (N) = O then 1 else 0))
   with Ghost, Pre => N <= Chunk_Count, Post => Count'Result <= N,
        Subprogram_Variant => (Decreases => N);

   function Valid (P : Pool) return Boolean is
     ((for all K in 1 .. P.Top =>
         P.Owners (P.Stack (K)) = Free and then P.Pos (P.Stack (K)) = K) and then
      (for all C in Chunk_Id =>
         (if P.Owners (C) = Free then P.Pos (C) in 1 .. P.Top and then P.Stack (P.Pos (C)) = C
          else P.Pos (C) = 0)) and then
      (for all C in Chunk_Id =>
         (if P.Owners (C) = Free then (for all I in Chunk_Index => P.Store (C) (I) = 0))) and then
      P.Top = Count (P.Owners, Free, Chunk_Count) and then
      (for all O in Owner_Id => P.Counter (O) = Count (P.Owners, O, Chunk_Count)));

   function Owner (P : Pool; C : Chunk_Id) return Maybe_Owner is (P.Owners (C));
   function Contents (P : Pool; C : Chunk_Id) return Block is (P.Store (C));
   function Free_Count (P : Pool) return Chunk_Total is (P.Top);
   function Held (P : Pool; O : Owner_Id) return Chunk_Total is (P.Counter (O));
end Chunk_Pool;
