with Interfaces;
generic
   -- True only for an admitted page free of retained software claims and
   -- protected scanout/platform ranges. Combine Space_Free on the publication
   -- ledger with the owner's exclusion predicate. The caller
   -- serializes this query and PTE reads against ledger/hardware mutation.
   with function Page_Available (Address : Interfaces.Unsigned_64) return Boolean;
   -- Bounded nonraising read under retained exclusive GGTT ownership.
   with procedure Read_PTE
     (Index : Interfaces.Unsigned_64; Value : out Interfaces.Unsigned_64;
      Success : out Boolean);
package Intel_GPU_GGTT_Search is
   type Result is (Rejected, Read_Failed, Exhausted, Found);
   type Evidence is record
      Outcome : Result := Rejected;
      Reads, Blocked, Nonzero : Interfaces.Unsigned_64 := 0;
      First_Nonzero_Index, First_Nonzero_Value : Interfaces.Unsigned_64 := 0;
   end record;
   -- Per-instance snapshot of the most recent Find; no extra hardware reads.
   -- Single-owner/non-reentrant, like the callbacks and ledger.
   function Last_Evidence return Evidence;
   -- Proposal only: no authority, reservation, writes or ledger replacement.
   -- The caller independently admits [First, First+Length). Page_Available
   -- excludes live/pending scanout, platform reservations and existing software
   -- claims, including failed/unpublished reservations.
   -- Revalidate/reserve with the publication ledger under the same ownership.
   -- Reads each visited entry at most once; at most Table_Bytes/8 callbacks.
   procedure Find
     (Admitted : Boolean;
      Table_Bytes, First, Length, Bytes, Alignment : Interfaces.Unsigned_64;
      Selected : out Interfaces.Unsigned_64; Status : out Result);
end Intel_GPU_GGTT_Search;
