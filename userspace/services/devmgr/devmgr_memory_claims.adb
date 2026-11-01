pragma Ada_2022;

package body Devmgr_Memory_Claims with SPARK_Mode is

   procedure Initialize (Item : out Ledger) is
   begin
      Item := (others => <>);
   end Initialize;

   procedure Claim_ACPI
     (Item : in out Ledger; Physical, Bytes : Unsigned_64; Granted : out Boolean)
   is
      Wanted : Claim;
      Same : Boolean := False;
   begin
      Granted := False;
      if not Whole_Pages (Physical, Bytes) then
         return;
      end if;
      Wanted := Pages_Of (Physical, Bytes);
      for K in 1 .. Item.Count loop
         if Item.Claims (K) = Wanted then
            Same := True;
         elsif not Disjoint (Item.Claims (K), Physical, Bytes) then
            return;        --  a different, overlapping ACPI window
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. K =>
              Item.Claims (J) = Wanted or else Disjoint (Item.Claims (J), Physical, Bytes));
         pragma Loop_Invariant
           (if Same then (for some J in 1 .. K => Item.Claims (J) = Wanted));
      end loop;
      if Same then
         Granted := True;
      elsif Item.Count < Maximum_Claims then
         Item.Count := Item.Count + 1;
         Item.Claims (Item.Count) := Wanted;
         Granted := True;
      end if;
   end Claim_ACPI;

end Devmgr_Memory_Claims;
