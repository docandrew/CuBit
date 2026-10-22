package body AML_Scalar_Ledger.Test_Inspection is
   function Scanned_Objects (Store : State) return Natural is
      Count : Natural := 0;
   begin
      -- Frozen pre-optimization definition, independent of the new cache.
      for Item of Store.Values loop
         if Item.Count /= 0 then Count := Count + 1; end if;
      end loop;
      return Count;
   end Scanned_Objects;
   function Issued (Store : State) return Natural is (Store.Last);
   procedure Set_Cached_Count (Store : in out State; Count : Natural) is
   begin Store.Live_Objects := Count; end Set_Cached_Count;
end AML_Scalar_Ledger.Test_Inspection;
