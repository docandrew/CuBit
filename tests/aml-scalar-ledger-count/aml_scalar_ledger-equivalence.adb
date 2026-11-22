package body AML_Scalar_Ledger.Equivalence is
   function Frozen_Valid (Store : State) return Boolean is
      Counts : array (Object_Slot) of Owner_Count := [others => 0];
      Live_Count : Object_Index := 0;
   begin
      for Item of Store.Leases loop
         if Item.Object /= 0 then
            if not AML_Frame_Handles.Has_Authority (Store.Domain)
              or else Item.Serial = 0 or else Item.Serial > Store.Last
              or else Counts (Item.Object) = Owner_Count'Last
            then return False; end if;
            Counts (Item.Object) := Counts (Item.Object) + 1;
         elsif Item.Serial /= 0 then return False;
         end if;
      end loop;
      for Index in Object_Slot loop
         if Store.Values (Index).Count /= Counts (Index) then return False; end if;
         if Store.Values (Index).Count /= 0 then Live_Count := Live_Count + 1; end if;
      end loop;
      return Store.Live_Objects = Live_Count;
   end Frozen_Valid;

   -- Executable independent specification: no production ghost routine called.
   function Reference_Model (Store : State) return Boolean is
      Total : Natural := 0;
      Count : Natural;
   begin
      for Item of Store.Leases loop
         if Item.Object = 0 then
            if Item.Serial /= 0 then return False; end if;
         elsif not AML_Frame_Handles.Has_Authority (Store.Domain)
           or else Item.Serial = 0 or else Item.Serial > Store.Last
         then return False;
         end if;
      end loop;
      for O in Object_Slot loop
         Count := 0;
         for R in Root_Slot loop
            if Store.Leases (R).Object = O then Count := Count + 1; end if;
         end loop;
         if Count /= Store.Values (O).Count then return False; end if;
         if Store.Values (O).Count /= 0 then Total := Total + 1; end if;
      end loop;
      return Store.Live_Objects = Total;
   end Reference_Model;
   procedure Run (Domain : AML_Frame_Handles.Invocation_Domain; Checks : out Natural) is
      S : State;
      Expected : Boolean;
      procedure Compare is
      begin
         Expected := Frozen_Valid (S);
         if Valid (S) /= Expected or else Reference_Model (S) /= Expected then
            raise Program_Error with "malformed-state equivalence";
         end if;
         Checks := Checks + 1;
      end Compare;
   begin
      if Object_Capacity /= 2 or else Root_Capacity /= 2 or else Generation_Limit /= 2 then
         raise Program_Error with "fixture requires exact 2/2/2 dimensions";
      end if;
      Checks := 0;
      for Authority in Boolean loop
       for Last in Generation loop
        for Cached in Object_Index loop
         for C1 in Owner_Count loop
          for C2 in Owner_Count loop
           for O1 in Object_Index loop
            for O2 in Object_Index loop
             for G1 in Generation loop
              for G2 in Generation loop
               S := (Domain => (if Authority then Domain else AML_Frame_Handles.No_Domain),
                     Last => Last, Live_Objects => Cached,
                     Values => [1 => (0, Ordinary, C1), 2 => (Number'Last, AML_Constant, C2)],
                     Leases => [1 => (O1, G1), 2 => (O2, G2)]);
               Compare;
               -- Payload/origin are unconstrained by validity, even absent.
               S.Values (1).Value := Number'Last;
               S.Values (1).Origin := AML_Constant;
               S.Values (2).Value := 0;
               S.Values (2).Origin := Ordinary;
               Compare;
              end loop;
             end loop;
            end loop;
           end loop;
          end loop;
         end loop;
        end loop;
       end loop;
      end loop;
   end Run;
end AML_Scalar_Ledger.Equivalence;
