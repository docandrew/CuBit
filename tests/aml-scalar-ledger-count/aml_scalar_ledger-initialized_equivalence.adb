package body AML_Scalar_Ledger.Initialized_Equivalence is
   function Frozen_Initialized
     (Store : State; Domain : AML_Frame_Handles.Invocation_Domain) return Boolean
   is
   begin
      if Store.Domain /= Domain or else Store.Last /= 0 or else Store.Live_Objects /= 0 then return False; end if;
      for Item of Store.Values loop
         if Item /= Scalar'(others => <>) then return False; end if;
      end loop;
      for Item of Store.Leases loop
         if Item /= Root_Entry'(others => <>) then return False; end if;
      end loop;
      return True;
   end Frozen_Initialized;
   function Candidate_Initialized
     (Store : State; Domain : AML_Frame_Handles.Invocation_Domain) return Boolean is
     (Store.Domain = Domain and then Store.Last = 0 and then Store.Live_Objects = 0
      and then (for all O in Object_Slot => Store.Values (O) = Scalar'(others => <>))
      and then (for all R in Root_Slot => Store.Leases (R) = Root_Entry'(others => <>)));
   procedure Run (Domain : AML_Frame_Handles.Invocation_Domain; Checks : out Natural) is
      S : State;
      Expected : Boolean;
      procedure Compare is
      begin
         Expected := Frozen_Initialized (S, Domain);
         if Candidate_Initialized (S, Domain) /= Expected then
            raise Program_Error with "initialized-state equivalence";
         end if;
         pragma Assert (Initialized (S, Domain) = Expected);
         Checks := Checks + 1;
      end Compare;
   begin
      if Object_Capacity /= 2 or else Root_Capacity /= 2 or else Generation_Limit /= 2 then
         raise Program_Error with "fixture requires exact 2/2/2 dimensions";
      end if;
      Checks := 0;
      -- Exact initialized success, wrong authority, and every default field.
      S := Empty (Domain); Compare;
      S := Empty (AML_Frame_Handles.No_Domain); Compare;
      for O in Object_Slot loop
         S := Empty (Domain); S.Values (O).Value := Number'Last; Compare;
         S := Empty (Domain); S.Values (O).Origin := AML_Constant; Compare;
         S := Empty (Domain); S.Values (O).Count := Owner_Count'Last; Compare;
      end loop;
      for R in Root_Slot loop
         S := Empty (Domain); S.Leases (R).Object := Object_Slot'Last; Compare;
         S := Empty (Domain); S.Leases (R).Serial := Generation'Last; Compare;
      end loop;
      S := Empty (Domain); S.Last := Generation'Last; Compare;
      S := Empty (Domain); S.Live_Objects := Object_Index'Last; Compare;
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
end AML_Scalar_Ledger.Initialized_Equivalence;
