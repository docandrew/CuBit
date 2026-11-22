pragma Ada_2022;
package body ACPI_Capacity_Growth with SPARK_Mode is
   Growth_Factor : constant Positive := 2;
   function Next_Capacity
     (Current, Demand : Count; Seed : Positive) return Count is
      Candidate : Count := Current;
   begin
      if Demand <= Current then return Current; end if;
      if Candidate = 0 then Candidate := Seed; end if;
      while Candidate < Demand loop
         pragma Loop_Invariant (Candidate > 0 and then Candidate >= Current);
         pragma Loop_Variant (Decreases => Count'Last - Candidate);
         if Candidate > Count'Last / Growth_Factor then
            Candidate := Count'Last;
         else
            Candidate := Candidate * Growth_Factor;
         end if;
      end loop;
      return Candidate;
   end Next_Capacity;
end ACPI_Capacity_Growth;
