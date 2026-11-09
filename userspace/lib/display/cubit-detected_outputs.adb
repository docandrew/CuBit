pragma Ada_2022;
package body CuBit.Detected_Outputs with SPARK_Mode is
   procedure Replace
     (Current : in out OD.Catalog; Source : OD.Output_Source; Fresh : OD.Catalog;
      Changed : out Boolean)
   is
      Before : constant OD.Catalog := Current;
      Result : OD.Catalog;
   begin
      for I in 1 .. Before.Count loop
         pragma Loop_Invariant (Result.Count < I and then Well_Formed (Result));
         pragma Loop_Invariant
           (for all K in 1 .. Result.Count =>
              not (Result.Items (K).Source = Source and then Not_Lit (Result.Items (K))));
         if not (Before.Items (I).Source = Source and then Not_Lit (Before.Items (I))) then
            Result.Count := Result.Count + 1;
            Result.Items (Result.Count) := Before.Items (I);
         end if;
      end loop;
      for J in 1 .. Fresh.Count loop
         pragma Loop_Invariant (Well_Formed (Result));
         pragma Loop_Invariant
           (for all K in 1 .. Result.Count =>
              (if Result.Items (K).Source = Source and then Not_Lit (Result.Items (K))
               then (for some L in 1 .. Fresh.Count => Fresh.Items (L) = Result.Items (K))));
         exit when Result.Count = OD.Output_Count'Last;
         if Fresh.Items (J).Source = Source and then Not_Lit (Fresh.Items (J)) then
            Result.Count := Result.Count + 1;
            Result.Items (Result.Count) := Fresh.Items (J);
         end if;
      end loop;
      Current := Result;
      Changed := not Same (Current, Before);
   end Replace;

   function Find
     (Data : OD.Catalog; Source : OD.Output_Source; Native : OD.Native_Output_Number)
      return OD.Output_Count
   is
   begin
      for I in 1 .. Data.Count loop
         if Not_Lit (Data.Items (I)) and then Data.Items (I).Source = Source and then
           Data.Items (I).Native_Number = Native
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find;
end CuBit.Detected_Outputs;
