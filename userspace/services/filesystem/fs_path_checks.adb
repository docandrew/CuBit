package body Fs_Path_Checks with SPARK_Mode is

   function Has_Traversal (Path : String) return Boolean is
   begin
      if Path'Length < 2 then
         return False;
      end if;
      for I in Path'First .. Path'Last - 1 loop
         if Parent_Component_At (Path, I) then
            return True;
         end if;
         pragma Loop_Invariant
           (for all J in Path'First .. I => not Parent_Component_At (Path, J));
      end loop;
      return False;
   end Has_Traversal;

   function Canonical (Path : String) return String is
      Result : String (1 .. Path'Length) := [others => ' '];
      Used : Natural := 0;
   begin
      for I in Path'Range loop
         if Path (I) /= Separator or else (Used > 0 and then Result (Used) /= Separator) then
            Used := Used + 1;
            Result (Used) := Path (I);
         end if;
         pragma Loop_Invariant (Used <= I - Path'First + 1);
         pragma Loop_Invariant
           (for all K in 1 .. Used =>
              (if Result (K) = Separator then
                 K > 1 and then Result (K - 1) /= Separator));
      end loop;
      if Used > 0 and then Result (Used) = Separator then
         Used := Used - 1;
      end if;
      return Result (1 .. Used);
   end Canonical;

end Fs_Path_Checks;
