pragma Ada_2022;
package body Firmware_Tables.Identifiers with SPARK_Mode is
   function Read_Identity (Data : Bytes) return Identity is
      Result : Identity := (others => [others => Character'Val (0)]);
   begin
      for I in Result.Name'Range loop
         Result.Name (I) := Character'Val (Data (Data'First + I - 1));
         pragma Loop_Invariant (for all J in 1 .. I =>
           Character'Pos (Result.Name (J)) = Data (Data'First + J - 1));
      end loop;
      for I in Result.OEM'Range loop
         Result.OEM (I) := Character'Val (Data (Data'First + 9 + I));
         pragma Loop_Invariant (for all J in 1 .. I =>
           Character'Pos (Result.OEM (J)) = Data (Data'First + 9 + J));
      end loop;
      for I in Result.OEM_Table'Range loop
         Result.OEM_Table (I) := Character'Val (Data (Data'First + 15 + I));
         pragma Loop_Invariant (for all J in 1 .. I =>
           Character'Pos (Result.OEM_Table (J)) = Data (Data'First + 15 + J));
      end loop;
      return Result;
   end Read_Identity;
end Firmware_Tables.Identifiers;
