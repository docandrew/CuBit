with AML_Decode;
generic
   Max_Result_Length : Positive;
package AML_Resource_Templates with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   type Structural_Profile is (Pinned_ACPICA);
   Profile : constant Structural_Profile := Pinned_ACPICA;
   -- Pinned ACPICA EndTag walk only: not full resource validity/authority.
   type Scan_Status is (Located, Invalid_Resource_Type, Bad_Resource_Length,
                        Buffer_Length, No_End_Tag);
   type Location (Status : Scan_Status := No_End_Tag) is record
      case Status is
         when Located => Prefix_Length : Natural := 0;
         when others => null;
      end case;
   end record;
   function Locate_End (Data : AML_Decode.Bytes) return Location
     with Global => null,
       Post => (if Locate_End'Result.Status = Located then
         Locate_End'Result.Prefix_Length <= Data'Length);
   type Build_Status is (Built, Invalid_Resource_Type, Bad_Resource_Length,
                         Buffer_Length, No_End_Tag, Output_Limit);
   subtype Result_Length is Natural range 0 .. Max_Result_Length;
   type Build_Result (Status : Build_Status := Output_Limit) is record
      case Status is
         when Built =>
            Length : Result_Length := 0;
            Data : AML_Decode.Bytes (1 .. Max_Result_Length) := [others => 0];
         when others => null;
      end case;
   end record;
   function Build (Left, Right : AML_Decode.Bytes) return Build_Result
     with Global => null,
       Post => (if Build'Result.Status = Built then
         Build'Result.Length >= 2 and then
         (for all I in Build'Result.Data'Range =>
           (if I > Build'Result.Length then Build'Result.Data (I) = 0)));
end AML_Resource_Templates;
