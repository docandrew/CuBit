pragma Ada_2022;
-- Common address geometry only; a feasible witness is not an allocation or
-- ownership decision. Bridge, translation and platform policy remain separate.
package ACPI_Resource_Data.Address_Spaces.Geometry with SPARK_Mode, Pure is
   type Resource_Use is (Assigned_Use, Possible_Use);
   type Parameters is record
      Width : Address_Width;
      Granularity, Minimum, Maximum, Length : Address_Value;
      Minimum_Fixed, Maximum_Fixed : Boolean;
   end record;
   type Geometry_Status is (Feasible, Not_Decoded, Outside_Width,
      Reversed_Bounds, Invalid_Granularity, Invalid_Combination,
      Requires_Possible_Use, Invalid_Fixed_Granularity, Invalid_Fixed_Length,
      Invalid_Fixed_Alignment, Invalid_Length_Alignment, No_Feasible_Range);
   type Descriptor_Form is (Fixed_Range, Possible_Fixed_Size, Possible_Variable_Size);
   type Geometry_Result (Status : Geometry_Status:=Not_Decoded) is record
      case Status is
         when Feasible =>
            Form : Descriptor_Form;
            First, Last : Address_Value; -- Inclusive feasible witness.
         when others => null;
      end case;
   end record;
   function Width_Maximum (Width : Address_Width) return Address_Value
     with Global=>null;
   -- Implements the common field combinations of ACPI table 6.44 and an
   -- aligned, nonempty witness. No full-width length or successor is required.
   function Check (Value : Parameters; Purpose : Resource_Use)
      return Geometry_Result with Global=>null,
      Post=>(if Check'Result.Status=Feasible then
         Check'Result.First>=Value.Minimum
         and then Check'Result.Last<=Value.Maximum
         and then Check'Result.First<=Check'Result.Last
         and then Check'Result.Last<=Width_Maximum(Value.Width)
         and then (Check'Result.First and Value.Granularity)=0
         and then (Check'Result.Last and Value.Granularity)=Value.Granularity
         and then (if Value.Minimum_Fixed then Check'Result.First=Value.Minimum)
         and then (if Value.Maximum_Fixed then Check'Result.Last=Value.Maximum)
         and then (if Value.Length>0 then
            Check'Result.Last-Check'Result.First=Value.Length-1));
   -- Adapts copied decoder fields only. It does not validate source names,
   -- bridge role, translation arithmetic, resource type or platform ownership.
   function Assess (Descriptor : Decode_Result; Purpose : Resource_Use)
      return Geometry_Result with Global=>null;
end ACPI_Resource_Data.Address_Spaces.Geometry;
