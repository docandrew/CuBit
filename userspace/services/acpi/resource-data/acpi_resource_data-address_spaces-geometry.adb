pragma Ada_2022;
package body ACPI_Resource_Data.Address_Spaces.Geometry with SPARK_Mode is
   function Width_Maximum (Width : Address_Width) return Address_Value is
     (case Width is
         when Word_Width=>Address_Value(Interfaces.Unsigned_16'Last),
         when DWord_Width=>Address_Value(Interfaces.Unsigned_32'Last),
         when QWord_Width=>Address_Value'Last);
   function Check (Value : Parameters; Purpose : Resource_Use)
      return Geometry_Result is
      Limit : constant Address_Value:=Width_Maximum(Value.Width);
      Gran : constant Address_Value:=Value.Granularity;
      First : Address_Value:=Value.Minimum;
      Last : Address_Value;
      Form : Descriptor_Form;
   begin
      if Gran>Limit or else Value.Minimum>Limit or else Value.Maximum>Limit
        or else Value.Length>Limit then return(Status=>Outside_Width); end if;
      if Value.Minimum>Value.Maximum then return(Status=>Reversed_Bounds); end if;
      if Gran/=Address_Value'Last and then (Gran and (Gran+1))/=0 then
         return(Status=>Invalid_Granularity);
      end if;
      if Value.Length=0 then
         if Value.Minimum_Fixed and then Value.Maximum_Fixed then
            return(Status=>Invalid_Combination);
         end if;
         Form:=Possible_Variable_Size;
      elsif Value.Minimum_Fixed and then Value.Maximum_Fixed then
         if Gran/=0 then return(Status=>Invalid_Fixed_Granularity); end if;
         -- Difference form avoids both span+1 and full-width length overflow.
         if Value.Length-1/=Value.Maximum-Value.Minimum then
            return(Status=>Invalid_Fixed_Length);
         end if;
         return(Status=>Feasible,Form=>Fixed_Range,First=>Value.Minimum,Last=>Value.Maximum);
      elsif Value.Minimum_Fixed or else Value.Maximum_Fixed then
         return(Status=>Invalid_Combination);
      else
         if (Value.Length and Gran)/=0 then return(Status=>Invalid_Length_Alignment); end if;
         Form:=Possible_Fixed_Size;
      end if;
      if Purpose/=Possible_Use then return(Status=>Requires_Possible_Use); end if;
      if (Value.Minimum_Fixed and then (Value.Minimum and Gran)/=0)
        or else (Value.Maximum_Fixed and then (Value.Maximum and Gran)/=Gran) then
         return(Status=>Invalid_Fixed_Alignment);
      end if;
      declare Low : constant Address_Value:=First and Gran; begin
         if Low/=0 then
            -- Low>0 ensures the increment is representable even for all-ones
            -- granularity. Check room before adding it to the minimum.
            declare Padding : constant Address_Value:=Gran-Low+1; begin
               if Padding>Value.Maximum-First then return(Status=>No_Feasible_Range); end if;
               First:=First+Padding;
            end;
         end if;
      end;
      if Value.Length>0 then
         if Value.Length-1>Value.Maximum-First then return(Status=>No_Feasible_Range); end if;
         Last:=First+(Value.Length-1);
      elsif Value.Maximum_Fixed then
         Last:=Value.Maximum;
      else
         -- The first full aligned granule; this includes the full 64-bit range
         -- without materializing its unrepresentable length.
         Last:=First or Gran;
         if Last>Value.Maximum then return(Status=>No_Feasible_Range); end if;
      end if;
      return(Status=>Feasible,Form=>Form,First=>First,Last=>Last);
   end Check;
   function Assess (Descriptor : Decode_Result; Purpose : Resource_Use)
      return Geometry_Result is
   begin
      if Descriptor.Status/=Decoded then return(Status=>Not_Decoded); end if;
      return Check((Width=>Descriptor.Width,Granularity=>Descriptor.Granularity,
        Minimum=>Descriptor.Minimum,Maximum=>Descriptor.Maximum,Length=>Descriptor.Length,
        Minimum_Fixed=>Descriptor.Minimum_Fixed,Maximum_Fixed=>Descriptor.Maximum_Fixed),Purpose);
   end Assess;
end ACPI_Resource_Data.Address_Spaces.Geometry;
