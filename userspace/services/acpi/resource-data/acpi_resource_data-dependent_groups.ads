pragma Ada_2022;
-- Descriptive layout of one copied resource template. Priorities and source
-- order are retained; no alternative is selected or admitted for hardware use.
package ACPI_Resource_Data.Dependent_Groups with SPARK_Mode, Pure is
   type Priority is (Good, Acceptable, Suboptimal);
   type Alternative is record
      Marker, First, Limit : Byte_Offset:=0; -- Half-open payload span [First,Limit).
      Items : Item_Count:=0;
      Information_Present : Boolean:=False;
      Information : Firmware_Tables.Byte:=0;
      Compatibility, Performance : Priority:=Acceptable;
   end record;
   type Alternative_Array is array(Positive range <>) of Alternative;
   function Valid_Span (Item : Alternative; Extent : Byte_Offset) return Boolean is
     (Item.Marker < Item.First and then Item.First <= Item.Limit
      and then Item.Limit <= Extent
      and then Item.Items <= Item_Count (Item.Limit - Item.First));
   type Layout is record
      Alternatives : Item_Count:=0;
      Common_Before, Common_After : Item_Count:=0;
      Group_First, Group_Limit, End_Tag : Byte_Offset:=0;
   end record;
   type Analysis_Status is (Grouped, Framing_Error, Bad_Dependent_Length,
      Reserved_Priority_Bits, Reserved_Priority, End_Without_Start,
      Repeated_End, Start_After_End, Missing_End, Alternative_Capacity);
   type Analysis_Result (Status : Analysis_Status:=Framing_Error) is record
      Position : Byte_Offset:=0;
      case Status is
         when Grouped => Value : Layout;
         when Framing_Error => Cause : Template_Status:=Missing_End_Tag;
         when others => null;
      end case;
   end record;
   -- Caller-sized output, no hidden fixed alternative limit or allocation.
   -- Full framing/checksum/quota are validated before analyzing the grammar.
   -- Errors leave the entire output defaulted; unused entries also default.
   -- Empty alternatives are retained, not implicitly rejected or selected.
   -- Offsets refer only to this exact copied Data; item payload semantics and
   -- cross-template/per-device policy are outside this structural analysis.
   procedure Analyze (Data : Firmware_Tables.Bytes; Alternatives : out Alternative_Array;
      Result : out Analysis_Result; Maximum_Items : Item_Count:=Default_Item_Limit)
      with Global=>null, Pre => not Result'Constrained,
      Post => Result.Position <= Byte_Offset (Data'Length)
        and then (if Result.Status /= Grouped then
          (for all Slot in Alternatives'Range => Alternatives (Slot) = Alternative'(others => <>))
        else
          Result.Position = Result.Value.End_Tag
          and then Result.Value.Alternatives <= Item_Count (Alternatives'Length)
          and then Result.Value.Group_First <= Result.Value.Group_Limit
          and then Result.Value.Group_Limit <= Result.Value.End_Tag
          and then (for all Slot in Alternatives'Range =>
            (if Item_Count (Slot - Alternatives'First) < Result.Value.Alternatives then
               Valid_Span (Alternatives (Slot), Result.Value.End_Tag)
             else Alternatives (Slot) = Alternative'(others => <>))));
end ACPI_Resource_Data.Dependent_Groups;
