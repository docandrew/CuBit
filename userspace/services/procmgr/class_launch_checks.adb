pragma Ada_2022;
with CCL.Typed_Values;

package body Class_Launch_Checks is
   package CP renames Child_Parameters;
   use type CP.Schema_Status, CP.Read_Status, CP.Merge_Status;

   procedure Check
     (Signature, Root, Launch : String;
      Facts : CS.Fact_Array; Fact_Total : CS.Fact_Count;
      Item : out Result)
   is
      procedure Say (Status : Outcome; Text : String) is
      begin
         Item.Status := Status;
         Item.Length := 0;
         Item.Message_Length := Natural'Min (Text'Length, Maximum_Message);
         Item.Message (1 .. Item.Message_Length) := Text (Text'First .. Text'First + Item.Message_Length - 1);
      end Say;
      Schema : String (1 .. CP.Maximum_Schema_Bytes);
      Schema_Length : Natural;
      Built : CP.Schema_Status;
      Checked : CCL.Typed_Values.Check_Result;
   begin
      Item := (others => <>);
      CP.Policy_Schema (Signature, Root, Facts, Fact_Total, Schema, Schema_Length, Built);
      if Built /= CP.Built then
         Say (Schema_Refused, CP.Schema_Status_Name (Built));
         return;
      end if;
      CCL.Typed_Values.Check (Schema (1 .. Schema_Length), CP.Launch_Type, Launch, Checked);
      if not Checked.Success then
         Say (Not_Typed, Checked.Message (1 .. Checked.Message_Length));
         return;
      end if;
      declare
         Canonical : String renames Checked.Canonical (1 .. Checked.Length);
         First, Last : Natural;
         Read : CP.Read_Status;
         Declared : CP.Declared_Array := [others => (others => <>)];
         Merge : CP.Merge_Status;
      begin
         CP.Read_Launch (Canonical, Facts, Fact_Total, First, Last, Read);
         if Read /= CP.Read then
            Say ((if Read = CP.Facts_Differ then Facts_Differ else Not_Typed), CP.Read_Status_Name (Read));
            return;
         end if;
         if Last - First + 1 > CP.Maximum_Merged_Bytes then
            Say (Too_Long, CP.Status_Name (CP.Too_Long));
            return;
         end if;
         --  Read_Launch found exactly the supervisor's facts in the value:
         --  they are the declared ones.
         for I in 1 .. Fact_Total loop
            Declared (I) := Facts (I).Field;
         end loop;
         CP.Merge (Canonical (First .. Last), Declared, Fact_Total, Facts, Fact_Total,
                   Item.Merged_Value, Item.Length, Merge);
         case Merge is
            when CP.Merged => Item.Status := Merged;
            when CP.Too_Long => Say (Too_Long, CP.Status_Name (Merge));
            when CP.Fact_Not_Declared | CP.Fact_Missing | CP.Declaration_Invalid =>
               Say (Facts_Differ, CP.Status_Name (Merge));
            when CP.Policy_Not_Record => Say (Not_Typed, CP.Status_Name (Merge));
         end case;
      end;
   end Check;
end Class_Launch_Checks;
