with CCL.Objects.Catalog;
with CCL.Host_Values;
with CCL.Ownership;
with CCL.Imports;
with CCL.Resource_Policies;
with Config_Object_Outcomes;

package body Config_Object_Interfaces with SPARK_Mode is
   use CCL.Catalog;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Publish
     (Catalog : in out Interface_Catalog;
      Alias : String; Interface_Key : Descriptor_Digest;
      Contract : CCL.Objects.Binding;
      Read_Description : Config_Read_Outcomes.Description;
      Resource_Kind : out CCL.Types.Type_Reference;
      Accepted : out Boolean)
   is
      Candidate : Interface_Catalog := Catalog;
      Descriptor : Interface_Descriptor;
      Declared : Operation_Descriptor;
      Signature : CCL.Host_Values.Import_Declaration;
      Error : Catalog_Error;
      Published : CCL.Objects.Catalog.Publication_Result;
      Specialized : Resource_Specialization_Result;
      Kind : CCL.Types.Type_Reference;
      Resource_Name : CCL.Types.Name;
      Policy : CCL.Resource_Policies.Description :=
        (Mode => CCL.Ownership.Must_Handle, Count => 1, others => <>);
   begin
      Accepted := False;
      Resource_Kind := CCL.Types.Invalid_Type;
      if not CCL.Objects.Is_Bound (Contract) or else
        not Config_Read_Outcomes.Matches (Read_Description, Contract)
      then return; end if;
      Publish_Schema (Candidate, Contract, Published);
      if Published not in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published then return; end if;
      Publish_Schema (Candidate, Config_Read_Outcomes.Schema (Read_Description), Published);
      if Published not in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published then return; end if;
      Publish_Schema (Candidate, Config_Object_Outcomes.Schema, Published);
      if Published not in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published then return; end if;
      Policy.Dispositions (0) := (Verb => 1, Effect => CCL.Ownership.Consume, others => <>);
      Specialize_Unary_Resource
        (Candidate, Visible_Types (Candidate), "ConfigCollection", "Value",
         Schema_Type (Candidate, CCL.Objects.Identity (Contract)), Policy, Kind, Specialized);
      if Specialized not in Specialization_Ready | Specialization_Already_Ready then return; end if;
      Resource_Name := CCL.Types.Describe (Visible_Types (Candidate), Kind).Identifier;
      Define_Interface (Alias, 1, 0, Interface_Key, Descriptor, Error);
      if Error /= Catalog_Valid then return; end if;
      for Action in Operation loop
         Signature := (others => <>);
         case Action is
            when Open_Collection =>
               Signature.Result := CCL.Host_Values.Resource_Value;
               Signature.Result_Resource := Resource_Name;
            when Read_Value | Write_Value =>
               Signature.Receiver_Resource := Resource_Name;
               Signature.Ownership_Argument := True;
               Signature.Transfer :=
                 (if Action = Read_Value then CCL.Imports.Borrowed_RO_Argument
                  else CCL.Imports.Borrowed_RW_Argument);
               Signature.Result := CCL.Host_Values.Object_Value;
               Signature.Result_Schema :=
                 (if Action = Read_Value then CCL.Objects.Identity (Config_Read_Outcomes.Schema (Read_Description))
                  else Config_Object_Outcomes.Key);
               if Action = Write_Value then
                  Signature.Argument := CCL.Host_Values.Object_Value;
                  Signature.Argument_Schema := CCL.Objects.Identity (Contract);
               end if;
            when Close_Collection =>
               Signature.Argument := CCL.Host_Values.Resource_Value;
               Signature.Argument_Resource := Resource_Name;
               Signature.Ownership_Argument := True;
               Signature.Transfer := CCL.Imports.Move_Argument;
               Signature.Success_Verb := 1;
               Signature.Failure_Verb := 1;
         end case;
         Define_Host_Operation
           (Name (Action), (if Action in Write_Value | Close_Collection then 1 else 0),
            Signature, Declared, Error);
         if Error /= Catalog_Valid then return; end if;
         Add_Operation (Descriptor, Declared, Error);
         if Error /= Catalog_Valid then return; end if;
      end loop;
      CCL.Catalog.Publish (Candidate, Descriptor, Error);
      if Error /= Catalog_Valid then return; end if;
      Catalog := Candidate;
      Resource_Kind := Kind;
      Accepted := True;
   end Publish;
end Config_Object_Interfaces;
