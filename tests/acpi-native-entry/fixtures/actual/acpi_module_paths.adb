pragma Ada_2022;
with ACPI_Resource_Names;
with Firmware_Tables;
package body ACPI_Module_Paths with SPARK_Mode is
   use type Namespace.Lookup_Status;
   use type AML_Names.Parse_Status;
   function Parse (Text : String) return AML_Names.Name_Result is
   begin
      if Text'Length = 0 then return (Kind => AML_Names.Truncated); end if;
      if Text'Length > Max_Path_Bytes then
         return (Kind => AML_Names.Limit_Exceeded);
      end if;
      declare
         Data : Firmware_Tables.Bytes (1 .. Text'Length + 1) := [others => 0];
      begin
         for Offset in 0 .. Text'Length - 1 loop
            Data (Offset + 1) := Firmware_Tables.Byte (Character'Pos (Text (Text'First + Offset)));
         end loop;
         return ACPI_Resource_Names.Parse (Data);
      end;
   end Parse;
   generic
      type Context_Type (<>) is limited private;
      with function Context_Valid (Arena : Context_Type) return Boolean;
      with procedure Resolve (Arena : Context_Type; Scope : Values.Node_Descriptor;
         Path : AML_Names.Name_Result; Item : out Values.Node_Descriptor;
         Outcome : out Namespace.Lookup_Status; Status : out Values.Access_Status);
   procedure Select_In_Context (Arena : Context_Type; Caller : Values.Node_Descriptor;
      Root_Path, Parameter_Path : String; Result : out Selection;
      Access_Status : out Values.Access_Status)
      with Pre => Context_Valid (Arena) and then not Result'Constrained;
   procedure Select_In_Context (Arena : Context_Type; Caller : Values.Node_Descriptor;
      Root_Path, Parameter_Path : String; Result : out Selection;
      Access_Status : out Values.Access_Status)
   is
      Root, Parameter : Values.Node_Descriptor := Values.No_Node;
      Root_Name : AML_Names.Name_Result :=
        (AML_Names.Accepted, True, 0, 0, [others => "____"], 1);
      Parameter_Name : AML_Names.Name_Result;
      Lookup : Namespace.Lookup_Status;
      Caller_Root : Values.Node_Descriptor;
   begin
      Result := (Status => Caller_Rejected);
      -- Authenticate the caller even when both strings are empty or absolute.
      Resolve (Arena, Caller, Root_Name, Caller_Root, Lookup, Access_Status);
      if Access_Status /= Values.Available then return; end if;
      if Lookup /= Namespace.Found then
         Result := (Root_Lookup_Error, Lookup); return;
      end if;
      if Root_Path'Length = 0 then
         Root := Caller_Root;
      else
         Root_Name := Parse (Root_Path);
         if Root_Name.Kind /= AML_Names.Accepted then
            Result := (Root_Syntax_Error, Root_Name.Kind); return;
         end if;
         Resolve (Arena, Caller, Root_Name, Root, Lookup, Access_Status);
         if Access_Status /= Values.Available then return; end if;
         if Lookup /= Namespace.Found then
            Result := (Root_Lookup_Error, Lookup); return;
         end if;
      end if;
      if Values.Node_Kind (Root) not in Namespace.Scope_Object | Namespace.Device_Object |
        Namespace.Power_Resource_Object | Namespace.Processor_Object | Namespace.Thermal_Zone_Object
      then
         Access_Status := Values.Wrong_Kind;
         Result := (Status => Root_Type_Error); return;
      end if;
      if Parameter_Path'Length /= 0 then
         Parameter_Name := Parse (Parameter_Path);
         if Parameter_Name.Kind /= AML_Names.Accepted then
            Result := (Parameter_Syntax_Error, Parameter_Name.Kind); return;
         end if;
         Resolve (Arena,
           (if Parameter_Name.Rooted or else Parameter_Name.Parents > 0 then Caller else Root),
           Parameter_Name, Parameter, Lookup, Access_Status);
         if Access_Status /= Values.Available then return; end if;
         if Lookup /= Namespace.Found then
            Result := (Parameter_Lookup_Error, Lookup); return;
         end if;
      end if;
      Result := (Selected, Root, Parameter_Path'Length /= 0, Parameter);
   end Select_In_Context;
   procedure Select_Values is new Select_In_Context
     (Values.Arena, Values.Valid, Values.Resolve_Node);
   procedure Select_Service is new Select_In_Context
     (Core.State, Core.Valid, Core.Resolve_Node);
   procedure Select_Paths (Arena : Values.Arena; Caller : Values.Node_Descriptor;
      Root_Path, Parameter_Path : String; Result : out Selection;
      Access_Status : out Values.Access_Status) is
   begin
      Select_Values (Arena, Caller, Root_Path, Parameter_Path, Result, Access_Status);
   end Select_Paths;
   procedure Select_Paths (Service : Core.State; Caller : Values.Node_Descriptor;
      Root_Path, Parameter_Path : String; Result : out Selection;
      Access_Status : out Values.Access_Status) is
   begin
      Select_Service (Service, Caller, Root_Path, Parameter_Path, Result, Access_Status);
   end Select_Paths;
end ACPI_Module_Paths;
