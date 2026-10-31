pragma Ada_2022;
with ACPI_Service_Core;
with AML_Names;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Module_Paths with SPARK_Mode is
   package Values renames Core.Values;
   package Namespace renames Core.Namespace;
   use type Values.Access_Status, Values.Node_Descriptor;
   type Selection_Status is (Selected, Caller_Rejected, Root_Syntax_Error,
      Root_Lookup_Error, Root_Type_Error, Parameter_Syntax_Error,
      Parameter_Lookup_Error);
   type Selection (Status : Selection_Status := Caller_Rejected) is record
      case Status is
         when Selected =>
            Root : Values.Node_Descriptor;
            Has_Parameter : Boolean;
            Parameter : Values.Node_Descriptor;
         when Root_Syntax_Error | Parameter_Syntax_Error =>
            Syntax : AML_Names.Parse_Status;
         when Root_Lookup_Error | Parameter_Lookup_Error =>
            Lookup : Namespace.Lookup_Status;
         when Caller_Rejected | Root_Type_Error => null;
      end case;
   end record;
   -- Bounded external path grammar: 255 prefixes and 255 four-byte segments,
   -- separated by dots. The resource-name parser owns byte validation.
   Prefix_Limit : constant Positive := 255;
   Max_Path_Bytes : constant Positive := Prefix_Limit +
     AML_Names.Segment_Array'Length * AML_Names.Segment'Length +
     (AML_Names.Segment_Array'Length - 1);
   -- Read-only selection of existing namespace objects. Empty Root_Path means
   -- namespace root; empty Parameter_Path means no parameter store. Absolute
   -- and parent-prefixed parameter paths use Caller, all others use Root.
   -- Results are owner-specific snapshots and must be revalidated before use.
   -- This does not load a table, write parameter data or execute AML.
   procedure Select_Paths (Arena : Values.Arena; Caller : Values.Node_Descriptor;
      Root_Path, Parameter_Path : String; Result : out Selection;
      Access_Status : out Values.Access_Status)
     with Pre => Values.Valid (Arena) and then not Result'Constrained,
       Post => (if Result.Status = Selected then
          Access_Status = Values.Available and then Values.Current_Node (Arena, Result.Root)
          and then Result.Has_Parameter = (Parameter_Path'Length /= 0)
          and then (if Result.Has_Parameter then Values.Current_Node (Arena, Result.Parameter)
            else Result.Parameter = Values.No_Node));
   -- Same parser/resolver policy over the owning service, without exporting its arena.
   procedure Select_Paths (Service : Core.State; Caller : Values.Node_Descriptor;
      Root_Path, Parameter_Path : String; Result : out Selection;
      Access_Status : out Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained,
       Post => (if Result.Status = Selected then
          Access_Status = Values.Available and then Core.Current_Node (Service, Result.Root)
          and then Result.Has_Parameter = (Parameter_Path'Length /= 0)
          and then (if Result.Has_Parameter then Core.Current_Node (Service, Result.Parameter)
            else Result.Parameter = Values.No_Node));
end ACPI_Module_Paths;
