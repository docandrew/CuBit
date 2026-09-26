with CCL.Imports;
with CCL.Types.Correspondence;

package body CCL.Catalog with
   SPARK_Mode => On
is
   use type CCL.Host_Values.Import_Declaration;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.VM.Authority_Class;
   use type CCL.Imports.Transfer_Mode;
   use type CCL.Imports.Cancellation_Mode;
   use type CCL.Objects.Schema_Key;
   use type CCL.VM.Value_Kind;
   use type CCL.Resource_Policies.Description;
   use type CCL.Ownership.Ownership_Mode;
   use type CCL.Ownership.Disposition_Effect;
   use type CCL.Types.Import_Result;
   use type CCL.Types.Shape;
   use type CCL.Types.Unary_Resource_Result;
   use type CCL.Resource_Policies.Layout_Result;
   use type CCL.Ownership.Type_Table;

   function Empty_Catalog return Interface_Catalog is ((others => <>));

   function Valid_Name_Character
     (Item : Character; Interface_Name : Boolean) return Boolean
   is
     ((Item >= 'a' and then Item <= 'z') or else
      (Item >= '0' and then Item <= '9') or else
      Item = '-' or else (Interface_Name and then Item = '.'));

   procedure Make_Name
     (Text           : String;
      Interface_Name : Boolean;
      Item           : out Bounded_Name;
      Valid          : out Boolean)
   is
   begin
      Item := (others => <>);
      Valid := Text'Length in 1 .. MAX_NAME_LENGTH;
      if not Valid then
         return;
      end if;

      for Position in 0 .. Text'Length - 1 loop
         if not Valid_Name_Character
           (Text (Text'First + Position), Interface_Name)
         then
            Valid := False;
            return;
         end if;
         Item.Data (Position + 1) := Text (Text'First + Position);
      end loop;
      Item.Length := Text'Length;
   end Make_Name;

   function Names_Equal (Left, Right : Bounded_Name) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      elsif Left.Length = 0 then
         return True;
      else
         return Left.Data (1 .. Left.Length) =
           Right.Data (1 .. Right.Length);
      end if;
   end Names_Equal;

   function Digest_Present (Item : Descriptor_Digest) return Boolean is
   begin
      for Word of Item loop
         if Word /= 0 then
            return True;
         end if;
      end loop;
      return False;
   end Digest_Present;

   procedure Define_Interface
     (Name   : String;
      Major  : Unsigned_16;
      Minor  : Unsigned_16;
      Digest : Descriptor_Digest;
      Item   : out Interface_Descriptor;
      Error  : out Catalog_Error)
   is
      Valid : Boolean;
   begin
      Item := (others => <>);
      Make_Name (Name, True, Item.Name, Valid);
      if not Valid then
         Error := Invalid_Interface_Name;
      elsif Major = 0 then
         Error := Invalid_Interface_Version;
      elsif not Digest_Present (Digest) then
         Error := Missing_Descriptor_Digest;
      else
         Item.Major := Major;
         Item.Minor := Minor;
         Item.Digest := Digest;
         Item.Defined := True;
         Error := Catalog_Valid;
      end if;
   end Define_Interface;

   procedure Define_Operation
     (Name       : String;
      Parameters : Parameter_Count;
      Import     : CCL.VM.Import_Declaration;
      Item       : out Operation_Descriptor;
      Error      : out Catalog_Error)
   is
   begin
      if not CCL.VM.Scalar_Import (Import) then
         Item := (others => <>);
         Error := Invalid_Host_Contract;
         return;
      end if;
      Define_Host_Operation
        (Name, Parameters, CCL.Host_Values.From_Bytecode (Import), Item, Error);
   end Define_Operation;

   procedure Define_Host_Operation
     (Name : String; Parameters : Parameter_Count;
      Import : CCL.Host_Values.Import_Declaration;
      Item : out Operation_Descriptor; Error : out Catalog_Error)
   is
      Valid : Boolean;
   begin
      Item := (others => <>);
      Make_Name (Name, False, Item.Name, Valid);
      if not Valid then
         Error := Invalid_Operation_Name;
      elsif not CCL.Host_Values.Well_Formed (Import) then
         Error := Invalid_Host_Contract;
      elsif Import.Binding /= 0 or else Import.Local /= 0 then
         --  Runtime-local bindings and local-variable positions are assigned
         --  after compilation and are never descriptor identity.
         Error := Runtime_Binding_In_Descriptor;
      elsif Parameters = 0 and then
        (Import.Argument /= CCL.Host_Values.Integer_Value or else
         (not CCL.Host_Values.Has_Receiver (Import) and then
           (Import.Ownership_Argument or else
            Import.Transfer /= CCL.Imports.Copy_Argument)))
      then
         -- The zero-data sentinel cannot carry ownership. An explicit
         -- receiver owns its separate local, not this Integer zero.
         Error := Invalid_Zero_Parameter_Import;
      else
         Item.Parameters := Parameters;
         Item.Import := Import;
         Item.Defined := True;
         Error := Catalog_Valid;
      end if;
   end Define_Host_Operation;

   procedure Add_Operation
     (Item      : in out Interface_Descriptor;
      Operation : Operation_Descriptor;
      Error     : out Catalog_Error)
   is
   begin
      if not Item.Defined then
         Error := Invalid_Interface_Name;
      elsif not Operation.Defined then
         Error := Invalid_Operation_Name;
      elsif Item.Operations_Length > 0 then
         for Index in 0 .. Item.Operations_Length - 1 loop
            if Names_Equal (Item.Operations (Index).Name, Operation.Name) then
               Error := Duplicate_Operation;
               return;
            end if;
         end loop;
         if Item.Operations_Length = MAX_OPERATIONS then
            Error := Interface_Full;
         else
            Item.Operations (Item.Operations_Length) := Operation;
            Item.Operations_Length := Item.Operations_Length + 1;
            Error := Catalog_Valid;
         end if;
      elsif Item.Operations_Length = MAX_OPERATIONS then
         Error := Interface_Full;
      else
         Item.Operations (Item.Operations_Length) := Operation;
         Item.Operations_Length := Item.Operations_Length + 1;
         Error := Catalog_Valid;
      end if;
   end Add_Operation;

   procedure Initialize (Item : out Interface_Catalog) is
   begin
      Item := (others => <>);
   end Initialize;

   procedure Publish_Type
     (Item : in out Interface_Catalog; Source : CCL.Types.Registry;
      Root : CCL.Types.Type_Reference; Ref : out CCL.Types.Type_Reference;
      Result : out CCL.Types.Import_Result) is
   begin
      CCL.Objects.Catalog.Publish_Type (Item.Data_Types, Source, Root, Ref, Result);
   end Publish_Type;

   procedure Publish_Resource
     (Item : in out Interface_Catalog; Source : CCL.Types.Registry;
      Root : CCL.Types.Type_Reference; Policy : CCL.Resource_Policies.Description;
      Ref : out CCL.Types.Type_Reference; Result : out Resource_Publication)
   is
      Candidate : CCL.Objects.Catalog.Schema_Catalog := Item.Data_Types;
      Root_Ref, Target_Ref : CCL.Types.Type_Reference;
      Imported : CCL.Types.Import_Result;
   begin
      Ref := CCL.Types.Invalid_Type; Result := Invalid_Resource_Policy;
      if not CCL.Resource_Policies.Valid (Source, Root, Policy) then return; end if;
      CCL.Objects.Catalog.Publish_Type (Candidate, Source, Root, Root_Ref, Imported);
      if Imported /= CCL.Types.Imported then
         Result := (if Imported = CCL.Types.Import_Full then Resource_Catalog_Full else Resource_Definition_Conflict);
         return;
      end if;
      for I in 0 .. Policy.Count - 1 loop
         if Policy.Dispositions (I).Effect = CCL.Ownership.Transition then
            CCL.Objects.Catalog.Publish_Type
              (Candidate, Source, CCL.Types.Find (Source, Policy.Dispositions (I).Next_Type), Target_Ref, Imported);
            if Imported /= CCL.Types.Imported then
               Result := (if Imported = CCL.Types.Import_Full then Resource_Catalog_Full else Resource_Definition_Conflict);
               return;
            end if;
         end if;
      end loop;
      if Item.Resource_Policies (Root_Ref).Mode /= CCL.Ownership.Unrestricted then
         if Item.Resource_Policies (Root_Ref) /= Policy then Result := Resource_Policy_Conflict; return; end if;
         Ref := Root_Ref; Result := Resource_Already_Published; return;
      end if;
      Item.Data_Types := Candidate;
      Item.Resource_Policies (Root_Ref) := Policy;
      Ref := Root_Ref; Result := Resource_Published;
   end Publish_Resource;

   procedure Specialize_Unary_Resource
     (Item : in out Interface_Catalog; Source : CCL.Types.Registry;
      Family, Parameter_Label : String; Parameter : CCL.Types.Type_Reference;
      Policy : CCL.Resource_Policies.Description;
      Ref : out CCL.Types.Type_Reference;
      Result : out Resource_Specialization_Result)
   is
      Candidate_Source : CCL.Types.Registry := Source;
      Candidate : CCL.Types.Type_Reference;
      Specialized : CCL.Types.Unary_Resource_Result;
      Published : Resource_Publication;
   begin
      Ref := CCL.Types.Invalid_Type;
      CCL.Types.Specialize_Unary_Resource
        (Candidate_Source, CCL.Types.Named (Family),
         CCL.Types.Named (Parameter_Label), Parameter, Candidate, Specialized);
      if Specialized not in CCL.Types.Resource_Specialized |
        CCL.Types.Resource_Already_Specialized
      then
         Result := (if Specialized = CCL.Types.Resource_Registry_Full then
                      Specialization_Full else Specialization_Invalid);
         return;
      end if;
      Publish_Resource (Item, Candidate_Source, Candidate, Policy, Ref, Published);
      case Published is
         when Resource_Published => Result := Specialization_Ready;
         when Resource_Already_Published => Result := Specialization_Already_Ready;
         when Resource_Catalog_Full => Result := Specialization_Full;
         when Resource_Definition_Conflict | Resource_Policy_Conflict =>
            Result := Specialization_Conflict;
         when Invalid_Resource_Policy => Result := Specialization_Invalid;
      end case;
   end Specialize_Unary_Resource;

   function Resource_Policy
     (Item : Interface_Catalog; Ref : CCL.Types.Type_Reference)
      return CCL.Resource_Policies.Description is (Item.Resource_Policies (Ref));

   procedure Layout_Resources
     (Item : Interface_Catalog; Roots : CCL.Resource_Policies.Selection;
      Bindings : out CCL.Resource_Policies.Binding_Map;
      Definitions : out CCL.Ownership.Type_Table;
      Count : out CCL.Resource_Policies.Layout_Count;
      Result : out CCL.Resource_Policies.Layout_Result) is
   begin
      CCL.Resource_Policies.Layout (Visible_Types (Item), Item.Resource_Policies, Roots,
        Bindings, Definitions, Count, Result);
   end Layout_Resources;

   procedure Publish_Schema
     (Item : in out Interface_Catalog; Contract : CCL.Objects.Binding;
      Result : out CCL.Objects.Catalog.Publication_Result) is
   begin
      CCL.Objects.Catalog.Publish (Item.Data_Types, Contract, Result);
   end Publish_Schema;
   procedure Resolve_Schema
     (Item : Interface_Catalog; Key : CCL.Objects.Schema_Key;
      Contract : out CCL.Objects.Binding) is
   begin
      CCL.Objects.Catalog.Resolve (Item.Data_Types, Key, Contract);
   end Resolve_Schema;
   function Schema_Type (Item : Interface_Catalog; Key : CCL.Objects.Schema_Key)
     return CCL.Types.Type_Reference is
     (CCL.Objects.Catalog.Root_Of (Item.Data_Types, Key));

   procedure Publish
     (Item       : in out Interface_Catalog;
      Descriptor : Interface_Descriptor;
      Error      : out Catalog_Error)
   is
   begin
      if not Descriptor.Defined then
         Error := Invalid_Interface_Name;
      elsif Descriptor.Operations_Length = 0 then
         Error := Empty_Interface;
      elsif Item.Count > 0 then
         for Index in 0 .. Item.Count - 1 loop
            if Names_Equal
              (Item.Descriptors (Index).Name, Descriptor.Name)
            then
               Error := Duplicate_Interface;
               return;
            end if;
         end loop;
         if Item.Count = MAX_INTERFACES then
            Error := Catalog_Full;
         else
            Item.Descriptors (Item.Count) := Descriptor;
            Item.Count := Item.Count + 1;
            Error := Catalog_Valid;
         end if;
      elsif Item.Count = MAX_INTERFACES then
         Error := Catalog_Full;
      else
         Item.Descriptors (Item.Count) := Descriptor;
         Item.Count := Item.Count + 1;
         Error := Catalog_Valid;
      end if;
   end Publish;

   function Qualified_Name_Matches
     (Interface_Name : Bounded_Name;
      Operation_Name : Bounded_Name;
      Candidate      : String) return Boolean
   is
      Expected : constant Natural :=
        Interface_Name.Length + 1 + Operation_Name.Length;
   begin
      if Candidate'Length /= Expected then
         return False;
      end if;

      if Interface_Name.Length > 0 then
         for Position in 0 .. Interface_Name.Length - 1 loop
            if Candidate (Candidate'First + Position) /=
              Interface_Name.Data (Position + 1)
            then
               return False;
            end if;
         end loop;
      end if;

      if Candidate (Candidate'First + Interface_Name.Length) /= '.' then
         return False;
      end if;

      if Operation_Name.Length > 0 then
         for Position in 0 .. Operation_Name.Length - 1 loop
            if Candidate
              (Candidate'First + Interface_Name.Length + 1 + Position) /=
              Operation_Name.Data (Position + 1)
            then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Qualified_Name_Matches;

   procedure Resolve
     (Item           : Interface_Catalog;
      Qualified_Name : String;
      Result         : out Resolved_Operation;
      Found          : out Boolean)
   is
   begin
      Result := (others => <>);
      Found := False;
      if Item.Count = 0 then
         return;
      end if;

      for Interface_Index in 0 .. Item.Count - 1 loop
         if Item.Descriptors (Interface_Index).Operations_Length > 0 then
            for Operation_Index in
              0 .. Item.Descriptors (Interface_Index).Operations_Length - 1
            loop
               if Qualified_Name_Matches
                 (Item.Descriptors (Interface_Index).Name,
                  Item.Descriptors (Interface_Index).Operations
                    (Operation_Index).Name,
                  Qualified_Name)
               then
                  Result :=
                    (Interface_Digest =>
                       Item.Descriptors (Interface_Index).Digest,
                     Interface_Major =>
                       Item.Descriptors (Interface_Index).Major,
                     Interface_Minor =>
                       Item.Descriptors (Interface_Index).Minor,
                     Operation => Operation_Index,
                     Parameters =>
                       Item.Descriptors (Interface_Index).Operations
                         (Operation_Index).Parameters,
                     Import =>
                       Item.Descriptors (Interface_Index).Operations
                         (Operation_Index).Import);
                  Found := True;
                  return;
               end if;
            end loop;
         end if;
      end loop;
   end Resolve;

   function Same_Operation
     (Left, Right : Resolved_Operation) return Boolean
   is
     (Left.Interface_Digest = Right.Interface_Digest and then
      Left.Interface_Major = Right.Interface_Major and then
      Left.Interface_Minor = Right.Interface_Minor and then
      Left.Operation = Right.Operation and then
      Left.Parameters = Right.Parameters and then
      Left.Import = Right.Import);

   procedure Initialize (Item : out Linkage_Table) is
   begin
      Item := (others => <>);
   end Initialize;

   procedure Intern
     (Item      : in out Linkage_Table;
      Operation : Resolved_Operation;
      Index     : out CCL.VM.Import_Index;
      Result    : out Intern_Result;
      Local     : CCL.Ownership.Binding_Id := 0)
   is
   begin
      Index := 0;
      if Item.Count > 0 then
         for Position in 0 .. Item.Count - 1 loop
            if Same_Operation (Item.Entries (Position), Operation) and then
              Item.Locals (Position) = Local
            then
               Index := Position;
               Result := Linkage_Existing;
               return;
            end if;
         end loop;
      end if;

      if Item.Count = CCL.VM.MAX_IMPORTS then
         Result := Linkage_Full;
      else
         Index := Item.Count;
         Item.Entries (Item.Count) := Operation;
         Item.Locals (Item.Count) := Local;
         Item.Count := Item.Count + 1;
         Result := Linkage_Added;
      end if;
   end Intern;

   function Valid_Resolved_Operation
     (Item : Resolved_Operation) return Boolean
   is
     (Digest_Present (Item.Interface_Digest) and then
      Item.Interface_Major > 0 and then
      Item.Import.Binding = 0 and then
      Item.Import.Local = 0 and then CCL.Host_Values.Well_Formed (Item.Import));

   function Contracts_Match
     (Compiled : CCL.VM.Import_Declaration;
      Declared : CCL.Host_Values.Import_Declaration;
      Types : CCL.Types.Registry) return Boolean
   is
     (Compiled.Binding = 0 and then CCL.Host_Values.Matches_Bytecode (Compiled, Declared, Types));

   function Resource_Layout_Matches
     (Program : CCL.VM.Program; Catalog : Interface_Catalog) return Boolean
   is
      Roots : CCL.Resource_Policies.Selection := [others => False];
      Policies : CCL.Resource_Policies.Policy_Table := [others => (others => <>)];
      Bindings : CCL.Resource_Policies.Binding_Map;
      Definitions : CCL.Ownership.Type_Table;
      Count : CCL.Resource_Policies.Layout_Count;
      Result : CCL.Resource_Policies.Layout_Result;
      Expected : CCL.Types.Type_Reference;
      Visible : constant CCL.Types.Registry := Visible_Types (Catalog);
      Any_Resource : Boolean := False;
   begin
      for I in 0 .. Program.Locals_Length - 1 loop
         if Program.Local_Kinds (I) = CCL.VM.Resource_Value then
            Roots (Program.Local_Data_Types (I)) := True;
            Any_Resource := True;
         end if;
      end loop;
      for I in 0 .. Program.Imports_Length - 1 loop
         if Program.Imports (I).Argument = CCL.VM.Resource_Value then
            Roots (Program.Imports (I).Argument_Data_Type) := True;
            Any_Resource := True;
         end if;
         if Program.Imports (I).Result = CCL.VM.Resource_Value then
            Roots (Program.Imports (I).Result_Data_Type) := True;
            Any_Resource := True;
         end if;
         if CCL.VM.Has_Receiver (Program.Imports (I)) then
            Roots (Program.Imports (I).Receiver_Data_Type) := True;
            Any_Resource := True;
         end if;
      end loop;
      if not Any_Resource then return True; end if;
      -- Reconstruct policy in the program's nominal type order. Numeric type
      -- IDs in independently published catalog snapshots need not coincide.
      for Ref in CCL.Types.Type_Reference loop
         if CCL.Types.Known (Program.Data_Types, Ref) and then
           CCL.Types.Describe (Program.Data_Types, Ref).Form = CCL.Types.Resource
         then
            Expected := CCL.Types.Correspondence.Resolve (Program.Data_Types, Ref, Visible);
            if Expected /= CCL.Types.Invalid_Type then
               Policies (Ref) := Catalog.Resource_Policies (Expected);
            end if;
         end if;
      end loop;
      CCL.Resource_Policies.Layout
        (Program.Data_Types, Policies, Roots, Bindings, Definitions, Count, Result);
      if Result /= CCL.Resource_Policies.Ready or else
        Program.Types_Length /= Count or else Program.Types /= Definitions
      then return False; end if;
      for I in 0 .. Program.Locals_Length - 1 loop
         if Program.Local_Kinds (I) = CCL.VM.Resource_Value then
            if Program.Local_Types (I) /= Bindings (Program.Local_Data_Types (I)) then return False; end if;
         elsif Program.Local_Types (I) /= 0 then return False;
         end if;
      end loop;
      for I in 0 .. Program.Imports_Length - 1 loop
         if Program.Imports (I).Result = CCL.VM.Resource_Value and then
           Program.Imports (I).Result_Type_Tag /= Bindings (Program.Imports (I).Result_Data_Type)
         then return False; end if;
      end loop;
      return True;
   end Resource_Layout_Matches;

   procedure Initialize (Item : out Granted_Bindings) is
   begin
      Item := (others => <>);
   end Initialize;

   procedure Install
     (Item      : in out Granted_Bindings;
      Operation : Resolved_Operation;
      Binding   : Unsigned_32;
      Result    : out Grant_Result)
   is
   begin
      if not Valid_Resolved_Operation (Operation) then
         Result := Invalid_Grant_Operation;
         return;
      elsif Binding = 0 then
         Result := Invalid_Runtime_Binding;
         return;
      end if;

      if Item.Count > 0 then
         for Position in 0 .. Item.Count - 1 loop
            if Same_Operation (Item.Entries (Position).Operation, Operation)
            then
               Result :=
                 (if Item.Entries (Position).Binding = Binding then
                     Grant_Existing
                  else Conflicting_Runtime_Binding);
               return;
            end if;
         end loop;
      end if;

      if Item.Count = CCL.VM.MAX_IMPORTS then
         Result := Grant_Full;
      else
         Item.Entries (Item.Count) :=
           (Operation => Operation, Binding => Binding);
         Item.Count := Item.Count + 1;
         Result := Grant_Added;
      end if;
   end Install;

   procedure Find_Granted_Binding
     (Item      : Granted_Bindings;
      Operation : Resolved_Operation;
      Binding   : out Unsigned_32;
      Found     : out Boolean)
   is
   begin
      Binding := 0;
      Found := False;
      if Item.Count > 0 then
         for Position in 0 .. Item.Count - 1 loop
            if Same_Operation (Item.Entries (Position).Operation, Operation)
            then
               Binding := Item.Entries (Position).Binding;
               Found := True;
               return;
            end if;
         end loop;
      end if;
   end Find_Granted_Binding;

   procedure Link_Program
     (Grants  : Granted_Bindings;
      Linkage : Linkage_Table;
      Program : in out CCL.VM.Program;
      Result  : out Link_Result;
      Schemas : Interface_Catalog := Empty_Catalog)
   is
      type Runtime_Binding_Array is
        array (CCL.VM.Import_Index) of Unsigned_32;
      Resolved_Bindings : Runtime_Binding_Array := [others => 0];
      Binding : Unsigned_32;
      Found   : Boolean;
      function Schema_Matches
        (Kind : CCL.VM.Value_Kind; Local : CCL.Types.Type_Reference;
         Key : CCL.Objects.Schema_Key; Resource_Name : CCL.Types.Name) return Boolean
      is
         Ref : constant CCL.Types.Type_Reference :=
           (case Kind is when CCL.VM.Integer_Value => CCL.Types.Integer_Type,
             when CCL.VM.Boolean_Value => CCL.Types.Boolean_Type,
             when CCL.VM.Variant_Value | CCL.VM.Object_Value | CCL.VM.Resource_Value => Local);
         Expected : constant CCL.Types.Type_Reference :=
           (if Kind = CCL.VM.Resource_Value then CCL.Types.Find (Visible_Types (Schemas), Resource_Name)
            else Schema_Type (Schemas, Key));
      begin
         if Key = CCL.Objects.No_Schema and Kind /= CCL.VM.Resource_Value then
            return Kind in CCL.VM.Scalar_Kind;
         end if;
         return Expected /= CCL.Types.Invalid_Type and then
           CCL.Types.Correspondence.Resolve (Program.Data_Types, Ref, Visible_Types (Schemas)) = Expected;
      end Schema_Matches;
   begin
      if Program.Imports_Length /= Linkage.Count then
         Result := Linkage_Length_Mismatch;
         return;
      end if;
      if not Resource_Layout_Matches (Program, Schemas) then
         Result := Import_Contract_Mismatch;
         return;
      end if;

      --  Validate the complete linkage before mutating Program.
      if Linkage.Count > 0 then
         for Position in 0 .. Linkage.Count - 1 loop
            if not Contracts_Match
              (Program.Imports (Position),
               Linkage.Entries (Position).Import, Program.Data_Types) or else
              (CCL.Host_Values.Has_Resources (Linkage.Entries (Position).Import) and then
               Program.Imports (Position).Local /= Linkage.Locals (Position))
            then
               Result := Import_Contract_Mismatch;
               return;
            end if;
            Find_Granted_Binding
              (Grants, Linkage.Entries (Position), Binding, Found);
            if not Found then
               Result := Authority_Not_Granted;
               return;
            end if;
            if not Schema_Matches
              (Program.Imports (Position).Argument, Program.Imports (Position).Argument_Data_Type,
               Linkage.Entries (Position).Import.Argument_Schema,
               Linkage.Entries (Position).Import.Argument_Resource) or else
              not Schema_Matches
              (Program.Imports (Position).Result, Program.Imports (Position).Result_Data_Type,
               Linkage.Entries (Position).Import.Result_Schema,
               Linkage.Entries (Position).Import.Result_Resource) or else
              (CCL.Host_Values.Has_Receiver (Linkage.Entries (Position).Import) and then
               not Schema_Matches
                 (CCL.VM.Resource_Value, Program.Imports (Position).Receiver_Data_Type,
                  CCL.Objects.No_Schema, Linkage.Entries (Position).Import.Receiver_Resource))
            then
               Result := Import_Contract_Mismatch;
               return;
            end if;
            Resolved_Bindings (Position) := Binding;
         end loop;

         for Position in 0 .. Linkage.Count - 1 loop
            Program.Imports (Position).Binding :=
              Resolved_Bindings (Position);
         end loop;
      end if;
      Result := Link_Valid;
   end Link_Program;
end CCL.Catalog;
