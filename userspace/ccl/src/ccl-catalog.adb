with CCL.Imports;

package body CCL.Catalog with
   SPARK_Mode => On
is
   use type CCL.VM.Import_Declaration;
   use type CCL.VM.Value_Kind;
   use type CCL.VM.Authority_Class;
   use type CCL.Imports.Transfer_Mode;
   use type CCL.Imports.Cancellation_Mode;

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
      Valid : Boolean;
   begin
      Item := (others => <>);
      Make_Name (Name, False, Item.Name, Valid);
      if not Valid then
         Error := Invalid_Operation_Name;
      elsif Import.Binding /= 0 or else Import.Local /= 0 then
         --  Runtime-local bindings and local-variable positions are assigned
         --  after compilation and are never descriptor identity.
         Error := Runtime_Binding_In_Descriptor;
      elsif Parameters = 0 and then
        (Import.Argument /= CCL.VM.Integer_Value or else
         Import.Ownership_Argument or else
         Import.Transfer /= CCL.Imports.Copy_Argument)
      then
         --  The v2 zero-parameter sentinel cannot carry ownership.
         Error := Invalid_Zero_Parameter_Import;
      else
         Item.Parameters := Parameters;
         Item.Import := Import;
         Item.Defined := True;
         Error := Catalog_Valid;
      end if;
   end Define_Operation;

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
      Result    : out Intern_Result)
   is
   begin
      Index := 0;
      if Item.Count > 0 then
         for Position in 0 .. Item.Count - 1 loop
            if Same_Operation (Item.Entries (Position), Operation) then
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
      Item.Import.Local = 0);

   function Contracts_Match
     (Compiled, Declared : CCL.VM.Import_Declaration) return Boolean
   is
     (Compiled.Binding = 0 and then
      Compiled.Argument = Declared.Argument and then
      Compiled.Result = Declared.Result and then
      Compiled.Authority = Declared.Authority and then
      Compiled.Ownership_Argument = Declared.Ownership_Argument and then
      Compiled.Transfer = Declared.Transfer and then
      Compiled.Cancellation = Declared.Cancellation and then
      Compiled.Success_Verb = Declared.Success_Verb and then
      Compiled.Failure_Verb = Declared.Failure_Verb and then
      Compiled.Cancel_Verb = Declared.Cancel_Verb);

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
      Result  : out Link_Result)
   is
      type Runtime_Binding_Array is
        array (CCL.VM.Import_Index) of Unsigned_32;
      Resolved_Bindings : Runtime_Binding_Array := [others => 0];
      Binding : Unsigned_32;
      Found   : Boolean;
   begin
      if Program.Imports_Length /= Linkage.Count then
         Result := Linkage_Length_Mismatch;
         return;
      end if;

      --  Validate the complete linkage before mutating Program.
      if Linkage.Count > 0 then
         for Position in 0 .. Linkage.Count - 1 loop
            if not Contracts_Match
              (Program.Imports (Position),
               Linkage.Entries (Position).Import)
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
