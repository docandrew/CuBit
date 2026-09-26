with Interfaces;
with CCL.VM;
with CCL.Host_Values;
with CCL.Types;
with CCL.Objects.Catalog;
with CCL.Resource_Policies;
with CCL.Ownership;

package CCL.Catalog with
   SPARK_Mode => On
is
   use Interfaces;
   use type CCL.Types.Type_Reference;

   MAX_NAME_LENGTH : constant := 48;
   MAX_INTERFACES  : constant := 16;
   MAX_OPERATIONS  : constant := 16;

   subtype Parameter_Count is Natural range 0 .. 1;
   -- Ordinary data arguments, excluding an optional owned receiver. A source
   -- receiver call spells the receiver first, then its data argument if any.
   subtype Interface_Count is Natural range 0 .. MAX_INTERFACES;
   subtype Operation_Count is Natural range 0 .. MAX_OPERATIONS;
   subtype Operation_Index is Natural range 0 .. MAX_OPERATIONS - 1;

   type Descriptor_Digest is array (Natural range 0 .. 3) of Unsigned_64;
   EMPTY_DIGEST : constant Descriptor_Digest := [others => 0];

   type Catalog_Error is
     (Catalog_Valid,
      Invalid_Interface_Name,
      Invalid_Operation_Name,
      Invalid_Interface_Version,
      Missing_Descriptor_Digest,
      Empty_Interface,
      Duplicate_Interface,
      Duplicate_Operation,
      Interface_Full,
      Catalog_Full,
      Runtime_Binding_In_Descriptor,
      Invalid_Zero_Parameter_Import,
      Invalid_Host_Contract);

   type Operation_Descriptor is private;
   type Interface_Descriptor is private;

   --  A descriptor is an immutable value once published into a catalog.  The
   --  digest identifies the complete external interface definition; the
   --  operation number is stable only within that pinned definition.
   procedure Define_Interface
     (Name   : String;
      Major  : Unsigned_16;
      Minor  : Unsigned_16;
      Digest : Descriptor_Digest;
      Item   : out Interface_Descriptor;
      Error  : out Catalog_Error);

   --  CCLB v2 host imports carry one scalar argument.  A zero-parameter
   --  source operation is lowered to the canonical Integer zero sentinel.
   --  That compatibility detail disappears when the VM gains Unit values.
   procedure Define_Operation
     (Name       : String;
      Parameters : Parameter_Count;
      Import     : CCL.VM.Import_Declaration;
      Item       : out Operation_Descriptor;
      Error      : out Catalog_Error);

   procedure Add_Operation
     (Item      : in out Interface_Descriptor;
      Operation : Operation_Descriptor;
      Error     : out Catalog_Error);

   procedure Define_Host_Operation
     (Name : String; Parameters : Parameter_Count;
      Import : CCL.Host_Values.Import_Declaration;
      Item : out Operation_Descriptor; Error : out Catalog_Error);

   type Resolved_Operation is record
      Interface_Digest : Descriptor_Digest := EMPTY_DIGEST;
      Interface_Major  : Unsigned_16 := 0;
      Interface_Minor  : Unsigned_16 := 0;
      Operation        : Operation_Index := 0;
      Parameters       : Parameter_Count := 0;
      Import           : CCL.Host_Values.Import_Declaration := (others => <>);
   end record;

   type Interface_Catalog is private;

   procedure Initialize (Item : out Interface_Catalog);
   function Empty_Catalog return Interface_Catalog;

   --  A trusted host publishes only descriptions the caller may discover.
   --  This does not grant an operation, authenticate a publisher or install a
   --  runtime binding. A name conflict rejects the complete import atomically.
   procedure Publish_Type
     (Item : in out Interface_Catalog; Source : CCL.Types.Registry;
      Root : CCL.Types.Type_Reference; Ref : out CCL.Types.Type_Reference;
      Result : out CCL.Types.Import_Result);
   function Visible_Types (Item : Interface_Catalog) return CCL.Types.Registry;
   type Resource_Publication is
     (Resource_Published, Resource_Already_Published, Invalid_Resource_Policy,
      Resource_Definition_Conflict, Resource_Policy_Conflict, Resource_Catalog_Full);
   procedure Publish_Resource
     (Item : in out Interface_Catalog; Source : CCL.Types.Registry;
      Root : CCL.Types.Type_Reference; Policy : CCL.Resource_Policies.Description;
      Ref : out CCL.Types.Type_Reference; Result : out Resource_Publication)
     with Global => null,
       Post => (if Result not in Resource_Published | Resource_Already_Published then
         Item = Item'Old and Ref = CCL.Types.Invalid_Type);
   -- Atomic trusted publication of nominal type + ownership/disposition rules.
   -- Transition target definitions are imported too; their own policies may be
   -- published afterward (including cycles). Layout rejects incomplete policy
   -- closures. Metadata publication grants no operation or endpoint authority.
   type Resource_Specialization_Result is
     (Specialization_Ready,
      Specialization_Already_Ready,
      Specialization_Invalid,
      Specialization_Conflict,
      Specialization_Full);
   --  Trusted descriptor construction for unary resource families such as
   --  ConfigCollection<T> and Stream<T>. Parameter is resolved in Source; the
   --  nominal resource and its non-unrestricted ownership policy are published
   --  atomically. This creates no operation, endpoint, grant, handle, or
   --  stream transport.
   procedure Specialize_Unary_Resource
     (Item : in out Interface_Catalog; Source : CCL.Types.Registry;
      Family, Parameter_Label : String; Parameter : CCL.Types.Type_Reference;
      Policy : CCL.Resource_Policies.Description;
      Ref : out CCL.Types.Type_Reference;
      Result : out Resource_Specialization_Result)
   with Global => null,
     Post => (if Result not in Specialization_Ready | Specialization_Already_Ready then
       Item = Item'Old and Ref = CCL.Types.Invalid_Type);
   function Resource_Policy
     (Item : Interface_Catalog; Ref : CCL.Types.Type_Reference)
      return CCL.Resource_Policies.Description;
   procedure Layout_Resources
     (Item : Interface_Catalog; Roots : CCL.Resource_Policies.Selection;
      Bindings : out CCL.Resource_Policies.Binding_Map;
      Definitions : out CCL.Ownership.Type_Table;
      Count : out CCL.Resource_Policies.Layout_Count;
      Result : out CCL.Resource_Policies.Layout_Result);
   procedure Publish_Schema
     (Item : in out Interface_Catalog; Contract : CCL.Objects.Binding;
      Result : out CCL.Objects.Catalog.Publication_Result);
   procedure Resolve_Schema
     (Item : Interface_Catalog; Key : CCL.Objects.Schema_Key;
      Contract : out CCL.Objects.Binding);
   function Schema_Type (Item : Interface_Catalog; Key : CCL.Objects.Schema_Key)
     return CCL.Types.Type_Reference;

   procedure Publish
     (Item       : in out Interface_Catalog;
      Descriptor : Interface_Descriptor;
      Error      : out Catalog_Error);

   --  Qualified_Name is "interface.operation".  The caller supplies the
   --  catalog view explicitly: an empty view reveals nothing, while a
   --  Workbench or compiler holding discovery authority may receive a richer
   --  snapshot from the future catalog service.
   procedure Resolve
     (Item           : Interface_Catalog;
      Qualified_Name : String;
      Result         : out Resolved_Operation;
      Found          : out Boolean);

   function Length (Item : Interface_Catalog) return Interface_Count;

   function Same_Operation
     (Left, Right : Resolved_Operation) return Boolean;

   type Linkage_Table is private;
   type Intern_Result is (Linkage_Existing, Linkage_Added, Linkage_Full);

   procedure Initialize (Item : out Linkage_Table);

   procedure Intern
     (Item      : in out Linkage_Table;
      Operation : Resolved_Operation;
      Index     : out CCL.VM.Import_Index;
      Result    : out Intern_Result;
      Local     : CCL.Ownership.Binding_Id := 0);

   function Length (Item : Linkage_Table) return CCL.VM.Import_Count;

   function Element
     (Item  : Linkage_Table;
      Index : CCL.VM.Import_Index) return Resolved_Operation;

   --  Discovery is not authority. A Granted_Bindings value is populated by a
   --  trusted host only after it has obtained an authorized local endpoint or
   --  handle. It maps a pinned descriptor operation to that runtime-local
   --  binding without changing the interface identity or contract.
   type Granted_Bindings is private;
   type Grant_Result is
     (Grant_Added,
      Grant_Existing,
      Grant_Full,
      Invalid_Grant_Operation,
      Invalid_Runtime_Binding,
      Conflicting_Runtime_Binding);

   procedure Initialize (Item : out Granted_Bindings);

   function Length (Item : Granted_Bindings) return CCL.VM.Import_Count;

   procedure Install
     (Item      : in out Granted_Bindings;
      Operation : Resolved_Operation;
      Binding   : Unsigned_32;
      Result    : out Grant_Result);

   type Link_Result is
     (Link_Valid,
      Linkage_Length_Mismatch,
      Import_Contract_Mismatch,
      Authority_Not_Granted);

   -- Same exact descriptor/contract lookup used by bytecode linking. A name
   -- visible in a catalog does not by itself provide a runtime binding.
   procedure Find_Granted_Binding
     (Item : Granted_Bindings; Operation : Resolved_Operation;
      Binding : out Unsigned_32; Found : out Boolean);

   --  Link_Program is deliberately a separate admission step. Compilation
   --  leaves every runtime binding zero. This procedure validates all imports
   --  before installing any binding, so failure never partially links a
   --  program.
   procedure Link_Program
     (Grants  : Granted_Bindings;
      Linkage : Linkage_Table;
      Program : in out CCL.VM.Program;
      Result  : out Link_Result;
      Schemas : Interface_Catalog := Empty_Catalog);
   -- Typed imports require an explicit authorized schema view. The empty
   -- default permits only schema-free scalar imports; it grants nothing.

private
   subtype Name_Length is Natural range 0 .. MAX_NAME_LENGTH;
   subtype Name_Buffer is String (1 .. MAX_NAME_LENGTH);

   type Bounded_Name is record
      Length : Name_Length := 0;
      Data   : Name_Buffer := [others => ' '];
   end record;

   type Operation_Descriptor is record
      Name       : Bounded_Name;
      Parameters : Parameter_Count := 0;
      Import     : CCL.Host_Values.Import_Declaration := (others => <>);
      Defined    : Boolean := False;
   end record;

   type Operation_Array is
     array (Operation_Index) of Operation_Descriptor;

   type Interface_Descriptor is record
      Name              : Bounded_Name;
      Major             : Unsigned_16 := 0;
      Minor             : Unsigned_16 := 0;
      Digest            : Descriptor_Digest := EMPTY_DIGEST;
      Operations_Length : Operation_Count := 0;
      Operations        : Operation_Array := [others => (others => <>)];
      Defined           : Boolean := False;
   end record;

   subtype Interface_Index is Natural range 0 .. MAX_INTERFACES - 1;
   type Interface_Array is
     array (Interface_Index) of Interface_Descriptor;

   type Interface_Catalog is record
      Count       : Interface_Count := 0;
      Data_Types  : CCL.Objects.Catalog.Schema_Catalog;
      Resource_Policies : CCL.Resource_Policies.Policy_Table := [others => (others => <>)];
      Descriptors : Interface_Array := [others => (others => <>)];
   end record;

   type Linkage_Array is
     array (CCL.VM.Import_Index) of Resolved_Operation;
   type Linkage_Local_Array is
     array (CCL.VM.Import_Index) of CCL.Ownership.Binding_Id;

   type Linkage_Table is record
      Count   : CCL.VM.Import_Count := 0;
      Entries : Linkage_Array := [others => (others => <>)];
      Locals : Linkage_Local_Array := [others => 0];
   end record;

   type Granted_Binding is record
      Operation : Resolved_Operation := (others => <>);
      Binding   : Unsigned_32 := 0;
   end record;

   type Granted_Binding_Array is
     array (CCL.VM.Import_Index) of Granted_Binding;

   type Granted_Bindings is record
      Count   : CCL.VM.Import_Count := 0;
      Entries : Granted_Binding_Array := [others => (others => <>)];
   end record;

   function Length (Item : Interface_Catalog) return Interface_Count is
     (Item.Count);

   function Visible_Types (Item : Interface_Catalog) return CCL.Types.Registry is
     (CCL.Objects.Catalog.Visible_Types (Item.Data_Types));

   function Length (Item : Linkage_Table) return CCL.VM.Import_Count is
     (Item.Count);

   function Length (Item : Granted_Bindings) return CCL.VM.Import_Count is
     (Item.Count);

   function Element
     (Item  : Linkage_Table;
      Index : CCL.VM.Import_Index) return Resolved_Operation is
     (Item.Entries (Index));
end CCL.Catalog;
