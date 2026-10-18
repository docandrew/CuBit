with Interfaces;
with CCL.Language;
with CuBit.Failures;
with CuBit.Startup_Tables;
with CuBit.Channel_Declarations;

--  Pure executable-declaration frontend. This emits requests, never grants.
--  Field expressions use the existing CCL evaluator without a host adapter.
package CCL.Manifests with SPARK_Mode => On is
   MAX_DECLARATION_LENGTH : constant := 4_096;
   MAX_SECTION_BYTES : constant := 4_096;
   MAX_BINDINGS : constant := 32;
   MAX_NAME_TEXT : constant := 64;
   subtype Binding_Name_Length is Natural range 0 .. MAX_NAME_TEXT;
   type Binding_Name is record
      Length : Binding_Name_Length := 0;
      Data : String (1 .. MAX_NAME_TEXT) := [others => ' '];
   end record;
   --  A request's startup binding: its name, its kind and whether the
   --  launch needs it (CuBit.Startup_Tables).
   type Named_Binding is record
      Name : Binding_Name;
      Kind : CuBit.Startup_Tables.Capability_Kind := CuBit.Startup_Tables.Endpoint;
      Requirement : CuBit.Startup_Tables.Need := CuBit.Startup_Tables.Required;
   end record;
   type Binding_Array is array (Positive range 1 .. MAX_BINDINGS) of Named_Binding;
   type Byte_Array is array (Positive range 1 .. MAX_SECTION_BYTES)
     of Interfaces.Unsigned_8;
   type Section is record
      Length : Natural range 0 .. MAX_SECTION_BYTES := 0;
      Data : Byte_Array := [others => 0];
   end record;
   type Diagnostic_Code is
     (No_Error, Source_Too_Long, Expected_Form, Unexpected_End,
      Unknown_Declaration, Duplicate_Field, Missing_Field, Invalid_Expression,
      Expected_Text, Invalid_Text, Unknown_Service, Unknown_Rights,
      Duplicate_Binding, Too_Many_Requests, Unsupported_Version,
      Trailing_Input, Nesting_Too_Deep, Invalid_Service_ID, Duplicate_Service,
      Too_Many_Services, Rights_Not_Offered, Invalid_Binding_Name,
      Invalid_Path, Invalid_Access_Rights,
      Too_Many_Scopes, Duplicate_Scope, Invalid_Connector, Invalid_Network_Scope,
      Unknown_Notification,
      Invalid_Notification_ID, Invalid_Device_Match, Duplicate_Device_Match,
      Missing_Device_Match, Invalid_Device_Resource, Invalid_Scheduling,
      Invalid_Launch, Invalid_Parameters,
      --  Startup capabilities (CAP-002).
      Invalid_Handle_Limit, Invalid_Channel_Limit, Unknown_Optional_Name,
      Reserved_Binding_Name, Not_Yet_Supported);
   type Compilation_Result is record
      Success : Boolean := False;
      Diagnostic : Diagnostic_Code := No_Error;
      Position : Natural := 0;
      In_Catalog : Boolean := False;
      Expression_Diagnostic : CCL.Language.Diagnostic_Code :=
        CCL.Language.No_Diagnostic;
      Identity, Capabilities, Access_Scopes, Resources : Section;
      --  .cubit.launch: the programs this executable may start (CuBit.Launch_Authority).
      Launch : Section;
      --  .cubit.description: typed parameters and their argv, ports and the
      --  descriptor map (CuBit.Program_Descriptions).
      Description : Section;
      --  .cubit.startup: the declared startup table (every entry Unassigned)
      --  and the handle limit (CuBit.Startup_Tables).
      Startup : Section;
      --  .cubit.channels: the channel offers the program serves
      --  (CuBit.Channel_Declarations), and the same as a table for the
      --  generated Startup package; empty when it declares none.
      Channels : Section;
      Channel_Offers : CuBit.Channel_Declarations.Table;
      Handles : CuBit.Startup_Tables.Handle_Limit;
      Binding_Count : Natural range 0 .. MAX_BINDINGS := 0;
      Bindings : Binding_Array := [others => (others => <>)];
      --  Which entry is wrong, why, and what would fix it (typed manifests).
      Why : CuBit.Failures.Failure;
   end record;
   --  Schema_Source: interfaces/executable-manifest.ccl, needed for typed manifests.
   procedure Compile
     (Source, Catalog_Source : String; Result : out Compilation_Result; Schema_Source : String := "");
end CCL.Manifests;
