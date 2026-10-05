with Interfaces;
with CCL.Language;
with CuBit.Failures;

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
   type Named_Binding is record
      Name : Binding_Name;
      Slot : Natural range 1 .. 62 := 1;
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
      Invalid_Slot, Duplicate_Binding, Too_Many_Requests, Unsupported_Version,
      Trailing_Input, Nesting_Too_Deep, Invalid_Service_ID, Duplicate_Service,
      Too_Many_Services, Rights_Not_Offered, Invalid_Binding_Name,
      Slots_Exhausted, Duplicate_Slot, Invalid_Path, Invalid_Access_Rights,
      Too_Many_Scopes, Duplicate_Scope, Invalid_Connector, Invalid_Network_Scope,
      Unknown_Notification,
      Invalid_Notification_ID, Invalid_Device_Match, Duplicate_Device_Match,
      Missing_Device_Match, Invalid_Device_Resource, Invalid_Scheduling,
      Invalid_Launch, Invalid_Parameters);
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
      Binding_Count : Natural range 0 .. MAX_BINDINGS := 0;
      Bindings : Binding_Array := [others => (others => <>)];
      --  Which entry is wrong, why, and what would fix it (typed manifests).
      Why : CuBit.Failures.Failure;
   end record;
   --  Schema_Source: interfaces/executable-manifest.ccl, needed for typed manifests.
   procedure Compile
     (Source, Catalog_Source : String; Result : out Compilation_Result; Schema_Source : String := "");
end CCL.Manifests;
