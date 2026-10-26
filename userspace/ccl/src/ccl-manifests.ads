with Interfaces;
with CCL.Language;
with CuBit.Failures;
with CuBit.Startup_Tables;
with CuBit.Channel_Declarations;
with CuBit.Parameter_Signatures;

--  Pure executable-declaration frontend. This emits requests, never grants.
--  Field expressions use the existing CCL evaluator without a host adapter.
package CCL.Manifests with SPARK_Mode => On is
   MAX_DECLARATION_LENGTH : constant := 4_096;
   MAX_SECTION_BYTES : constant := 4_096;
   MAX_BINDINGS : constant := 32;
   MAX_NAME_TEXT : constant := 64;
   MAX_SIGNATURE_BYTES : constant := CuBit.Parameter_Signatures.Maximum_Signature_Bytes;
   subtype Binding_Name_Length is Natural range 0 .. MAX_NAME_TEXT;
   type Binding_Name is record
      Length : Binding_Name_Length := 0;
      Data : String (1 .. MAX_NAME_TEXT) := [others => ' '];
   end record;
   --  The typed client a catalog service's startup handle is handed out as
   --  (CAP-003): the instance of that name in CuBit.Handles.Services
   --  (CuBit.Handles.Services.Logstore.Endpoint and so on), so a program can
   --  give it only to what serves that service. Plain: a CuBit.Handles
   --  Endpoint. The service catalog names it: (service logstore 13
   --  read-write (client Logstore)).
   type Service_Client is (Plain, Logstore, Log_Observer, Process_Manager, Config);
   --  Its spelling in the catalog, which is also its instance's Ada name.
   function Client_Name (Client : Service_Client) return String is
     (case Client is
         when Plain => "Plain",
         when Logstore => "Logstore",
         when Log_Observer => "Log_Observer",
         when Process_Manager => "Process_Manager",
         when Config => "Config");
   --  A request's startup binding: its name, its kind, whether the launch
   --  needs it (CuBit.Startup_Tables) and its typed client.
   type Named_Binding is record
      Name : Binding_Name;
      Kind : CuBit.Startup_Tables.Capability_Kind := CuBit.Startup_Tables.Endpoint;
      Requirement : CuBit.Startup_Tables.Need := CuBit.Startup_Tables.Required;
      Client : Service_Client := Plain;
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
      Reserved_Binding_Name, Not_Yet_Supported,
      --  A catalog (client …) that is not a Service_Client (CAP-003).
      Unknown_Client);
   type Compilation_Result is record
      Success : Boolean := False;
      Diagnostic : Diagnostic_Code := No_Error;
      Position : Natural := 0;
      In_Catalog : Boolean := False;
      Expression_Diagnostic : CCL.Language.Diagnostic_Code :=
        CCL.Language.No_Diagnostic;
      Identity, Capabilities, Access_Scopes, Resources : Section;
      --  .cubit.devices: the devices it drives (CCL.Device_Sections).
      Devices : Section;
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
      --  Typed program parameters (docs/ccl-launch-parameters.md): the
      --  parameter record's type name and its canonical signature
      --  (CCL.Parameter_Signatures); both empty when the manifest declares
      --  none (Parameter_Signature.None).
      Parameter_Root : Binding_Name;
      Parameter_Signature : String (1 .. MAX_SIGNATURE_BYTES) := [others => ' '];
      Parameter_Signature_Length : Natural range 0 .. MAX_SIGNATURE_BYTES := 0;
   end record;
   --  Schema_Source: interfaces/executable-manifest.ccl, needed for typed manifests.
   procedure Compile
     (Source, Catalog_Source : String; Result : out Compilation_Result; Schema_Source : String := "");
end CCL.Manifests;
