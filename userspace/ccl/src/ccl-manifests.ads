with Interfaces;
with CCL.Language;

--  Pure executable-declaration frontend. This emits requests, never grants.
--  Field expressions use the existing CCL evaluator without a host adapter.
package CCL.Manifests with SPARK_Mode => On is
   MAX_DECLARATION_LENGTH : constant := 4_096;
   MAX_SECTION_BYTES : constant := 1_024;
   MAX_BINDINGS : constant := 32;
   type Binding_Name is record
      Length : Natural range 0 .. 64 := 0;
      Data : String (1 .. 64) := [others => ' '];
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
      Slots_Exhausted);
   type Compilation_Result is record
      Success : Boolean := False;
      Diagnostic : Diagnostic_Code := No_Error;
      Position : Natural := 0;
      In_Catalog : Boolean := False;
      Expression_Diagnostic : CCL.Language.Diagnostic_Code :=
        CCL.Language.No_Diagnostic;
      Identity, Capabilities : Section;
      Binding_Count : Natural range 0 .. MAX_BINDINGS := 0;
      Bindings : Binding_Array := [others => (others => <>)];
   end record;
   procedure Compile
     (Source, Catalog_Source : String; Result : out Compilation_Result);
end CCL.Manifests;
