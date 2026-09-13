with CCL.Declarations;

--  Pure, bounded image planning. Artifact lookup is not host file access.
package CCL.Images with SPARK_Mode => On is
   MAX_ITEMS : constant := 64;
   MAX_PATH : constant := 192;
   type Text is record
      Length : Natural range 0 .. MAX_PATH := 0;
      Data : String (1 .. MAX_PATH) := [others => ' '];
   end record;
   function Value (Item : Text) return String;
   function Safe_Path (Path : String) return Boolean;
   function Overlaps (Left, Right : Text) return Boolean;

   type Source_Kind is (Repository_File, Supplied_File, Supplied_Tree);
   type Region_Kind is (Bootstrap, Optical);
   type Layout_Kind is (Bootstrap_Only, Optical_Image);
   type Role_Kind is (Content, Startup, Settings, Kernel, Boot_Menu);
   type Artifact is record
      Name : Text;
      Kind : Source_Kind := Repository_File;
      Source : Text;
   end record;
   type Requirement is record
      Provider : Text;
      Artifact_Name : Text;
      Destination : Text;
   end record;
   type Placement is record
      Artifact_Name : Text;
      Region : Region_Kind := Bootstrap;
      Role : Role_Kind := Content;
      Destination : Text;
   end record;
   type Artifact_Array is array (Positive range 1 .. MAX_ITEMS) of Artifact;
   type Requirement_Array is array (Positive range 1 .. MAX_ITEMS) of Requirement;
   type Placement_Array is array (Positive range 1 .. MAX_ITEMS) of Placement;
   type Document_Kind is (Artifact_Catalog, System_Image);
   type Document is record
      Kind : Document_Kind := System_Image;
      Catalog_Name : Text;
      Provider : Text;
      Layout : Layout_Kind := Bootstrap_Only;
      Artifact_Count : Natural range 0 .. MAX_ITEMS := 0;
      Requirement_Count : Natural range 0 .. MAX_ITEMS := 0;
      Placement_Count : Natural range 0 .. MAX_ITEMS := 0;
      Artifacts : Artifact_Array;
      Requirements : Requirement_Array;
      Placements : Placement_Array;
   end record;
   type Diagnostic_Code is
     (No_Error, Invalid_Syntax, Unsupported_Version, Unknown_Declaration, Invalid_Value,
      Invalid_Path, Duplicate_Name, Duplicate_Destination, Duplicate_Field,
      Too_Many_Items, Missing_Field, Catalog_Mismatch, Unknown_Artifact,
      Unknown_Provider, Missing_Bootstrap_Dependency, Invalid_Placement);
   type Compilation_Result is record
      Success : Boolean := False;
      Diagnostic : Diagnostic_Code := No_Error;
      Syntax_Diagnostic : CCL.Declarations.Diagnostic_Code := CCL.Declarations.No_Error;
      Position : Positive := 1;
      Plan : Document;
   end record;
   procedure Compile (Source : String; Result : out Compilation_Result);
   --  No plan may be realized until both documents compile and this succeeds.
   procedure Validate
     (Catalog, Profile : Document; Diagnostic : out Diagnostic_Code);
end CCL.Images;
