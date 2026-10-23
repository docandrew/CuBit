with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

--  build-params.ccl and a component's build.ccl, checked by the CCL type
--  checker against interfaces/build.ccl and read by field and alternative
--  name (no keyword reader: docs/ccl-build.md, "Steps").
package Build_Declarations is
   use Ada.Strings.Unbounded;

   Declaration_Error : exception;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);

   --  Link_Argument's alternatives, in the schema's order.
   type Link_Kind is (Text, File, Manifest_Object);
   type Link_Argument is record
      Kind : Link_Kind := Text;
      Value : Unbounded_String;    --  Text and File
   end record;
   package Link_Vectors is new Ada.Containers.Vectors (Positive, Link_Argument);

   --  Switches for some units instead of the program's compiler switches.
   type Unit_Switches is record
      Files, Switches : String_Vectors.Vector;
   end record;
   package Unit_Switch_Vectors is new Ada.Containers.Vectors (Positive, Unit_Switches);

   --  A library programs use: per-unit switches and archives to link.
   type Library is record
      Name : Unbounded_String;
      Units : Unit_Switch_Vectors.Vector;
      Archives : String_Vectors.Vector;
   end record;
   package Library_Vectors is new Ada.Containers.Vectors (Positive, Library);

   type Toolchain is record
      Name, Identity, Prefix : Unbounded_String;
   end record;

   type Catalog is (Native_Runtime, Driver, No_Manifest);
   subtype Manifest_Catalog is Catalog range Native_Runtime .. Driver;
   type Catalog_Paths is array (Manifest_Catalog) of Unbounded_String;

   --  The repository's build parameters: pins and the native profile.
   type Parameters is record
      Compiler_Tools, Binutils : Toolchain;
      Runtime, Manifest_Tool, Manifest_Schema : Unbounded_String;
      Catalogs : Catalog_Paths;
      Compiler, Binder, Binder_Unit : String_Vectors.Vector;
      Linker : Link_Vectors.Vector;
      --  What every compilation starts with (gprbuild's Builder switches);
      --  a library's unit switches follow it.
      Builder : String_Vectors.Vector;
      Libraries : Library_Vectors.Vector;
   end record;

   --  An Ada program with the profile's values filled in where its
   --  declaration says Profile.
   type Ada_Program is record
      Executable, Directory, Main : Unbounded_String;
      Sources : String_Vectors.Vector;
      Manifest_Catalog : Catalog := Native_Runtime;
      Compiler, Binder, Binder_Unit : String_Vectors.Vector;
      Linker : Link_Vectors.Vector;
      --  The libraries it uses (from the parameters), then its own unit
      --  switches; a unit's switches are the first group naming it here.
      Libraries : Library_Vectors.Vector;
      Units : Unit_Switch_Vectors.Vector;
      Builder : String_Vectors.Vector;
   end record;

   --  Evaluate Source after Schema (interfaces/build.ccl). Raise
   --  Declaration_Error with the checker's diagnostic.
   procedure Read_Parameters (Source, Schema : String; Result : out Parameters);
   --  build-libraries.ccl, into Profile's libraries.
   procedure Read_Libraries (Source, Schema : String; Profile : in out Parameters);
   procedure Read_Program
     (Source, Schema : String; Profile : Parameters; Program : out Ada_Program);

private
   --  Evaluation runs on a task with this stack: the CCL interpreter
   --  recurses with large frames (see ccl-manifest).
   EVALUATION_STACK_BYTES : constant Natural := 256 * 1024 * 1024;
end Build_Declarations;
