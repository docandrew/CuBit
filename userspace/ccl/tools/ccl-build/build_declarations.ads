with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

--  build-params.ccl, build-catalog.ccl and a component's component.ccl,
--  checked by the CCL type checker against interfaces/build.ccl and read by
--  field and alternative name (no keyword reader: docs/ccl-build.md,
--  "Steps"). A component is resolved here into what its rule family runs:
--  its executable from its name and kind, its catalog from its launcher,
--  its switches from its code, described by intent.
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

   --  How Ada code is compiled, by intent (the schema's Ada_Code).
   type Ada_Language is (Ada_2012, Ada_2022);
   type Optimization is (Normal, Maximum);
   type Checks is (Enabled, Suppressed);
   type Frame_Pointer is (Omitted, Kept);
   type Ada_Code is record
      Language : Ada_Language := Ada_2012;
      Level : Optimization := Normal;
      Run_Time_Checks : Checks := Enabled;
      Frames : Frame_Pointer := Omitted;
      Value_Range_Propagation : Boolean := True;
   end record;

   type Code_Profile is record
      Name : Unbounded_String;
      Code : Ada_Code;
   end record;
   package Profile_Vectors is new Ada.Containers.Vectors (Positive, Code_Profile);

   --  Units compiled with a named code profile.
   type Unit_Code is record
      Files : String_Vectors.Vector;
      Profile : Unbounded_String;
   end record;
   package Unit_Code_Vectors is new Ada.Containers.Vectors (Positive, Unit_Code);

   type Library is record
      Name : Unbounded_String;
      Sources, Archives : String_Vectors.Vector;
      Units : Unit_Code_Vectors.Vector;
   end record;
   package Library_Vectors is new Ada.Containers.Vectors (Positive, Library);

   type Build_Catalog is record
      Profiles : Profile_Vectors.Vector;
      Libraries : Library_Vectors.Vector;
   end record;

   type Toolchain is record
      Name, Identity, Prefix : Unbounded_String;
   end record;

   --  The service catalog a component's manifest is checked against, which
   --  follows from who launches it; one the kernel starts has none.
   type Catalog is (Process_Manager, Device_Manager, No_Manifest);
   subtype Manifest_Catalog is Catalog range Process_Manager .. Device_Manager;
   type Catalog_Paths is array (Manifest_Catalog) of Unbounded_String;

   --  The repository's build parameters.
   type Parameters is record
      Compiler_Tools, Binutils : Toolchain;
      Runtime, Manifest_Tool, Manifest_Schema : Unbounded_String;
      Catalogs : Catalog_Paths;
      Target, Binder : String_Vectors.Vector;
      Linker : Link_Vectors.Vector;
   end record;

   --  Switches for some units instead of the program's.
   type Unit_Switches is record
      Files, Switches : String_Vectors.Vector;
   end record;
   package Unit_Switch_Vectors is new Ada.Containers.Vectors (Positive, Unit_Switches);

   --  A component resolved for the Ada program rule family.
   type Ada_Program is record
      Name, Executable, Directory, Main : Unbounded_String;
      --  Its own source directories, then its libraries' (no repeats).
      Sources : String_Vectors.Vector;
      Manifest_Catalog : Catalog := Process_Manager;
      Compiler, Binder, Binder_Unit : String_Vectors.Vector;
      Linker : Link_Vectors.Vector;
      --  Its own unit groups, then its libraries'; a unit's switches are
      --  the first group naming it.
      Units : Unit_Switch_Vectors.Vector;
      Archives : String_Vectors.Vector;
   end record;

   --  The file a component is declared in, in its own directory.
   Component_File : constant String := "component.ccl";

   --  Evaluate Source after Schema (interfaces/build.ccl). Raise
   --  Declaration_Error with the checker's diagnostic.
   procedure Read_Parameters (Source, Schema : String; Result : out Parameters);
   procedure Read_Catalog (Source, Schema : String; Result : out Build_Catalog);
   --  Directory: the component's directory, relative to the tree's root.
   procedure Read_Component
     (Source, Schema, Directory : String; Profile : Parameters; Known : Build_Catalog;
      Program : out Ada_Program);

private
   --  Evaluation runs on a task with this stack: the CCL interpreter
   --  recurses with large frames (see ccl-manifest).
   EVALUATION_STACK_BYTES : constant Natural := 256 * 1024 * 1024;
end Build_Declarations;
