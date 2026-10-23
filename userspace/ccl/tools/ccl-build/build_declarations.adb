with Ada.Exceptions;
with CCL.Catalog;
with CCL.Evaluation;
with CCL.Language;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Types;

package body Build_Declarations is
   package Views renames CCL.Objects.Views;
   use type CCL.Language.Analysis_Status;
   use type CCL.Language.Interpretation_Status;

   --  The value never leaves this process: an in-process schema key.
   LOCAL_KEY : constant CCL.Objects.Schema_Key :=
     [16#4255_494C_442D_4343#, 16#4C2D_4C4F_4341_4C01#, 0, 0];
   Conventional_Main : constant String := "main.adb";
   --  A declaration is a constant expression; this bounds its evaluation.
   DECLARATION_FUEL : constant := 100_000;

   procedure Evaluate
     (Program_Text : String; Schema_Length : Natural; Root_Type : String;
      Object : in out Views.Snapshot)
   is
      Catalog : CCL.Catalog.Interface_Catalog;
      Checked : CCL.Language.Analysis_Result;
      Contract : CCL.Objects.Binding;
      Published, Captured : Boolean;
      Value : CCL.Language.Object_Interpretation_Result;

      --  Positions in the declaration, not in the schema before it.
      function Where (Position : Natural) return String is
        (if Position > Schema_Length then "character" & Natural'Image (Position - Schema_Length) & ": "
         else "in the schema, character" & Natural'Image (Position) & ": ");
   begin
      CCL.Catalog.Initialize (Catalog);
      CCL.Language.Analyze (Program_Text, Catalog, Checked);
      if CCL.Language.Analysis_Status_Of (Checked) /= CCL.Language.Analysis_Succeeded then
         raise Declaration_Error with
           Where (CCL.Language.Analysis_Diagnostic_Position (Checked)) &
           CCL.Language.Analysis_Status_Of (Checked)'Image & " " &
           CCL.Language.Analysis_Diagnostic (Checked)'Image;
      end if;
      CCL.Objects.Bind
        (CCL.Language.Analysis_Types (Checked),
         CCL.Types.Find (CCL.Language.Analysis_Types (Checked), CCL.Types.Named (Root_Type)),
         LOCAL_KEY, Contract, Published);
      if not Published then
         raise Declaration_Error with "the schema declares no persistable " & Root_Type;
      end if;
      CCL.Evaluation.Evaluate_Object (Program_Text, DECLARATION_FUEL, Contract, Value);
      if Value.Status /= CCL.Language.Succeeded or else not Value.Has_Value then
         raise Declaration_Error with
           Where (Value.Diagnostic_Position) & Value.Status'Image & " " &
           Value.Diagnostic'Image & " " & CCL.Types.Image (Value.Diagnostic_Subject) &
           " (expected a " & Root_Type & ")";
      end if;
      Views.Capture (Object, Contract, Value.Value, Captured);
      if not Captured then
         raise Declaration_Error with "the declaration's value could not be read";
      end if;
   end Evaluate;

   --  Reading a captured value by field and alternative name.
   generic
      type Result_Type is private;
      with procedure Read_Value (Object : Views.Snapshot; Result : out Result_Type);
   procedure Read_Document (Source, Schema, Root_Type : String; Result : out Result_Type);
   procedure Read_Document (Source, Schema, Root_Type : String; Result : out Result_Type) is
      Failure : Unbounded_String;
      Done : Boolean := False;
      Value : Result_Type;
   begin
      if Schema'Length + 1 + Source'Length > CCL.Language.MAX_SOURCE_LENGTH then
         raise Declaration_Error with "schema and declaration exceed" &
           Natural'Image (CCL.Language.MAX_SOURCE_LENGTH) & " bytes together";
      end if;
      declare
         task Evaluation is
            pragma Storage_Size (EVALUATION_STACK_BYTES);
         end Evaluation;
         task body Evaluation is
            Object : Views.Snapshot;
         begin
            Evaluate (Schema & ASCII.LF & Source, Schema'Length + 1, Root_Type, Object);
            Read_Value (Object, Value);
            Done := True;
         exception
            when E : Declaration_Error | Constraint_Error =>
               Failure := To_Unbounded_String (Ada.Exceptions.Exception_Message (E));
         end Evaluation;
      begin
         null;
      end;
      if not Done then
         raise Declaration_Error with To_String (Failure);
      end if;
      Result := Value;
   end Read_Document;

   function Alternative (Object : Views.Snapshot; At_Cursor : Views.Cursor) return String is
      Shape : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
      Choice : constant Natural := Views.Alternative (Object, At_Cursor);
   begin
      return (if Choice in 1 .. Shape.Count then CCL.Types.Image (Shape.Parts (Choice).Identifier) else "");
   end Alternative;

   --  A record's field by name: the schema decides the order.
   function Named (Object : Views.Snapshot; At_Cursor : Views.Cursor; Name : String) return Views.Cursor is
      Shape : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
   begin
      for Part in 1 .. Shape.Count loop
         if CCL.Types.Image (Shape.Parts (Part).Identifier) = Name then
            return Views.Field (Object, At_Cursor, Part);
         end if;
      end loop;
      raise Declaration_Error with "the schema has no field " & Name;
   end Named;

   function Field (Object : Views.Snapshot; At_Cursor : Views.Cursor; Name : String) return Unbounded_String is
     (To_Unbounded_String (Views.Text (Object, Named (Object, At_Cursor, Name))));

   procedure Texts (Object : Views.Snapshot; At_Cursor : Views.Cursor; Into : out String_Vectors.Vector) is
   begin
      Into.Clear;
      for I in 1 .. Views.Length (Object, At_Cursor) loop
         Into.Append (Views.Text (Object, Views.Element (Object, At_Cursor, I)));
      end loop;
   end Texts;

   procedure Links (Object : Views.Snapshot; At_Cursor : Views.Cursor; Into : out Link_Vectors.Vector) is
   begin
      Into.Clear;
      for I in 1 .. Views.Length (Object, At_Cursor) loop
         declare
            Argument : constant Views.Cursor := Views.Element (Object, At_Cursor, I);
            Kind : constant Link_Kind := Link_Kind'Value (Alternative (Object, Argument));
         begin
            Into.Append
              (Link_Argument'(Kind => Kind,
                              Value => (if Kind in Text | File
                                        then To_Unbounded_String (Views.Text (Object, Views.Payload (Object, Argument)))
                                        else Null_Unbounded_String)));
         end;
      end loop;
   end Links;

   procedure Unit_Groups (Object : Views.Snapshot; At_Cursor : Views.Cursor; Into : out Unit_Switch_Vectors.Vector) is
   begin
      Into.Clear;
      for I in 1 .. Views.Length (Object, At_Cursor) loop
         declare
            Group : constant Views.Cursor := Views.Element (Object, At_Cursor, I);
            Item : Unit_Switches;
         begin
            Texts (Object, Named (Object, Group, "files"), Item.Files);
            Texts (Object, Named (Object, Group, "switches"), Item.Switches);
            Into.Append (Item);
         end;
      end loop;
   end Unit_Groups;

   function Toolchain_Of (Object : Views.Snapshot; At_Cursor : Views.Cursor) return Toolchain is
     ((Name => Field (Object, At_Cursor, "name"), Identity => Field (Object, At_Cursor, "identity"),
       Prefix => Field (Object, At_Cursor, "prefix")));

   procedure Parameters_Value (Object : Views.Snapshot; Result : out Parameters) is
      Root : constant Views.Cursor := Views.Root (Object);
   begin
      Result.Compiler_Tools := Toolchain_Of (Object, Named (Object, Root, "toolchain"));
      Result.Binutils := Toolchain_Of (Object, Named (Object, Root, "binutils"));
      Result.Runtime := Field (Object, Root, "runtime");
      Result.Manifest_Tool := Field (Object, Root, "manifest_tool");
      Result.Manifest_Schema := Field (Object, Root, "manifest_schema");
      Result.Catalogs (Native_Runtime) := Field (Object, Root, "native_catalog");
      Result.Catalogs (Driver) := Field (Object, Root, "driver_catalog");
      Texts (Object, Named (Object, Root, "compiler"), Result.Compiler);
      Texts (Object, Named (Object, Root, "binder"), Result.Binder);
      Texts (Object, Named (Object, Root, "binder_unit"), Result.Binder_Unit);
      Links (Object, Named (Object, Root, "linker"), Result.Linker);
      Texts (Object, Named (Object, Root, "builder"), Result.Builder);
      Result.Libraries.Clear;
   end Parameters_Value;

   procedure Libraries_Value (Object : Views.Snapshot; Result : out Library_Vectors.Vector) is
      Root : constant Views.Cursor := Views.Root (Object);
   begin
      declare
         List : constant Views.Cursor := Named (Object, Root, "libraries");
      begin
         Result.Clear;
         for I in 1 .. Views.Length (Object, List) loop
            declare
               Item : constant Views.Cursor := Views.Element (Object, List, I);
               L : Library;
            begin
               L.Name := Field (Object, Item, "name");
               Unit_Groups (Object, Named (Object, Item, "units"), L.Units);
               Texts (Object, Named (Object, Item, "archives"), L.Archives);
               Result.Append (L);
            end;
         end loop;
      end;
   end Libraries_Value;

   procedure Read_Libraries_Document is new Read_Document (Library_Vectors.Vector, Libraries_Value);

   procedure Read_Libraries (Source, Schema : String; Profile : in out Parameters) is
   begin
      Read_Libraries_Document (Source, Schema, "Library_Catalog", Profile.Libraries);
   end Read_Libraries;

   procedure Read_Parameters_Document is new Read_Document (Parameters, Parameters_Value);

   procedure Read_Parameters (Source, Schema : String; Result : out Parameters) is
   begin
      Read_Parameters_Document (Source, Schema, "Parameters", Result);
   end Read_Parameters;

   --  A program as declared: its own lists, and which parts are the
   --  profile's (filled in by Read_Program).
   type Switch_Part is (Compiler_Part, Binder_Part, Binder_Unit_Part, Linker_Part, Builder_Part);
   type Profile_Parts is array (Switch_Part) of Boolean;
   type Declared_Program is record
      Program : Ada_Program;
      Library_Names : String_Vectors.Vector;
      From_Profile : Profile_Parts := [others => False];
   end record;

   procedure Program_Value (Object : Views.Snapshot; Result : out Declared_Program) is
      Root : constant Views.Cursor := Views.Root (Object);

      --  A Switches value: the profile's, or exactly the listed ones.
      procedure Choose (At_Cursor : Views.Cursor; Part : Switch_Part; Into : out String_Vectors.Vector) is
      begin
         Into.Clear;
         Result.From_Profile (Part) := Alternative (Object, At_Cursor) = "Profile";
         if not Result.From_Profile (Part) and then Alternative (Object, At_Cursor) /= "No_Switches" then
            Texts (Object, Views.Payload (Object, At_Cursor), Into);
         end if;
      end Choose;
   begin
      if Alternative (Object, Root) /= "Ada_Program" then
         raise Declaration_Error with "unknown component kind " & Alternative (Object, Root);
      end if;
      declare
         Item : constant Views.Cursor := Views.Payload (Object, Root);
         Linker : constant Views.Cursor := Named (Object, Item, "linker");
         P : Ada_Program renames Result.Program;
      begin
         P.Executable := Field (Object, Item, "executable");
         P.Directory := Field (Object, Item, "directory");
         declare
            Main : constant Views.Cursor := Named (Object, Item, "main");
         begin
            P.Main := (if Alternative (Object, Main) = "Conventional" then To_Unbounded_String (Conventional_Main)
                       else To_Unbounded_String (Views.Text (Object, Views.Payload (Object, Main))));
         end;
         Texts (Object, Named (Object, Item, "sources"), P.Sources);
         P.Manifest_Catalog := Catalog'Value (Alternative (Object, Named (Object, Item, "catalog")));
         Choose (Named (Object, Item, "compiler"), Compiler_Part, P.Compiler);
         Choose (Named (Object, Item, "binder"), Binder_Part, P.Binder);
         Choose (Named (Object, Item, "binder_unit"), Binder_Unit_Part, P.Binder_Unit);
         Choose (Named (Object, Item, "builder"), Builder_Part, P.Builder);
         Result.From_Profile (Linker_Part) := Alternative (Object, Linker) = "Profile";
         P.Linker.Clear;
         if not Result.From_Profile (Linker_Part) then
            Links (Object, Views.Payload (Object, Linker), P.Linker);
         end if;
         Texts (Object, Named (Object, Item, "libraries"), Result.Library_Names);
         Unit_Groups (Object, Named (Object, Item, "units"), P.Units);
      end;
   end Program_Value;

   procedure Read_Program_Document is new Read_Document (Declared_Program, Program_Value);

   procedure Read_Program
     (Source, Schema : String; Profile : Parameters; Program : out Ada_Program)
   is
      Declared : Declared_Program;
   begin
      Read_Program_Document (Source, Schema, "Component", Declared);
      Program := Declared.Program;
      if Declared.From_Profile (Compiler_Part) then Program.Compiler := Profile.Compiler; end if;
      if Declared.From_Profile (Binder_Part) then Program.Binder := Profile.Binder; end if;
      if Declared.From_Profile (Binder_Unit_Part) then Program.Binder_Unit := Profile.Binder_Unit; end if;
      if Declared.From_Profile (Linker_Part) then Program.Linker := Profile.Linker; end if;
      if Declared.From_Profile (Builder_Part) then Program.Builder := Profile.Builder; end if;
      for Name of Declared.Library_Names loop
         declare
            Found : Boolean := False;
         begin
            for L of Profile.Libraries loop
               if To_String (L.Name) = Name then
                  Program.Libraries.Append (L);
                  Found := True;
               end if;
            end loop;
            if not Found then
               raise Declaration_Error with "build-params.ccl names no library " & Name;
            end if;
         end;
      end loop;
   end Read_Program;

end Build_Declarations;
