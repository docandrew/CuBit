with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Build_Platform;
with Build_Store;
with Storehouse;

--  Steps (docs/ccl-build.md, "Steps"): one typed tool call with its inputs
--  placed at relative paths, its arguments as typed pieces, and its named
--  outputs. A step's identity is the SHA-256 of its canonical encoding, in
--  which each input appears as its content identity; the rendered command
--  line, the step directory and the store's location never do.
package Build_Steps is
   use Ada.Strings.Unbounded;
   use Storehouse;

   Step_Error : exception;

   type Input is record
      Path : Unbounded_String;
      Kind : Entry_Kind;
      Identity : Digest;
   end record;

   type Output is record
      Path : Unbounded_String;
      Executable : Boolean := False;
   end record;

   subtype Input_Number is Positive;
   subtype Output_Number is Positive;

   --  One argv entry: Prefix & <what the kind names> & Suffix.
   --  Input_Path, Output_Path: the file's path in the step directory.
   --  Local_Input, Local_Output: the same, relative to the working
   --  directory (the file must be inside it). Step_Path: Path inside the
   --  step directory (a directory holding inputs, such as a runtime).
   --  Step_Root: the step directory itself, for -ffile-prefix-map.
   --  Tree_Path: Path inside a tree entry the step reads in place, or the
   --  entry itself when Path is empty: a toolchain recorded in place
   --  (binutils for -B), or a composite of input trees (a runtime and the
   --  directories it searches; Build_Store.Put_Composite), which is
   --  then never copied into the step directory. Tree names the entry, so
   --  the step's identity covers its content. On CuBit the step is given
   --  the entry read-only and nothing else of the Storehouse (Run).
   --  Literal: Prefix alone.
   type Piece_Kind is
     (Literal, Input_Path, Output_Path, Local_Input, Local_Output, Step_Path, Step_Root,
      Tree_Path);
   type Piece is record
      Kind : Piece_Kind := Literal;
      Prefix, Suffix : Unbounded_String;
      Index : Natural := 0;              --  Input_Path .. Local_Output
      Path : Unbounded_String;           --  Step_Path, Tree_Path
      Tree : Digest := [others => 0];    --  Tree_Path
   end record;

   --  What a step runs: a program inside a toolchain tree, one of the
   --  step's inputs, or a program installed on the build host at an
   --  absolute name (Path), run in place and named in the step by its
   --  SHA-256 (Contents): build-params.ccl's Installed manifest tool, on
   --  CuBit, where launch tables name programs exactly and a step
   --  directory's copy could not be started. Appended last, so the
   --  encoding of the other kinds, and their steps' identities, stay.
   type Program_Kind is (Toolchain_Program, Input_Program, Installed_Program);
   type Program is record
      Kind : Program_Kind := Toolchain_Program;
      Toolchain : Digest := [others => 0];    --  a tree entry
      Path : Unbounded_String;                --  inside the toolchain; installed: absolute
      Index : Natural := 0;                   --  an input of the step
      Contents : Digest := [others => 0];     --  installed: its SHA-256
   end record;

   package Input_Vectors is new Ada.Containers.Vectors (Input_Number, Input);
   package Output_Vectors is new Ada.Containers.Vectors (Output_Number, Output);
   package Piece_Vectors is new Ada.Containers.Vectors (Positive, Piece);

   type Step is record
      Label : Unbounded_String;       --  for people; not part of the identity
      Tool : Program;
      Arguments : Piece_Vectors.Vector;
      Inputs : Input_Vectors.Vector;
      Outputs : Output_Vectors.Vector;
      Directory : Unbounded_String;   --  the working directory, relative
      Capture : Natural := 0;         --  the output unix.stdout is written to
   end record;

   function Text (Value : String) return Piece is
     ((Kind => Literal, Prefix => To_Unbounded_String (Value), others => <>));
   function Add_Input (S : in out Step; Path : String; Kind : Entry_Kind; Identity : Digest)
     return Input_Number;
   function Add_Output (S : in out Step; Path : String; Executable : Boolean := False)
     return Output_Number;

   --  As above, for inputs and outputs no argument names.
   procedure Declare_Input (S : in out Step; Path : String; Kind : Entry_Kind; Identity : Digest);
   procedure Declare_Output (S : in out Step; Path : String; Executable : Boolean := False);

   --  Checks the step's names and references (Storehouse.Paths); raises
   --  Step_Error naming the first problem.
   procedure Check (S : Step);

   function Identity (S : Step) return Digest;
   --  The identity without the inputs' contents: what the step does, not to
   --  what. It names the step's directory (Build_Store.Claim_Step), so
   --  paths a tool records stay the same when only input contents change.
   function Shape (S : Step) return Digest;

   package Digest_Vectors is new Ada.Containers.Vectors (Output_Number, Digest);

   type Outcome is record
      Step_Identity : Digest;
      Outputs : Digest_Vectors.Vector;   --  by output number
      Ran : Boolean := False;            --  False: an action record answered
      Not_Reproducible : Boolean := False;
   end record;

   --  An action record of any step, decoded: the step's identity and its
   --  outputs. Raises Build_Encoding.Decode_Error unless Data is exactly a
   --  canonical action record.
   type Recorded_Output is record
      Path : Unbounded_String;
      Kind : Entry_Kind;
      Identity : Digest;
   end record;
   package Recorded_Vectors is new Ada.Containers.Vectors (Output_Number, Recorded_Output);
   procedure Decode_Action (Data : Byte_Array; Step_Identity : out Digest; Outputs : out Recorded_Vectors.Vector);

   --  Answers from the action cache when its record and every output entry
   --  are present; otherwise runs the step in a fresh directory holding only
   --  its inputs, stores the outputs and records the action. The step's
   --  process is given only its places (Build_Platform.Run_Confined): its
   --  directory, writable, and read-only the trees it reads in place (its
   --  Tree_Path pieces' and its toolchain's).
   procedure Run (Store : Build_Store.Store; S : Step; Result : out Outcome);

   --  The places S's process is given when it runs in Root (see Run).
   function Places (Store : Build_Store.Store; S : Step; Root : String) return Build_Platform.Place_List;

   --  The command a run would execute, for explanations and errors.
   function Command_Image (Store : Build_Store.Store; S : Step; Root : String) return String;

end Build_Steps;
