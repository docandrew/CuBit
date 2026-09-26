with CCL.Declarations;

--  Shared native/host evaluator. Produces owned plans, never performs I/O.
package CCL.Configurations with SPARK_Mode => On is
   MAX_SETTINGS : constant := 128;
   MAX_LAUNCHES : constant := 16;
   type Profile_Kind is (System_Profile, Startup_Profile);
   type Network_Approval is (Deny, Approve_Declared);
   --  Trusted startup wiring, not a role an executable may claim for itself.
   type Startup_Role is (Application, Config_Storage);
   subtype Startup_Priority is Positive range 1 .. 10;
   type Key_Text is record
      Length : Natural range 0 .. 128 := 0;
      Data : String (1 .. 128) := [others => ' '];
   end record;
   type Value_Text is record
      Length : Natural range 0 .. 1024 := 0;
      Data : String (1 .. 1024) := [others => ' '];
   end record;
   type Executable_Text is record
      Length : Natural range 0 .. 64 := 0;
      Data : String (1 .. 64) := [others => ' '];
   end record;
   type Setting_Entry is record
      Key : Key_Text;
      Value : Value_Text;
   end record;
   type Launch_Entry is record
      Executable : Executable_Text;
      Priority : Startup_Priority := 5;
      Approval : Network_Approval := Deny;
      Role : Startup_Role := Application;
   end record;
   type Setting_Array is array (Positive range 1 .. MAX_SETTINGS) of Setting_Entry;
   type Launch_Array is array (Positive range 1 .. MAX_LAUNCHES) of Launch_Entry;
   type Configuration_Plan is record
      Kind : Profile_Kind := System_Profile;
      Setting_Count : Natural range 0 .. MAX_SETTINGS := 0;
      Launch_Count : Natural range 0 .. MAX_LAUNCHES := 0;
      Settings : Setting_Array;
      Launches : Launch_Array;
   end record;
   type Diagnostic_Code is
     (No_Error, Invalid_Syntax, Unsupported_Version, Unknown_Declaration,
      Invalid_Key, Invalid_Value, Duplicate_Key, Too_Many_Entries,
      Invalid_Executable, Invalid_Priority, Invalid_Approval, Invalid_Role, Duplicate_Field,
      Missing_Field, Trailing_Input);
   type Compilation_Result is record
      Success : Boolean := False;
      Diagnostic : Diagnostic_Code := No_Error;
      Syntax_Diagnostic : CCL.Declarations.Diagnostic_Code := CCL.Declarations.No_Error;
      Position : Positive := 1;
      Plan : Configuration_Plan;
   end record;
   procedure Compile (Source : String; Result : out Compilation_Result);
   --  Native runtime enumeration 'Image currently formats numeric positions.
   function Diagnostic_Name (Code : Diagnostic_Code) return String;
end CCL.Configurations;
