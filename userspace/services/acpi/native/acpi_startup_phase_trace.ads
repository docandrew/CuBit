pragma Ada_2022;
with Interfaces;
package ACPI_Startup_Phase_Trace with Pure, SPARK_Mode is
   type Phase_Code is
     (No_Phase, Early_Node_Errors, HID_Discovery, HID_Path_Diagnostics,
      HID_Module_Notes, Power_Discovery, Power_Module_Notes, Close_Startup,
      Enter_Runtime, Register_Service, Arm_SCI, Start_Events, Offer_HID,
      Offer_Path_Diagnostics, Complete_Consumers, Serve);
   subtype Measured_Phase is Phase_Code range Early_Node_Errors .. Serve;
   type Phase_Edge is (Not_Started, Phase_Entered, Phase_Exited);
   -- Diagnostic text policy only; never an AML/table/resource limit.
   Maximum_Outcome_Characters : constant Positive := 48;
   subtype Outcome_Length is Natural range 0 .. Maximum_Outcome_Characters;
   subtype Outcome_Text is String (1 .. Maximum_Outcome_Characters);
   type Outcome is record
      Data : Outcome_Text := [others => ' '];
      Length : Outcome_Length := 0;
      Truncated : Boolean := False;
   end record;
   function Text (Value : Outcome) return String with Global => null,
      Post => Text'Result'Length = Value.Length;
   type Checkpoint is record
      Last_Phase : Phase_Code := No_Phase;
      Edge : Phase_Edge := Not_Started;
      Result : Outcome;
      Succeeded : Boolean := False;
      Failure_Present : Boolean := False;
      Failed_Phase : Phase_Code := No_Phase;
      Failed_Result : Outcome;
      Consistent : Boolean := True;
   end record;
   Maximum_Phase_Name_Characters : constant Positive := 22;
   Maximum_Edge_Name_Characters : constant Positive := 13;
   -- Explicit names survive native Discard_Names builds.
   function Name (Value : Phase_Code) return String with Global => null,
      Post => Name'Result'Length <= Maximum_Phase_Name_Characters;
   function Name (Value : Phase_Edge) return String with Global => null,
      Post => Name'Result'Length <= Maximum_Edge_Name_Characters;
   Maximum_Diagnostic_Characters : constant Positive := 512;
   function Image (Value : Checkpoint; Publisher_Dropped : Interfaces.Unsigned_64)
      return String with Global => null,
      Post => Image'Result'Length <= Maximum_Diagnostic_Characters;
   type State is private;
   -- Copied diagnostics only: no owner decision, AML, IO or borrowed handle.
   -- Repeated entry/exit and exit without entry make Consistent sticky-false.
   -- Successful later steps never clear a prior unsuccessful outcome.
   procedure Enter (Item : in out State; Phase : Measured_Phase);
   procedure Leave (Item : in out State; Phase : Measured_Phase;
      Result : String; Succeeded : Boolean);
   function Observe (Item : State) return Checkpoint with Global => null;
private
   type Seen_Array is array (Measured_Phase) of Boolean;
   type State is record
      View : Checkpoint;
      Entered, Exited : Seen_Array := [others => False];
   end record;
end ACPI_Startup_Phase_Trace;
