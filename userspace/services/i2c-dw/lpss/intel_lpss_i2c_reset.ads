pragma Ada_2022;
with Interfaces;
with DW_I2C_Transfer;
with Intel_LPSS_I2C_Profile;
package Intel_LPSS_I2C_Reset with SPARK_Mode is
   package T renames DW_I2C_Transfer;
   package P renames Intel_LPSS_I2C_Profile;
   subtype Word is Interfaces.Unsigned_32;
   subtype Physical_Address is Interfaces.Unsigned_64;
   type Register_ID is (Capabilities, Reset_Control, Remap_Low, Remap_High);
   type Register_Offset is range 0 .. 16#2FC#;
   function Offset (ID : Register_ID) return Register_Offset;
   Minimum_Window_Bytes : constant Physical_Address := 16#300#;
   Function_Reset_Mask : constant Word := 16#3#;
   Release_Resets : constant Word := 16#7#;
   Controller_Type_Mask : constant Word := 16#F0#;
   type Stage is (Idle, Read_Capabilities, Assert_Reset, Confirm_Reset,
                  Release_Reset, Confirm_Release, Set_Remap_Low, Set_Remap_High,
                  Confirm_Remap_Low, Confirm_Remap_High, Ready, Failed);
   type Access_Status is (Access_OK, Unavailable, Denied, Unknown_Effect);
   type Failure is (None, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Budget_Exhausted, Wrong_Controller_Type, Register_Refused,
      Register_Indeterminate, Remap_Mismatch);
   type Admission is (Accepted, Busy, Unsupported_Device, Invalid_Window, Expired);
   type Report is record
      At_Stage : Stage := Idle;
      Failed_At : Stage := Idle;
      Reason : Failure := None;
      Last_Register : Register_ID := Capabilities;
      Last_Value : Word := 0;
      Last_Status : Access_Status := Access_OK;
      Access_Attempted : Boolean := False;
      Reset_May_Be_Released : Boolean := True;
      Steps_Used, Writes_Completed : T.Count := 0;
      Clock_Hz : P.Hertz := 0;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   procedure Start
     (S : in out State; Vendor, Device : P.PCI_ID;
      BAR_Base, Window_Bytes : Physical_Address; Now, Deadline : T.Tick;
      Step_Budget : T.Limit; Result : out Admission);
   -- BAR_Base/Window_Bytes are internal launcher-validated BAR/grant facts.
   -- Identity checks only select this profile; they grant no address authority.
   -- No client may choose remap addresses. Reset ownership excludes all other
   -- users of the controller, including its DMA engine.
   generic
      with procedure Clock (Now : out T.Tick; Available : out Boolean);
      with procedure Read (ID : Register_ID; Value : out Word;
                           Status : out Access_Status);
      with procedure Write (ID : Register_ID; Value : Word;
                            Status : out Access_Status);
   procedure Step (S : in out State);
   -- One bounded, non-reentrant, non-raising primitive access per Step. Owner
   -- supplies barriers and scheduling. Waits are bounded by deadline AND quota.
   -- No retry of writes/unknown outcomes; no restart of a terminal owner. Clock
   -- errors retain successful reset/remap effects for supervisor cleanup.
private
   type State is limited record
      Info : Report;
      Low_Address, High_Address : Word := 0;
      Last_Time, Expires : T.Tick := 0;
      Remaining : T.Count := 0;
   end record;
end Intel_LPSS_I2C_Reset;
