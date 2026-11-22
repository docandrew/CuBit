pragma Ada_2022;
with Interfaces;
with ACPI_FADT;
with ACPI_Backend_Windows;
with ACPI_Backend_Binding_Types;
with ACPI_Sleep_Entry;
generic
   type Hardware_State is limited private;
   type Access_View (<>) is private;
   with function Allowed (View : Access_View) return Boolean;
   with function Select_Access
     (Hardware : Hardware_State; ID : ACPI_Backend_Windows.Window_ID;
      Space : ACPI_Backend_Windows.P.Address_Space; Address : Interfaces.Unsigned_64;
      Width : ACPI_Backend_Windows.T.Access_Width; For_Write : Boolean) return Access_View;
   -- Immediate borrowed view. One exact transaction, no retained view, retry,
   -- reentry or owner revocation. Unknown writes MUST be Indeterminate.
   with procedure Transfer (View : Access_View; Width : ACPI_Backend_Windows.T.Access_Width;
      For_Write : Boolean; Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64;
      Status : out ACPI_Sleep_Entry.Completion);
   with procedure Read_Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
package ACPI_Backend_Sleep_Entry with SPARK_Mode is
   package W renames ACPI_Backend_Windows;
   package S renames ACPI_Sleep_Entry;
   subtype Word is Interfaces.Unsigned_64;
   type Register_Name is (Control_A, Control_B, Status_A, Status_B,
      GPE0_Block, GPE1_Block, Reduced_Control, Reduced_Status);
   type Windows is array (Register_Name) of W.Window_ID;
   No_Windows : constant Windows := [others => W.No_Window];
   -- Pure startup cross-check: a fresh kernel inspection of the retained
   -- port handle must exactly match both the launch claim and catalog view.
   -- This predicate neither creates authority nor chooses a window ID.
   function Port_Window_Matches (Current : ACPI_Backend_Binding_Types.Description;
      Retained : W.Window_Result; Expected_First : Interfaces.Unsigned_16;
      Expected_Count : Interfaces.Unsigned_32; Address, Length : Word;
      Reading : Boolean) return Boolean with Global => null;
   type Outcome is (Not_Prepared, Prepared, Already_Prepared, Reentered,
      Missing_Register, Bad_Geometry, Overlapping_Registers, Window_Unavailable,
      Invalid_Action, Replayed_Action, Invalid_Deadline, Clock_Unavailable,
      Clock_Reversed, Deadline_Expired, Transferred, Not_Sent, Unknown_Effect);
   subtype Transfer_Count is Natural range 0 .. 1_024;
   type Report is record
      Status : Outcome := Not_Prepared;
      Model : S.Register_Model := S.No_Registers;
      At_Register : Register_Name := Control_A;
      Completion : S.Completion := S.Not_Attempted;
      Transfers, Completed_Writes : Transfer_Count := 0;
   end record;
   type State is limited private;
   function Observe (Item : State) return Report;
   -- Trusted startup only: immutable copied FADT + independently retained
   -- catalog windows. No public request may install these descriptors.
   -- Preparation performs access selection only, never register transfers.
   procedure Prepare (Item : in out State; Hardware : Hardware_State;
      FADT : ACPI_FADT.Description; Held : Windows; Result : out Report);
   -- Only typed named actions from the local S5 sequencer. The first action
   -- latches one finite absolute deadline; later calls must use that same one.
   -- Each action is consumed once. Any failure is terminal; partial bulk
   -- effects become Indeterminate. Every datum obtains a fresh catalog view.
   procedure Execute (Item : in out State; Hardware : Hardware_State;
      Command : S.Command; Deadline : Word; Output : out S.Control_Word; Result : out Report);
private
   type Descriptor is record
      Present : Boolean := False;
      Window : W.Window_ID := W.No_Window;
      Space : W.P.Address_Space := W.P.Memory_Space;
      Address : Word := 0;
      Length : Positive range 1 .. 255 := 1;
   end record;
   type Descriptors is array (Register_Name) of Descriptor;
   type Action_Set is array (S.Action) of Boolean;
   type Phase is (Fresh, Ready, Failed);
   type State is limited record
      Stage : Phase := Fresh;
      Working, Clock_Seen : Boolean := False;
      Until_Time, Last_Time : Word := 0;
      Items : Descriptors;
      Seen : Action_Set := [others => False];
      Info : Report;
   end record;
end ACPI_Backend_Sleep_Entry;
