pragma Ada_2022;
with Interfaces;
with ACPI_Service_Core;
with ACPI_Sleep_Preparation;
with ACPI_Sleep_Objects;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Methods is new ACPI_Sleep_Preparation (Core => Core, others => <>);
   with package Objects is new ACPI_Sleep_Objects (Core => Core, others => <>);
   -- Trusted owner boundaries: one already-open finite native runtime window;
   -- no reset/reentry, deadline renewal or catalog/owner replacement. Clock
   -- reads must not invoke AML. May_Continue certifies local owner safety.
   with procedure Read_Clock (Now_Microseconds : out Interfaces.Unsigned_64;
      Available : out Boolean);
   with function May_Continue (Service : Core.State) return Boolean;
package ACPI_Sleep_Controller with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   subtype Microseconds is Interfaces.Unsigned_64;
   type Phase is (Inspect_Window, Transition_Method, Preparation_Method,
      Sleep_Data, Final_Check, Complete);
   type Outcome is (Not_Ready, Invalid_Window, No_Budget, Clock_Unavailable,
      Clock_Reversed, Window_Expired, Recovery_Required, No_Sleep_Data,
      Prepared, Prepared_With_Method_Failure);
   type Method_Reports is array (Methods.Method_Phase) of Methods.Preparation_Report;
   type Report is record
      Status : Outcome := Not_Ready;
      Reached : Phase := Inspect_Window;
      Charged : Natural := 0;
      Last_Clock_Microseconds : Microseconds := 0;
      Method_Failed : Boolean := False;
      First_Failure : Methods.Method_Phase := Methods.Transition_To_State;
      Method_Results : Method_Reports;
      Data_Result : Objects.Query_Report;
   end record;
   type Copied_Types is record
      Available : Boolean := False;
      Registers : Objects.Register_Model := Objects.No_Sleep_Registers;
      Type_A, Type_B : Objects.Sleep_Type := 0;
   end record;
   Empty_Types : constant Copied_Types := (others => <>);
   type State is limited private;
   procedure Invalidate (Item : in out State);
   function Current_Types (Item : State; Service : Core.State) return Copied_Types
     with Pre => Core.Valid (Service);
   -- Authorized S5 owner operation only. A new attempt invalidates any old
   -- copy FIRST. _TTS, _PTS and _S5 share Budget and the absolute deadline.
   -- Ordinary method failure may still yield a fresh valid type copy. Unsafe
   -- ownership, failed cleanup, exhausted/expired work or invalid data cannot.
   -- No register IO, IPC, source teardown or final sleep entry is performed.
   procedure Prepare_S5 (Item : in out State; Service : aliased in out Core.State;
      Registers : Objects.Register_Model; Budget : Natural;
      Deadline_Microseconds : Microseconds; Result : out Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Result.Charged <= Budget;
private
   type State is limited record
      Types : Copied_Types;
      Context : Core.Context_Key := Core.No_Context;
   end record;
end ACPI_Sleep_Controller;
