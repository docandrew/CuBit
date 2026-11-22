pragma Ada_2022;
with Interfaces;
with ACPI_Requests;
-- Transport-free broker policy. The owner keeps this limited state for the
-- entire completion-routing lifetime; never replace it while replies can arrive.
-- Reconnect/reset must call Start_Session, even when the endpoint handle is reused.
-- Accept_Reply is called only by the trusted transport with its original token,
-- never with correlation reconstructed from message payload or a PID.
generic
   Maximum_Path : Positive := 32;
   Final_Session : Interfaces.Unsigned_64 := ACPI_Requests.Max_Revision;
   Final_Request : Interfaces.Unsigned_64 := ACPI_Requests.Max_Revision;
package ACPI_Notification_Consumer with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   subtype Word is Interfaces.Unsigned_64;
   subtype Receipt is ACPI_Requests.Receipt_Number;
   subtype Path_Count is Natural range 0 .. Maximum_Path;
   type Segment_Array is array (Positive range 1 .. Maximum_Path) of Interfaces.Unsigned_32;
   type Observation is record
      ID : Receipt := 0;
      Kind : ACPI_Requests.Notification_Kind_Code := ACPI_Requests.Notify_Device;
      Class : ACPI_Requests.Notification_Class_Code := ACPI_Requests.Notify_System_Class;
      Number : Interfaces.Unsigned_16 := 0;
      Node, Incarnation : ACPI_Requests.Revision_Number := 0;
      Current_At_Claim : Boolean := False;
      Length : Path_Count := 0;
      Path : Segment_Array := [others => 0];
   end record;
   type Token is private;
   No_Token : constant Token;
   type Result is (Ready, Idle, Applied, Empty, Ignored, Invalid_Reply,
                   Remote_Error, Counter_Exhausted, Invalid_Delivery);
   type State is limited private;
   procedure Start_Session (S : in out State; Outcome : out Result);
   function Session (S : State) return Word;
   function Request_Count (S : State) return Word;
   function Has_Observation (S : State) return Boolean;
   function Copy_Observation (S : State) return Observation with Pre => Has_Observation (S);
   function Last_Remote_Error (S : State) return ACPI_Requests.Outcome;
   procedure Next_Request (S : in out State; Request : out ACPI_Requests.Packet;
                           Correlation : out Token; Outcome : out Result);
   -- Malformed replies stop this session without sending ACK. A valid error is
   -- classified separately; Stale refreshes revision while preserving pending ACK.
   procedure Accept_Reply (S : in out State; Correlation : Token;
                           Reply : ACPI_Requests.Packet; Outcome : out Result);
   procedure Transport_Unknown (S : in out State; Correlation : Token; Outcome : out Result);
   procedure Confirm_Delivery (S : in out State; Lifetime : Word;
                               ID : Receipt; Outcome : out Result);
private
   type Token is record
      Lifetime, Sequence : Word := 0;
   end record;
   No_Token : constant Token := (0, 0);
   type Phase is (Stopped, Synchronize, Claim, Header, Identity, Path, Deliver, Acknowledge);
   type State is limited record
      Lifetime, Sequence : Word := 0;
      Exhausted : Boolean := False;
      Pending : Token := No_Token;
      Step : Phase := Stopped;
      Ack_Confirmed : Boolean := False;
      Revision : ACPI_Requests.Revision_Number := 0;
      Item : Observation;
      Next_Segment : Path_Count := 0;
      Error : ACPI_Requests.Outcome := ACPI_Requests.OK;
   end record;
   function Session (S : State) return Word is (S.Lifetime);
   function Request_Count (S : State) return Word is (S.Sequence);
   function Has_Observation (S : State) return Boolean is (S.Step = Deliver);
   function Copy_Observation (S : State) return Observation is (S.Item);
   function Last_Remote_Error (S : State) return ACPI_Requests.Outcome is (S.Error);
end ACPI_Notification_Consumer;
