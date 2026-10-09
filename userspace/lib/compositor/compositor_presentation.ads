with Interfaces;
--  Policy only. The caller authenticates kernel completions and owns mappings.
package Compositor_Presentation with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype ID is Interfaces.Unsigned_64;
   subtype Live_ID is ID range 1 .. ID'Last;
   type Phase is (Closed, Available, Prepared, In_Flight, Quarantined);
   type State is private;
   function Current (S : State) return Phase;
   function Session (S : State) return ID;
   function Token (S : State) return ID;
   function Retry_At (S : State) return ID;
   -- Millisecond clock; ID'Last is unavailable. Gate every kernel attempt,
   -- including loops awakened by an unrelated stream of input or requests.
   function Can_Attempt (S : State; Now : ID) return Boolean is
     (Current (S) = Prepared and then Now < ID'Last and then Now >= Retry_At (S));
   function Writable (S : State) return Boolean is (Current (S) = Available);
   -- Only for newly acquired storage; never resets ownership of an old mapping.
   function Open (Session_ID : Live_ID) return State
     with Post => Writable (Open'Result) and
       Session (Open'Result) = Session_ID and Token (Open'Result) = 0;
   procedure Quarantine (S : in out State)
     with Post => Current (S) = Quarantined and
       Session (S) = Session (S'Old) and Token (S) = Token (S'Old);
   procedure Prepare (S : in out State; Frame : ID; Started : out Boolean)
     with Post => Started = (Writable (S'Old) and Frame > Token (S'Old)) and
       Session (S) = Session (S'Old) and
       (if Started then Current (S) = Prepared and Token (S) = Frame
        else Current (S) = Quarantined and Token (S) = Token (S'Old)) and
       (if Started then Retry_At (S) = 0);
   -- Accepted must be the result of the one synchronous kernel submit call.
   -- False certifies no publication. Keep the prepared frame immutable for a
   -- later attempt; it is neither writable nor eligible for completion.
   procedure Submitted (S : in out State; Accepted : Boolean; Now : ID)
     with Post => Session (S) = Session (S'Old) and Token (S) = Token (S'Old) and
       Current (S) = (if Current (S'Old) = Prepared then
         (if Accepted then In_Flight else Prepared) else Quarantined) and
       (if Current (S'Old) = Prepared and not Accepted then
          Retry_At (S) = (if Now < ID'Last then Now + 1 else ID'Last));
   -- Cancellation is legal only before publication. The caller must separately
   -- retire its local pool ticket and restore captured damage before reuse.
   procedure Cancel (S : in out State)
     with Post => Session (S) = Session (S'Old) and Token (S) = Token (S'Old) and
       Current (S) = (if Current (S'Old) = Prepared then Available else Quarantined);
   type Completion is record
      Kernel_Valid, Kernel_OK, Payload_Valid : Boolean := False;
      Kernel_Token, Payload_Session, Payload_Frame : ID := 0;
      Published, Released : Boolean := False;
   end record;
   function Releases (S : State; Reply : Completion) return Boolean is
     (Current (S) = In_Flight and then
      Reply.Kernel_Valid and then Reply.Kernel_OK and then Reply.Payload_Valid and then
      Reply.Kernel_Token = Token (S) and then Reply.Payload_Frame = Token (S) and then
      Reply.Payload_Session = Session (S) and then Reply.Published and then Reply.Released);
   procedure Complete (S : in out State; Reply : Completion)
     with Post => Writable (S) = Releases (S'Old, Reply) and
       Current (S) = (if Releases (S'Old, Reply) then Available else Quarantined) and
       Session (S) = Session (S'Old) and Token (S) = Token (S'Old);
private
   type State is record
      Status : Phase := Closed;
      Session_ID, Frame_ID : ID := 0;
      Attempt_At : ID := 0;
   end record;
   function Retry_At (S : State) return ID is (S.Attempt_At);
   function Current (S : State) return Phase is (S.Status);
   function Session (S : State) return ID is (S.Session_ID);
   function Token (S : State) return ID is (S.Frame_ID);
end Compositor_Presentation;
