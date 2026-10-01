with Interfaces;
--  Policy only. The caller authenticates kernel completions and owns mappings.
package Compositor_Presentation with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype ID is Interfaces.Unsigned_64;
   subtype Live_ID is ID range 1 .. ID'Last;
   type Phase is (Closed, Available, In_Flight, Quarantined);
   type State is private;
   function Current (S : State) return Phase;
   function Session (S : State) return ID;
   function Token (S : State) return ID;
   function Writable (S : State) return Boolean is (Current (S) = Available);
   -- Only for newly acquired storage; never resets ownership of an old mapping.
   function Open (Session_ID : Live_ID) return State
     with Post => Writable (Open'Result) and
       Session (Open'Result) = Session_ID and Token (Open'Result) = 0;
   procedure Quarantine (S : in out State)
     with Post => Current (S) = Quarantined and
       Session (S) = Session (S'Old) and Token (S) = Token (S'Old);
   procedure Submit (S : in out State; Frame : ID; Started : out Boolean)
     with Post => Started = (Writable (S'Old) and Frame > Token (S'Old)) and
       Session (S) = Session (S'Old) and
       (if Started then Current (S) = In_Flight and Token (S) = Frame
        else Current (S) = Quarantined and Token (S) = Token (S'Old));
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
   end record;
   function Current (S : State) return Phase is (S.Status);
   function Session (S : State) return ID is (S.Session_ID);
   function Token (S : State) return ID is (S.Frame_ID);
end Compositor_Presentation;
