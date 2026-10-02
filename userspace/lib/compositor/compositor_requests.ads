with Interfaces;
package Compositor_Requests with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype ID is Interfaces.Unsigned_64;
   -- Shared by all completion routes. Zero/Last are never issued.
   procedure Allocate (Sequence : in out ID; Token : out ID)
     with Post =>
       (if Sequence'Old < ID'Last - 1 then
          Sequence = Sequence'Old + 1 and Token = Sequence and Token > 0 and Token < ID'Last
        else Sequence = Sequence'Old and Token = 0);
   type State is private;
   function Available (S : State) return Boolean;
   function Busy (S : State) return Boolean;
   function Faulted (S : State) return Boolean;
   function Token (S : State) return ID;
   procedure Begin_Request (S : in out State; New_Token : ID; Accepted : out Boolean)
     with Post => Accepted = (Available (S'Old) and New_Token > Token (S'Old) and New_Token < ID'Last) and
       (if Accepted then Busy (S) and Token (S) = New_Token
        else S = S'Old);
   procedure Quarantine (S : in out State)
     with Post => Faulted (S) and Token (S) = Token (S'Old);
   -- Confirmed means authenticated transport AND the operation's valid terminal
   -- reply. It is not a timeout or an assumption that a remote reader stopped.
   procedure Complete (S : in out State; Reply_Token : ID; Confirmed : Boolean)
     with Post => Token (S) = Token (S'Old) and
       Available (S) = (Busy (S'Old) and Reply_Token = Token (S'Old) and Confirmed) and
       (if not Available (S) then Faulted (S));
private
   type Phase is (Idle, Pending, Uncertain);
   type State is record
      Status : Phase := Idle;
      Last : ID := 0;
   end record;
   function Available (S : State) return Boolean is (S.Status = Idle);
   function Busy (S : State) return Boolean is (S.Status = Pending);
   function Faulted (S : State) return Boolean is (S.Status = Uncertain);
   function Token (S : State) return ID is (S.Last);
end Compositor_Requests;
