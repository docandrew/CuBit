with Interfaces;
package Compositor_Lease_Request with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype ID is Interfaces.Unsigned_64;
   type Phase is (Ready, Submitting, Pending, Released, Quarantined);
   type State is private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Status (S : State) return Phase;
   function Token (S : State) return ID;
   -- Tokens come from Desktop's shared non-reusing request sequence.
   procedure Prepare (S : in out State; New_Token : ID; Prepared : out Boolean)
     with Pre => Valid (S),
       Post => Valid (S) and Prepared =
         (Status (S'Old) = Ready and New_Token > Token (S'Old) and New_Token < ID'Last)
         and (if Prepared then Status (S) = Submitting and Token (S) = New_Token
              else S = S'Old);
   -- False is trusted kernel evidence that capSubmit published no request.
   -- It permits another bounded attempt, never buffer reclamation.
   procedure Submitted (S : in out State; Accepted : Boolean)
     with Pre => Valid (S) and Status (S) = Submitting,
       Post => Valid (S) and Token (S) = Token (S'Old) and
         Status (S) = (if Accepted then Pending else Ready);
   -- Confirmed includes exact kernel token/status and successful wire reply.
   procedure Complete (S : in out State; Reply_Token : ID; Confirmed : Boolean)
     with Pre => Valid (S),
       Post => Valid (S) and Token (S) = Token (S'Old) and
         Status (S) = (if Status (S'Old) = Pending and Reply_Token = Token (S'Old)
                         and Confirmed then Released else Quarantined);
   procedure Quarantine (S : in out State)
     with Post => Valid (S) and Status (S) = Quarantined and Token (S) = Token (S'Old);
private
   type State is record
      Stage : Phase := Ready;
      Last : ID := 0;
   end record;
   function Status (S : State) return Phase is (S.Stage);
   function Token (S : State) return ID is (S.Last);
   function Valid (S : State) return Boolean is
     (if S.Stage in Submitting | Pending | Released then S.Last > 0 and S.Last < ID'Last);
end Compositor_Lease_Request;
