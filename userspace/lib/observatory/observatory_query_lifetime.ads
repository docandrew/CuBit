with Interfaces; use Interfaces;
with Compositor_Requests;
package Observatory_Query_Lifetime with SPARK_Mode is
   type Phase is (Idle, Waiting, Retiring, Ready, Failed);
   type State is private;
   Timeout_Us : constant Unsigned_64 := 250_000;
   function Status (S : State) return Phase;
   function Token (S : State) return Unsigned_64;
   function Deadline (S : State) return Unsigned_64;
   procedure Start (S : in out State; New_Token, Now : Unsigned_64; Accepted : out Boolean)
     with Post =>
       (if Accepted then Status (S) = Waiting and Token (S) = New_Token
        else S = S'Old);
   -- Wrong/stale tokens cannot modify the live query. Invalid matching replies
   -- disable it; a valid envelope still needs independent grant retirement.
   procedure Receive (S : in out State; Reply_Token : Unsigned_64; Valid : Boolean)
     with Post =>
       (if Status (S'Old) /= Waiting or Reply_Token /= Token (S'Old) then S = S'Old
        elsif Valid then Status (S) = Retiring
        else Status (S) = Failed);
   procedure Retired (S : in out State; Confirmed : Boolean)
     with Post =>
       (if Status (S'Old) = Retiring and Confirmed then Status (S) = Ready
        else S = S'Old);
   procedure Expire (S : in out State; Now : Unsigned_64)
     with Post =>
       (if Status (S'Old) in Waiting | Retiring and Now >= Deadline (S'Old)
        then Status (S) = Failed else S = S'Old);
   procedure Fail (S : in out State)
     with Post => Status (S) = Failed and Token (S) = Token (S'Old);
   procedure Consume (S : in out State)
     with Post => (if Status (S'Old) = Ready then Status (S) = Idle else S = S'Old);
private
   type State is record
      Mode : Phase := Idle;
      Flight : Compositor_Requests.State;
      Due : Unsigned_64 := 0;
   end record;
   function Status (S : State) return Phase is (S.Mode);
   function Token (S : State) return Unsigned_64 is (Compositor_Requests.Token (S.Flight));
   function Deadline (S : State) return Unsigned_64 is (S.Due);
end Observatory_Query_Lifetime;
