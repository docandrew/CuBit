pragma Ada_2022;
with Interfaces;
with GPIO_Loop_Time;
generic
   with procedure Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
   with procedure Start_Sweep (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean);
   with procedure Advance;
   -- Source_Clear means each admitted source observed by this sweep is
   -- confirmed quiet or masked with its status cleared. A retained client
   -- ticket need not be delivered/rearmed. Uncertain effects are not clear.
   with procedure Inspect (Source_Clear, Uncertain : out Boolean);
   with procedure Acknowledge (Accepted : out Boolean);
   Sweep_Window, Progress_Interval : GPIO_Loop_Time.Duration;
package GPIO_IRQ_Turn with SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   Last_Finite : constant Word := Word'Last - 1;
   type Phase is (Idle, Sweep_Pending, Sweeping, Faulted);
   type Failure is (None, No_Clock, Clock_Reversed, Clock_Exhausted,
      Deadline_Expired, Sweep_Refused, Source_Uncertain, Acknowledge_Refused);
   type Report is record
      At_Phase : Phase := Idle;
      Reason : Failure := None;
      Deadline, Wake_At : Word := 0;
      Needs_Progress, Parent_Held : Boolean := False;
   end record;
   type State is private;
   procedure Note (S : in out State; Authenticated_Parent : Boolean);
   -- Native queue metadata and the manifest-installed line must authenticate
   -- the notice before True. Payload addresses/IDs never establish authority.
   procedure Turn (S : in out State);
   function Observe (S : State) return Report;
   -- One serialized owner, bounded/nonraising/nonreentrant callbacks. A sweep
   -- deadline is sampled once; repeated notices cannot renew it. Exactly one
   -- parent acknowledgement follows a completed sweep (or coalesced sweeps).
   -- Failure retains the parent obligation for supervision, never retries ACK.
   -- This owner does not poll pins, execute client ticket ACKs, or release MMIO.
private
   type State is record
      View : Report;
      Seen_Clock, Sweep_Again : Boolean := False;
      Previous : Word := 0;
   end record;
end GPIO_IRQ_Turn;
