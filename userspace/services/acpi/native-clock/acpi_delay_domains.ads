with Interfaces;
-- Local time admission only. Source authentication, AML ownership and the
-- validity/epoch of the clock are obligations of the serialized native owner.
package ACPI_Delay_Domains with SPARK_Mode, Pure is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Interfaces.Unsigned_64;
   subtype Tick is Interfaces.Unsigned_64;
   type Phase is (Fresh, Startup_Active, Startup_Paused, Runtime_Idle, Event_Active, Stopped);
   type State is limited private
     with Default_Initial_Condition =>
       Model (State) = (Stage => Fresh, Until_Time => 0, Floor => 0,
                       Pending_Wait => False);
   -- Proof-only snapshot: no native alias or authority escapes.
   type Observation is record
      Stage : Phase;
      Until_Time, Floor : Tick;
      Pending_Wait : Boolean;
   end record with Ghost;
   function Model (S : State) return Observation with Ghost;
   function Current (S : State) return Phase
     with Post => Current'Result = Model (S).Stage;
   function Deadline (S : State) return Tick
     with Post => Deadline'Result = Model (S).Until_Time;
   function Working (S : State) return Boolean
     with Post => Working'Result = Model (S).Pending_Wait;
   -- Failed first admission is consumed. No operation reopens Stopped.
   procedure Start (S : in out State; Now, Until_Time : Tick;
      Clock_Available : Boolean; Accepted : out Boolean)
     with Contract_Cases =>
       (Current (S) /= Fresh =>
          not Accepted and then Model (S) = Model (S)'Old,
        Current (S) = Fresh and then Clock_Available and then Until_Time /= Tick'Last
            and then Now < Until_Time =>
          Accepted and then Model (S) = (Model (S)'Old with delta Stage => Startup_Active,
              Until_Time => Until_Time, Floor => Now),
        others =>
          not Accepted and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0));
   -- A trusted startup controller may suspend admission for a quiet IPC
   -- boundary. No wait may be outstanding. The original absolute deadline
   -- and elapsed time remain owned; neither operation replenishes them.
   -- Paused denies Inspect_Window/Begin_Wait and cannot enter runtime.
   -- Expiry, unavailable/reversed clock, or a pending wait stops admission;
   -- an outstanding wait remains owned until End_Wait returns.
   procedure Pause_Startup (S : in out State; Now : Tick;
      Clock_Available : Boolean; Accepted : out Boolean)
     with Contract_Cases =>
       (Working (S) =>
          not Accepted and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0),
        not Working (S) and then Current (S) /= Startup_Active =>
          not Accepted and then Model (S) = Model (S)'Old,
        not Working (S) and then Current (S) = Startup_Active and then Clock_Available
            and then Now >= Model (S).Floor and then Now < Deadline (S) =>
          Accepted and then Model (S) = (Model (S)'Old with delta Stage => Startup_Paused, Floor => Now),
        others =>
          not Accepted and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0));
   procedure Resume_Startup (S : in out State; Now : Tick;
      Clock_Available : Boolean; Accepted : out Boolean)
     with Contract_Cases =>
       (Working (S) =>
          not Accepted and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0),
        not Working (S) and then Current (S) /= Startup_Paused =>
          not Accepted and then Model (S) = Model (S)'Old,
        not Working (S) and then Current (S) = Startup_Paused and then Clock_Available
            and then Now >= Model (S).Floor and then Now < Deadline (S) =>
          Accepted and then Model (S) = (Model (S)'Old with delta Stage => Startup_Active, Floor => Now),
        others =>
          not Accepted and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0));
   -- Metadata observation while paused, not an I/O admission. Validates and
   -- advances the same monotonic floor; never opens/replenishes the window.
   procedure Inspect_Paused_Startup (S : in out State; Now : Tick;
      Clock_Available : Boolean; Until_Time : out Tick; Accepted : out Boolean)
     with Contract_Cases =>
       (Working (S) =>
          not Accepted and then Until_Time = 0
            and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0),
        not Working (S) and then not (Current (S) = Startup_Paused) =>
          not Accepted and then Until_Time = 0 and then Model (S) = Model (S)'Old,
        not Working (S) and then (Current (S) = Startup_Paused) and then Clock_Available
            and then Now >= Model (S).Floor and then Now < Deadline (S) =>
          Accepted and then Until_Time = Deadline (S)'Old
            and then Model (S) = (Model (S)'Old with delta Floor => Now),
        others =>
          not Accepted and then Until_Time = 0
            and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0));
   -- Successful startup may transfer to an idle runtime owner once. It must
   -- do so before closing startup and after all startup evaluation has ended.
   procedure Enter_Runtime (S : in out State; Now : Tick;
      Clock_Available : Boolean; Accepted : out Boolean)
     with Contract_Cases =>
       (Current (S) /= Startup_Active or else Working (S) =>
          not Accepted and then Model (S) = Model (S)'Old,
        Current (S) = Startup_Active and then not Working (S) and then Clock_Available
            and then Now >= Model (S).Floor and then Now < Deadline (S) =>
          Accepted and then Model (S) = (Model (S)'Old with delta Stage => Runtime_Idle,
              Until_Time => 0, Floor => Now),
        others =>
          not Accepted and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0));
   -- Only a trusted controller can admit an event. Repeated/nested admission
   -- refuses without changing the original deadline or replenishing time.
   procedure Begin_Event (S : in out State; Now, Until_Time : Tick;
      Clock_Available : Boolean; Accepted : out Boolean)
     with Contract_Cases =>
       (Current (S) /= Runtime_Idle or else Working (S) =>
          not Accepted and then Model (S) = Model (S)'Old,
        Current (S) = Runtime_Idle and then not Working (S) and then Now >= Model (S).Floor
            and then Clock_Available and then Until_Time /= Tick'Last and then Now < Until_Time =>
          Accepted and then Model (S) = (Model (S)'Old with delta Stage => Event_Active,
              Until_Time => Until_Time, Floor => Now),
        others =>
          not Accepted and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0));
   -- Observe the existing deadline without opening/replenishing a window.
   -- The trusted clock sample becomes the monotonic floor for later waits.
   -- Inspection during a pending wait stops admission and retains that wait.
   procedure Inspect_Window (S : in out State; Now : Tick;
      Clock_Available : Boolean; Until_Time : out Tick; Accepted : out Boolean)
     with Contract_Cases =>
       (Working (S) =>
          not Accepted and then Until_Time = 0
            and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0),
        not Working (S) and then not (Current (S) in Startup_Active | Event_Active) =>
          not Accepted and then Until_Time = 0 and then Model (S) = Model (S)'Old,
        not Working (S) and then (Current (S) in Startup_Active | Event_Active)
            and then Clock_Available and then Now >= Model (S).Floor and then Now < Deadline (S) =>
          Accepted and then Until_Time = Deadline (S)'Old
            and then Model (S) = (Model (S)'Old with delta Floor => Now),
        others =>
          not Accepted and then Until_Time = 0
            and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0));
   procedure Begin_Wait (S : in out State; Until_Time : out Tick;
      Accepted : out Boolean)
     with Contract_Cases =>
       (Working (S) =>
          not Accepted and then Until_Time = 0
            and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0),
        not Working (S) and then (Current (S) not in Startup_Active | Event_Active) =>
          not Accepted and then Until_Time = 0 and then Model (S) = Model (S)'Old,
        others =>
          Accepted and then Until_Time = Deadline (S)'Old
            and then Model (S) = (Model (S)'Old with delta Pending_Wait => True));
   -- Began/Last are the wait policy's first/last clock samples. A reversed
   -- first sample is rejected even if the clock catches up during the wait.
   -- A failed or reversed wait
   -- closes the whole domain, including later events; no automatic replay.
   procedure End_Wait (S : in out State; Began, Last : Tick;
      Completed : Boolean; Accepted : out Boolean)
     with Contract_Cases =>
       (not Working (S) =>
          not Accepted and then Model (S) = Model (S)'Old,
        Working (S) and then (Current (S) not in Startup_Active | Event_Active) =>
          not Accepted and then Model (S) = (Model (S)'Old with delta Pending_Wait => False),
        Working (S) and then (Current (S) in Startup_Active | Event_Active) and then Completed
            and then Began >= Model (S).Floor and then Last >= Began and then Last < Deadline (S) =>
          Accepted and then Model (S) = (Model (S)'Old with delta Pending_Wait => False, Floor => Last),
        others =>
          not Accepted
            and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0,
              Pending_Wait => False));
   procedure End_Event (S : in out State; Now : Tick;
      Clock_Available : Boolean; Accepted : out Boolean)
     with Contract_Cases =>
       (Current (S) /= Event_Active or else Working (S) =>
          not Accepted and then Model (S) = Model (S)'Old,
        Current (S) = Event_Active and then not Working (S) and then Clock_Available
            and then Now >= Model (S).Floor and then Now < Deadline (S) =>
          Accepted and then Model (S) = (Model (S)'Old with delta Stage => Runtime_Idle,
              Until_Time => 0, Floor => Now),
        others =>
          not Accepted and then Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0));
   procedure Stop (S : in out State)
     with Post => Model (S) = (Model (S)'Old with delta Stage => Stopped, Until_Time => 0);
private
   type State is limited record
      Stage : Phase := Fresh;
      Until_Time, Last : Tick := 0;
      In_Call : Boolean := False;
   end record;
end ACPI_Delay_Domains;
