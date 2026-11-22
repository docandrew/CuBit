package body ACPI_Delay_Domains with SPARK_Mode is
   function Model (S : State) return Observation is
     ((Stage => S.Stage, Until_Time => S.Until_Time, Floor => S.Last,
       Pending_Wait => S.In_Call));
   function Current (S : State) return Phase is (S.Stage);
   function Deadline (S : State) return Tick is (S.Until_Time);
   function Working (S : State) return Boolean is (S.In_Call);
   procedure Stop (S : in out State) is
   begin
      S.Stage := Stopped;
      S.Until_Time := 0;
      -- Keep In_Call until the outstanding callback returns. Stop is neither
      -- cancellation nor permission to release its backing storage.
   end Stop;
   function Valid_Window (Now, Until_Time : Tick; Available : Boolean) return Boolean is
     (Available and then Until_Time /= Tick'Last and then Now < Until_Time);
   function Current_Sample (S : State; Now : Tick; Available : Boolean) return Boolean is
     (Available and then Now >= S.Last and then Now < S.Until_Time);
   procedure Start (S : in out State; Now, Until_Time : Tick;
      Clock_Available : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Stage /= Fresh then return; end if;
      Stop (S);
      if not Valid_Window (Now, Until_Time, Clock_Available) then return; end if;
      S.Stage := Startup_Active; S.Last := Now; S.Until_Time := Until_Time;
      Accepted := True;
   end Start;
   procedure Pause_Startup (S : in out State; Now : Tick;
      Clock_Available : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.In_Call then Stop (S); return; end if;
      if S.Stage /= Startup_Active then return; end if;
      if not Current_Sample (S, Now, Clock_Available) then Stop (S); return; end if;
      S.Stage := Startup_Paused; S.Last := Now; Accepted := True;
   end Pause_Startup;
   procedure Resume_Startup (S : in out State; Now : Tick;
      Clock_Available : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.In_Call then Stop (S); return; end if;
      if S.Stage /= Startup_Paused then return; end if;
      if not Current_Sample (S, Now, Clock_Available) then Stop (S); return; end if;
      S.Stage := Startup_Active; S.Last := Now; Accepted := True;
   end Resume_Startup;
   procedure Inspect_Paused_Startup (S : in out State; Now : Tick;
      Clock_Available : Boolean; Until_Time : out Tick; Accepted : out Boolean) is
   begin
      Until_Time := 0; Accepted := False;
      if S.In_Call then Stop (S); return; end if;
      if S.Stage /= Startup_Paused then return; end if;
      if not Current_Sample (S, Now, Clock_Available) then Stop (S); return; end if;
      S.Last := Now; Until_Time := S.Until_Time; Accepted := True;
   end Inspect_Paused_Startup;
   procedure Enter_Runtime (S : in out State; Now : Tick;
      Clock_Available : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Stage /= Startup_Active or else S.In_Call then return; end if;
      if not Current_Sample (S, Now, Clock_Available) then Stop (S); return; end if;
      S.Stage := Runtime_Idle; S.Last := Now; S.Until_Time := 0;
      Accepted := True;
   end Enter_Runtime;
   procedure Begin_Event (S : in out State; Now, Until_Time : Tick;
      Clock_Available : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Stage /= Runtime_Idle or else S.In_Call then return; end if;
      if Now < S.Last or else not Valid_Window (Now, Until_Time, Clock_Available) then
         Stop (S); return;
      end if;
      S.Stage := Event_Active; S.Last := Now; S.Until_Time := Until_Time;
      Accepted := True;
   end Begin_Event;
   procedure Inspect_Window (S : in out State; Now : Tick;
      Clock_Available : Boolean; Until_Time : out Tick; Accepted : out Boolean) is
   begin
      Until_Time := 0; Accepted := False;
      if S.In_Call then Stop (S); return; end if;
      if S.Stage not in Startup_Active | Event_Active then return; end if;
      if not Current_Sample (S, Now, Clock_Available) then Stop (S); return; end if;
      S.Last := Now; Until_Time := S.Until_Time; Accepted := True;
   end Inspect_Window;
   procedure Begin_Wait (S : in out State; Until_Time : out Tick;
      Accepted : out Boolean) is
   begin
      Until_Time := 0; Accepted := False;
      if S.In_Call then Stop (S); return; end if;
      if S.Stage not in Startup_Active | Event_Active then return; end if;
      S.In_Call := True; Until_Time := S.Until_Time; Accepted := True;
   end Begin_Wait;
   procedure End_Wait (S : in out State; Began, Last : Tick;
      Completed : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not S.In_Call then return; end if;
      S.In_Call := False;
      if S.Stage not in Startup_Active | Event_Active then return; end if;
      if not Completed or else Began < S.Last or else Last < Began
        or else not Current_Sample (S, Last, True) then
         Stop (S); return;
      end if;
      S.Last := Last; Accepted := True;
   end End_Wait;
   procedure End_Event (S : in out State; Now : Tick;
      Clock_Available : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Stage /= Event_Active or else S.In_Call then return; end if;
      -- Closing an idle event is cleanup, not admission to another effect.
      -- Expiry never extends the original window. Unknown/reversed clocks
      -- and an outstanding wait still refuse cleanup.
      if not Clock_Available or else Now < S.Last or else Now = Tick'Last then
         Stop (S); return;
      end if;
      S.Stage := Runtime_Idle; S.Last := Now; S.Until_Time := 0;
      Accepted := True;
   end End_Event;
end ACPI_Delay_Domains;
