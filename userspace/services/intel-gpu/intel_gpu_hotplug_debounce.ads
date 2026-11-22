with Interfaces; use Interfaces;
with Intel_GPU_Port_Detection;
--  DISP-009: when a hot-plug interrupt's port is re-detected. i915 v6.16
--  intel_hotplug.c: an HPD pulse queues detection of its pin; a pin that
--  pulses more than HPD_STORM_DEFAULT_THRESHOLD (50) times within
--  HPD_STORM_DETECT_PERIOD (1000 ms) is a storm: its interrupt is masked
--  and re-enabled after HPD_STORM_REENABLE_DELAY (two minutes); an HDMI
--  sink whose EDID is not readable yet right after connecting is retried
--  once after HPD_RETRY_DELAY (1000 ms). CuBit adds a short settle time so
--  a connector's bouncing contacts give one detection, not several.
--  Pure state machine; the caller passes monotonic milliseconds.
package Intel_GPU_Hotplug_Debounce with SPARK_Mode, Pure is
   package PD renames Intel_GPU_Port_Detection;
   use type PD.Port;
   subtype Milliseconds is Unsigned_64;
   Settle_Ms : constant Milliseconds := 100;
   Retry_Ms : constant Milliseconds := 1_000;
   Storm_Period_Ms : constant Milliseconds := 1_000;
   Storm_Threshold : constant := 50;
   Storm_Reenable_Ms : constant Milliseconds := 120_000;
   --  Never: no time is due.
   Never : constant Milliseconds := Milliseconds'Last;

   subtype Pulse_Count is Natural range 0 .. Storm_Threshold + 1;
   type Pin_State is record
      --  The detection due for this port (Never: none).
      Due : Milliseconds := Never;
      --  A retry is allowed (one per connect).
      Retry_Left : Boolean := True;
      --  Pulses in the current storm period, which began at Period_Start.
      Pulses : Pulse_Count := 0;
      Period_Start : Milliseconds := 0;
      --  A storm masked this pin until Masked_Until (Never: not masked).
      Masked_Until : Milliseconds := Never;
   end record;
   type State is array (PD.Port) of Pin_State;
   Initial : constant State := [others => <>];

   function Storming (Item : Pin_State) return Boolean is (Item.Masked_Until /= Never);

   --  An interrupt for Port at Now. Unless the pin storms, its detection is
   --  due Settle_Ms after the last pulse (a burst gives one detection). The
   --  pulse that crosses the threshold masks the pin (Masked: the caller
   --  masks its interrupt).
   procedure Pulse (Item : in out State; Port : PD.Port; Now : Milliseconds; Masked : out Boolean)
     with Pre => Now < Never - Storm_Reenable_Ms,
          Post => (if Masked then Storming (Item (Port)) and then Item (Port).Due = Never) and then
                  (if not Storming (Item (Port)) then Item (Port).Due = Now + Settle_Ms) and then
                  (for all P in PD.Port => (if P /= Port then Item (P) = Item'Old (P)));

   --  The ports whose detection is due at Now; they are cleared (Due :=
   --  Never). A due port is run once, whatever number of pulses led to it.
   type Port_Set is array (PD.Port) of Boolean;
   procedure Take_Due (Item : in out State; Now : Milliseconds; Due : out Port_Set)
     with Post => (for all P in PD.Port =>
                     Due (P) = (Item'Old (P).Due /= Never and then Item'Old (P).Due <= Now) and then
                     (if Due (P) then Item (P).Due = Never else Item (P).Due = Item'Old (P).Due));

   --  After a detection of Port: a sink whose EDID did not read gets one
   --  retry Retry_Ms later; a settled result restores the retry.
   procedure Detected
     (Item : in out State; Port : PD.Port; Now : Milliseconds; Sink_Without_EDID : Boolean)
     with Pre => Now < Never - Retry_Ms;

   --  Storm-masked pins whose re-enable time has come (the caller unmasks
   --  them and re-detects: i915 does both).
   procedure Take_Reenabled (Item : in out State; Now : Milliseconds; Ready : out Port_Set)
     with Post => (for all P in PD.Port =>
                     Ready (P) = (Item'Old (P).Masked_Until /= Never and then Item'Old (P).Masked_Until <= Now));

   --  The earliest time something is due (Never: nothing).
   function Next_Wake (Item : State) return Milliseconds;
end Intel_GPU_Hotplug_Debounce;
