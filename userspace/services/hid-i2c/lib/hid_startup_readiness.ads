pragma Ada_2022;
with I2C_Transfer_Wire;
package HID_Startup_Readiness with SPARK_Mode is
   package W renames I2C_Transfer_Wire;
   use type W.Word;
   type Choice is (No_Report, Report_Ready, Report_Declined);
   subtype Outgoing is Choice range Report_Ready .. Report_Declined;
   type State is private;
   function Selected (S : State) return Choice;
   function Attempted (S : State) return Boolean;
   function Receipt_Acknowledged (S : State) return Boolean;
   procedure Decide (S : in out State; Now : W.Word; Limit : W.Millisecond;
      Clock_Available, Device_Ready, Stopping : Boolean; Send : out Choice)
   with Global => null, Always_Terminates,
     Post =>
       Attempted (S) = Attempted (S'Old) and then
       Receipt_Acknowledged (S) = Receipt_Acknowledged (S'Old) and then
       (if Selected (S'Old) /= No_Report then
           Send = No_Report and then Selected (S) = Selected (S'Old)
        else
           Selected (S) = Send and then
           Send = (if not Clock_Available or else Stopping or else Now >= Limit then Report_Declined
                   elsif Device_Ready then Report_Ready else No_Report));
   -- Latch before the transport call. Clock failure, stop and expiry beat
   -- readiness; subsequent observations never replay/replace a selected report.
   procedure Delivery (S : in out State; Acknowledged : Boolean)
      with Global => null, Always_Terminates,
        Pre => Selected (S) /= No_Report,
        Post => Selected (S) = Selected (S'Old) and then Attempted (S) and then
          Receipt_Acknowledged (S) =
            (if Attempted (S'Old) then Receipt_Acknowledged (S'Old) else Acknowledged);
   -- Records only the first attempt's outcome. False includes unknown delivery,
   -- not evidence that the launcher ignored the report or cancelled anything.
   function Must_Stop (S : State) return Boolean
      with Global => null,
        Post => Must_Stop'Result = (Selected (S) = Report_Declined or else
          (Attempted (S) and then not Receipt_Acknowledged (S)));
   function Confirmed_Ready (S : State) return Boolean
      with Global => null,
        Post => Confirmed_Ready'Result = (Selected (S) = Report_Ready and then
          Attempted (S) and then Receipt_Acknowledged (S));
   -- Startup acknowledgement only, not a continuing device-health guarantee.
   function Wake_Deadline (S : State; Limit : W.Millisecond; Otherwise : W.Word)
      return W.Word
      with Global => null,
        Post => Wake_Deadline'Result =
          (if Selected (S) = No_Report then W.Word'Min (Limit, Otherwise) else Otherwise);
private
   type State is record
      Reported : Choice := No_Report;
      Attempt_Completed, Was_Acknowledged : Boolean := False;
   end record;
   function Selected (S : State) return Choice is (S.Reported);
   function Attempted (S : State) return Boolean is (S.Attempt_Completed);
   function Receipt_Acknowledged (S : State) return Boolean is (S.Was_Acknowledged);
end HID_Startup_Readiness;
