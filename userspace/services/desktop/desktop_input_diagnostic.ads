with Interfaces;
-- Observational scalars only. Never an input to routing or lifetime policy.
package Desktop_Input_Diagnostic with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   type Age_Kind is (No_Time, Backward, OK);
   type Rejection is (Malformed, Delivery, Replayed, Table_Full);
   type State is record
      Raw_Key, Raw_Pointer, Legacy_Key, Legacy_Pointer : Word := 0;
      Invalid_Wire, Bad_Delivery, Duplicate, Full : Word := 0;
      Payload_Buttons, Snapshot_Buttons, Seat_Buttons : Word := 0;
      Sources, Gaps, Drops : Word := 0;
      Drops_Valid : Boolean := False;
      Last_Age, Max_Age : Word := 0;
      Age_Status : Age_Kind := No_Time;
   end record;
   procedure Observe_Raw (S : in out State; Is_Source : Boolean;
      Label : Interfaces.Unsigned_32; Header, Payload, Snapshot, Now_Us : Word);
   procedure Reject (S : in out State; Why : Rejection);
   procedure New_Source (S : in out State);
   procedure Accepted (S : in out State; Seat : Word; Gap : Boolean);
   procedure Observe_Drops (S : in out State; Value : Word);
end Desktop_Input_Diagnostic;
