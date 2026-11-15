pragma Ada_2022;
with Interfaces;
with CuBit.Power_Event_Exchange;
generic
   type Reply_Token is private;
   No_Reply : Reply_Token;
   Maximum_Count : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
   -- Trusted non-reentrant transport boundary. Saving a refusal retains the
   -- current request's reply authority. Answer consumes a saved token even
   -- when its caller disappeared. Neither callback may raise an exception.
   with procedure Save_Current (Token : out Reply_Token; Saved : out Boolean);
   with procedure Answer_Saved
     (Token : in out Reply_Token; ID : Interfaces.Unsigned_64;
      Status : CuBit.Power_Event_Exchange.Watch_Status;
      Count : Interfaces.Unsigned_64;
      Source : CuBit.Power_Event_Exchange.Press_Source; Delivered : out Boolean);
package ACPI_Power_Button_Watches with SPARK_Mode is
   package E renames CuBit.Power_Event_Exchange;
   subtype Word is Interfaces.Unsigned_64;
   subtype Actual_Source is E.Press_Source range E.Fixed_Event .. E.Intel_HID;
   type Availability is (Unobserved, Present, Absent);
   type Stop_Cause is (Not_Stopping, Owner_Stopping, Counter_Exhausted);
   type Request is record
      Authority, Length, ID, Seen, Flags, Reserved : Word := 0;
   end record;
   type Response is record
      ID : Word := 0;
      Status : E.Watch_Status := E.Watch_Unavailable;
      Count : Word := 0;
      Source : E.Press_Source := E.No_Press;
   end record;
   type Snapshot is record
      Count : Word := 0;
      Source : E.Press_Source := E.No_Press;
      Hardware : Availability := Unobserved;
      Stop : Stop_Cause := Not_Stopping;
      Kept : Boolean := False;
      Request_ID, Seen : Word := 0;
   end record;
   type Delivery is (No_Attempt, Answered, Caller_Gone);
   type State is limited private;
   function Observe (Item : State) return Snapshot with Global => null;
   function Valid (Item : State) return Boolean with Global => null;
   function Well_Formed (Input : Request) return Boolean with Global => null;
   -- These operations copy facts only. No transport, Core, AML or logging.
   procedure Set_Availability (Item : in out State; Value : Availability)
     with Pre => Valid (Item), Post => Valid (Item);
   procedure Record_Press (Item : in out State; Source : Actual_Source)
     with Pre => Valid (Item), Post => Valid (Item);
   procedure Begin_Stop (Item : in out State)
     with Pre => Valid (Item), Post => Valid (Item);
   function Retirement_Requested (Item : State) return Boolean with Global => null;
   function Runnable (Item : State) return Boolean with Global => null;
   function Quiet (Item : State) return Boolean with Global => null;
   -- Advance answers at most one kept request, only at the caller's quiet
   -- boundary. A lost delivery consumes the token and is never replayed.
   procedure Advance (Item : in out State; Owner_Quiet : Boolean; Result : out Delivery)
     with Pre => Valid (Item), Post => Valid (Item);
   -- Unauthorized/malformed requests do not change state or call transport.
   -- A save refusal leaves an older unready watch owned. A successful save
   -- obtains the new token before consuming the superseded old token.
   -- Deferred=True requires the server to skip its normal immediate reply.
   procedure Watch (Item : in out State; Input : Request; Owner_Quiet : Boolean;
      Reply : out Response; Deferred : out Boolean; Previous : out Delivery)
     with Pre => Valid (Item), Post => Valid (Item);
private
   type State is limited record
      View : Snapshot;
      Token : Reply_Token := No_Reply;
   end record;
end ACPI_Power_Button_Watches;
