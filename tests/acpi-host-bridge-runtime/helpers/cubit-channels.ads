with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Control_Events;
with CuBit.Grant_References;
with CuBit.Messages;
package CuBit.Channels is
   package CC renames CuBit.Channel_Contracts;
   package CP renames CuBit.Channel_Protocol;
   package GR renames CuBit.Grant_References;
   type Side is (Producing, Consuming);
   type Channel is record
      Active, Opener : Boolean := False;
      Item : CC.Contract;
      This_Side : Side := Producing;
      Own_Base, Own_Bytes, Peer_Base : Unsigned_64 := 0;
      Own_Grant, Peer_Grant, Closed_Own_Grant : GR.Reference;
      Has_Own, Has_Peer, Closed_Own_Present : Boolean := False;
   end record;
   type Open_Result is (Opened, Refused, Failed);
   type Signal_Result is (Signalled, No_Region, Ended);
   type Model_Mode is (Good, Refuse_Open, Fail_Open, Bad_Alignment, Bad_Contract,
      Missing_Peer, Bad_Own_Length, Aliased_Regions, Overlapping_Regions);
   Mode : Model_Mode := Good;
   Own_Base, Peer_Base : Unsigned_64 := 0;
   Own_Reference : constant GR.Reference := (slot => 7, generation => 19);
   Peer_Reference : constant GR.Reference := (slot => 9, generation => 21);
   Next_Own_Reference : GR.Reference := Own_Reference;
   Next_Peer_Reference : GR.Reference := Peer_Reference;
   Open_Calls, Close_Calls, Signals : Natural := 0;
   Last_Deadline : Unsigned_64 := 0;
   Last_Sync : Boolean := True;
   Signal_Answer : Signal_Result := Signalled;
   Last_Signalled : GR.Reference;
   Submit_OK : Boolean := True;
   Finish_Calls : Natural := 0;
   Last_Token : Unsigned_64 := 0;
   procedure Begin_Open
     (Endpoint : CuBit.Handles.Endpoint; Item : CC.Contract; This_Side : Side;
      Token : Unsigned_64; C : out Channel; Submitted : out Boolean;
      Connector : Unsigned_16 := 0);
   procedure Finish_Open
     (C : in out Channel; Reply : CuBit.Messages.Message;
      Result : out Open_Result; Refusal : out CP.Open_Refusal);
   procedure Open (Endpoint : CuBit.Handles.Endpoint; Item : CC.Contract; This_Side : Side;
      C : out Channel; Result : out Open_Result; Refusal : out CP.Open_Refusal;
      Connector : Unsigned_16 := 0; Deadline : Unsigned_64 := Unsigned_64'Last);
   procedure Signal (C : Channel; Sync : Boolean; Result : out Signal_Result);
   function Ended (C : Channel; Event : CuBit.Control_Events.Event) return Boolean;
   procedure Close (C : in out Channel);
end CuBit.Channels;
