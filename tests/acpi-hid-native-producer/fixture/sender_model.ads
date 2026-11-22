-- Hosted test model only; never a native runtime/provider implementation.
with Interfaces; with HID_Offer_Payload;
package Sender_Model is
 use Interfaces;
 Mode, Calls, Allocations, Frees, Creates, Revokes, Queries, Clock_Reads, Boundary_Reads : Natural := 0;
 Boundary, Second_Boundary, Confirmed, Revoke_OK, Create_OK, Allocate_OK : Boolean := True;
 Current, Read_Right, Good_Kind, Good_Tag, Good_Peer : Boolean := True;
 Times : array(Positive range 1 .. 3) of Unsigned_64 := [100,100,200];
 Own_ID : Unsigned_64 := 16#1_0000_0007#;
 Buffer : aliased HID_Offer_Payload.Byte_Array(0 .. 4095) := [others=>16#A5#] with Alignment=>4096;
 Second_Buffer : aliased HID_Offer_Payload.Byte_Array(0 .. 4095) := [others=>16#A5#] with Alignment=>4096;
 Second_Retired : Boolean := False;
 First_Frees,Second_Frees : Natural := 0;
 Offer_Calls : array(Positive range 1..2) of Natural := [others=>0];
 procedure Reset;
 procedure Assert(OK : Boolean);
end Sender_Model;
