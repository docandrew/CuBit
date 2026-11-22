-- Hosted test model only; never a native runtime/provider implementation.
package body Sender_Model is
 procedure Assert(OK : Boolean) is begin if not OK then raise Program_Error with "model boundary"; end if; end Assert;
 procedure Reset is begin
 Mode:=0; Calls:=0; Allocations:=0; Frees:=0; Creates:=0; Revokes:=0; Queries:=0; Clock_Reads:=0; Boundary_Reads:=0;
 Boundary:=True; Second_Boundary:=True; Confirmed:=True; Revoke_OK:=True; Create_OK:=True; Allocate_OK:=True;
 Current:=True; Read_Right:=True; Good_Kind:=True; Good_Tag:=True; Good_Peer:=True;
 Times:=[100,100,200]; Buffer:=[others=>16#A5#]; Own_ID:=16#1_0000_0007#;
 Second_Buffer:=[others=>16#A5#];Second_Retired:=False;
 First_Frees:=0;Second_Frees:=0;Offer_Calls:=[others=>0];
 end Reset;
end Sender_Model;
