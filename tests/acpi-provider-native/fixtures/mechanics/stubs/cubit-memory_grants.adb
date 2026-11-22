pragma Ada_2022;
with Bridge_Trace; use Bridge_Trace;
package body CuBit.Memory_Grants is
 use type System.Address;
 procedure Create_Via_Capability(slot : CuBit.Handles.Endpoint;
  localAddr : System.Address; numPages : Natural; readWrite : Boolean;
  reference : out Grant_Reference; success : out Boolean; notify : Boolean := False) is
 begin
  if not CuBit.Handles.Valid(slot) or else localAddr/=Buffer'Address or else notify then raise Program_Error; end if;
  Note(Grant_Create); Creates:=Creates+1; Last_Read_Write:=readWrite; Last_Pages:=numPages;
  Hidden_Live:=Hidden_Creation;
  success:=Created_OK;
  reference:=(if Created_OK then (7,9) else (0,1));
 end Create_Via_Capability;
 procedure Revoke(reference : Grant_Reference; success : out Boolean) is
 begin
  if reference.slot/=7 or else reference.generation/=9 then raise Program_Error; end if;
  Note(Grant_Revoke); Revokes:=Revokes+1; success:=Revoked_OK;
 end Revoke;
 function Retirement_Confirmed(reference : Grant_Reference) return Boolean is
 begin
  if reference.slot/=7 or else reference.generation/=9 then raise Program_Error; end if;
  Note(Retirement_Query); Queries:=Queries+1; return Retired;
 end Retirement_Confirmed;
end CuBit.Memory_Grants;
