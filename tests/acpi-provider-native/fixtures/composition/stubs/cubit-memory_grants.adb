pragma Ada_2022;
with Firmware_Tables;
with Bridge_Trace; use Bridge_Trace;
package body CuBit.Memory_Grants is
 use type System.Address;
 use type Firmware_Tables.Bytes;
 procedure Check_Image is
 begin
  if Active and then Buffer(1..Grant_Pages*4096)/=Grant_Image(1..Grant_Pages*4096) then
   raise Program_Error with "grant bytes changed before retirement";
  end if;
 end Check_Image;
 use type CuBit.Grant_References.Reference;
 procedure Create_Via_Capability(slot : CuBit.Handles.Endpoint;
  localAddr : System.Address; numPages : Natural; readWrite : Boolean;
  reference : out Grant_Reference; success : out Boolean; notify : Boolean := False) is
 begin
  if not CuBit.Handles.Is_Service(slot) or else localAddr/=Buffer'Address or else
    numPages not in 1..2 or else Unsigned_64(numPages)*PAGE_SIZE>Allocated_Bytes or else readWrite or else notify or else Active or else Acquired
  then raise Program_Error with "invalid grant creation/reuse"; end if;
  Note(Grant_Create); Creates:=Creates+1;
  Grant_Pages:=numPages; Grant_Image:=Buffer;
  success:=Created_OK;
  if success then
   Bridge_Trace.Reference:=(7,Unsigned_64(Creates)+8);
   reference:=Bridge_Trace.Reference; Active:=True; Revoked:=False;
  else reference:=(0,1);
  end if;
 end Create_Via_Capability;
 procedure Acquire_Via_Capability(owner : CuBit.Handles.Endpoint;
  reference : Grant_Reference; byteOffset,byteLength : Unsigned_64;
  accessMode : Required_Access; localAddr : out System.Address; success : out Boolean) is
 begin
  Check_Image;
  Note(Grant_Acquire); Acquires:=Acquires+1;
  success:=CuBit.Handles.Is_Provider(owner) and then Active and then not Revoked
    and then not Acquired and then reference=Bridge_Trace.Reference
    and then byteOffset=0 and then byteLength in 36..Unsigned_64(Grant_Pages)*PAGE_SIZE and then
    (byteLength+PAGE_SIZE-1)/PAGE_SIZE=Unsigned_64(Grant_Pages) and then accessMode=Read_Access;
  localAddr:=System.Null_Address;
  if success then
   Acquired:=True; localAddr:=Buffer'Address;
   if not (for all I in Natural(byteLength)+1..Grant_Pages*4096 => Buffer(I)=0) then
    raise Program_Error with "nonzero granted padding";
   end if;
  end if;
 end Acquire_Via_Capability;
 procedure Return_Acquisition(reference : Grant_Reference; success : out Boolean) is
 begin
  if reference/=Bridge_Trace.Reference or else not Acquired then raise Program_Error; end if;
  Check_Image;
  Note(Grant_Return); Returns:=Returns+1; success:=Allow_Return;
  if success then Acquired:=False; end if;
 end Return_Acquisition;
 procedure Revoke(reference : Grant_Reference; success : out Boolean) is
 begin
  if reference/=Bridge_Trace.Reference then raise Program_Error with "foreign/default revoke"; end if;
  Check_Image;
  Note(Grant_Revoke); Revokes:=Revokes+1; success:=Active and then not Revoked;
  Revoked:=True;
 end Revoke;
 function Retirement_Confirmed(reference : Grant_Reference) return Boolean is
 begin
  if reference/=Bridge_Trace.Reference then raise Program_Error with "foreign/default query"; end if;
  Check_Image;
  Note(Retirement_Query); Queries:=Queries+1;
  if Revoked and then not Acquired then Active:=False; return True; end if;
  return False;
 end Retirement_Confirmed;
end CuBit.Memory_Grants;
