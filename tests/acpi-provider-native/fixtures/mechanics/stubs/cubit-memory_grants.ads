pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Handles;
with CuBit.Grant_References;
package CuBit.Memory_Grants is
 PAGE_SIZE : constant Unsigned_64 := 4096;
 subtype Grant_Reference is CuBit.Grant_References.Reference;
 procedure Create_Via_Capability(slot : CuBit.Handles.Endpoint;
  localAddr : System.Address; numPages : Natural; readWrite : Boolean;
  reference : out Grant_Reference; success : out Boolean; notify : Boolean := False);
 procedure Revoke(reference : Grant_Reference; success : out Boolean);
 function Retirement_Confirmed(reference : Grant_Reference) return Boolean;
end CuBit.Memory_Grants;
