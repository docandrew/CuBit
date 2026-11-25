pragma Ada_2022;
package body AML_Declaration_Method_Adapter with SPARK_Mode is
   procedure Forward
  (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
   Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte;
   Width : AML_Decode.Integer_Width; Code : AML_Decode.Bytes;
   Status : out Declaration_Status)
  with SPARK_Mode
is
begin
   if not Valid (Environment, Lease)
     or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease)
   then
      Status := Unsupported_Status;
      return;
   end if;
   Define_Method (Environment, Scope, Path, Flags, Width, Code, Status);
   end Forward;

end AML_Declaration_Method_Adapter;
