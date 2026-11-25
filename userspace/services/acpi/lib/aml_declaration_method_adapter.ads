pragma Ada_2022;
with AML_Decode;
with AML_Names;
with AML_Declaration_Contexts;
-- Preserve the existing owner lease check before forwarding a method declaration.
-- Callback effects are inferred from actuals; this adapter adds no admission contract.
generic
   type Context (<>) is limited private;
   type Declaration_Status is private;
   Unsupported_Status : Declaration_Status;
   with function Valid (Environment : Context; Lease : AML_Declaration_Contexts.Lease)
     return Boolean;
   with procedure Define_Method
     (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
      Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
      Code : AML_Decode.Bytes; Status : out Declaration_Status);
package AML_Declaration_Method_Adapter with SPARK_Mode, Pure is
   procedure Forward
  (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
   Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte;
   Width : AML_Decode.Integer_Width; Code : AML_Decode.Bytes;
   Status : out Declaration_Status)
;

end AML_Declaration_Method_Adapter;
