with CBOR;

-- Hosted codec experiment, NOT the adopted remote-control protocol.
-- Array: [version, request-id, kind, payload...]. The operation-specific
-- schema determines the remaining fields; there are no maps or CBOR tags.
package Sample_Profile with SPARK_Mode is
   type Message_Kind is
     (Evaluate_Request, Evaluation_Error, Endpoint_Reference, Unsigned_Result);
   for Message_Kind use
     (Evaluate_Request => 1, Evaluation_Error => 2,
      Endpoint_Reference => 3, Unsigned_Result => 4);
   type Error_Kind is (Parse_Error, Type_Error, Authority_Denied);
   for Error_Kind use (Parse_Error => 1, Type_Error => 2, Authority_Denied => 3);
   Max_Message_Bytes : constant := 512;
   Max_Source_Bytes : constant := 256;
   Max_Diagnostic_Bytes : constant := 128;
   Reference_Bytes : constant := 16;

   -- Total boundary validator: rejects unsuitable array bounds before calling
   -- the library. Success means shape-valid, NEVER authorized or executable.
   function Valid (Data : CBOR.Byte_Array) return Boolean;
end Sample_Profile;
