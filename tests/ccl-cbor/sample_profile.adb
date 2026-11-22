with CBOR.Decoding;
with Interfaces;

package body Sample_Profile with SPARK_Mode is
   use CBOR;
   use type CBOR.SE_Offset;
   use type Interfaces.Unsigned_64;

   function Valid (Data : CBOR.Byte_Array) return Boolean is
   begin
      if Data'First < 0 or else Data'Last > Decoding.Max_Data_Length or else
        Data'Length not in 1 .. Max_Message_Bytes
      then
         return False;
      end if;
      declare
         R : constant Decode_All_Result := Decoding.Decode_All_Strict
           (Data, Check_UTF8 => True, Max_String_Len => Max_Source_Bytes,
            Max_Depth => 1);
      begin
         if R.Status /= OK or else R.Count not in 5 | 8 or else
           R.Items (1).Kind /= MT_Array or else
           R.Items (2).Kind /= MT_Unsigned_Integer or else
           R.Items (2).UInt_Value /= 1 or else
           R.Items (3).Kind /= MT_Unsigned_Integer or else
           R.Items (3).UInt_Value = 0 or else
           R.Items (4).Kind /= MT_Unsigned_Integer or else
           R.Items (4).UInt_Value not in
             Message_Kind'Enum_Rep (Message_Kind'First) ..
             Message_Kind'Enum_Rep (Message_Kind'Last)
         then
            return False;
         end if;
         case Message_Kind'Enum_Val (R.Items (4).UInt_Value) is
            when Evaluate_Request =>
               return R.Count = 5 and then R.Items (1).Arr_Count = 4 and then
                 R.Items (5).Kind = MT_Text_String and then
                 R.Items (5).TS_Ref.Length <= Max_Source_Bytes;
            when Evaluation_Error =>
               return R.Count = 8 and then R.Items (1).Arr_Count = 7 and then
                 R.Items (5).Kind = MT_Unsigned_Integer and then
                 R.Items (5).UInt_Value in
                   Error_Kind'Enum_Rep (Error_Kind'First) ..
                   Error_Kind'Enum_Rep (Error_Kind'Last) and then
                 R.Items (6).Kind = MT_Unsigned_Integer and then
                 R.Items (6).UInt_Value in 1 .. 2 ** 32 - 1 and then
                 R.Items (7).Kind = MT_Unsigned_Integer and then
                 R.Items (7).UInt_Value in 1 .. 2 ** 32 - 1 and then
                 R.Items (8).Kind = MT_Text_String and then
                 R.Items (8).TS_Ref.Length <= Max_Diagnostic_Bytes;
            when Endpoint_Reference =>
               -- The reference is opaque data. Session admission/resolution
               -- must subsequently check ownership, liveness and authority.
               return R.Count = 5 and then R.Items (1).Arr_Count = 4 and then
                 R.Items (5).Kind = MT_Byte_String and then
                 R.Items (5).BS_Ref.Length = Reference_Bytes;
            when Unsigned_Result =>
               return R.Count = 5 and then R.Items (1).Arr_Count = 4 and then
                 R.Items (5).Kind = MT_Unsigned_Integer;
         end case;
      end;
   end Valid;
end Sample_Profile;
