with AML_Decode;
package Mutex_Matrix_Data is
   Case_Count : constant := 24;
   Observations_Per_Case : constant := 6;
   subtype Case_Index is Positive range 1 .. Case_Count;
   subtype Observation_Index is Positive range 1 .. Observations_Per_Case;
   type Expected_Kind is (Integer_Result, Mutex_Order_Error,
                          Division_By_Zero_Error, Mutex_Not_Acquired_Error);
   type Expectation (Kind : Expected_Kind := Integer_Result) is record
      case Kind is
         when Integer_Result => Number : AML_Decode.Integer_Value;
         when others => null;
      end case;
   end record;
   function Table (Index : Case_Index) return AML_Decode.Bytes;
   function Case_Name (Index : Case_Index) return String;
   function Revision (Index : Case_Index) return AML_Decode.Byte;
   function Expected (Index : Case_Index; Position : Observation_Index) return Expectation;
   function Method_Name (Position : Observation_Index) return String;
end Mutex_Matrix_Data;
