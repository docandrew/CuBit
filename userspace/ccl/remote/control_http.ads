package Control_HTTP with SPARK_Mode is
   Max_Header : constant := 2048;
   Max_Input : constant := 4096;
   Development_Origin : constant String := "http://127.0.0.1:8787";
   type Parse_State is (Incomplete, Ready, Rejected);
   type Method is (Submit, Preflight);
   type Request is record
      State : Parse_State := Incomplete;
      Verb : Method := Submit;
      Body_First : Positive range 1 .. Max_Input := 1;
      Body_Length : Natural range 0 .. 1100 := 0;
   end record;
   procedure Parse (Data : String; Result : out Request)
     with Pre => Data'First = 1 and Data'Length <= Max_Input;
   function Response_Header (Length : Natural) return String;
   function Preflight_Response return String;
   function Error_Response return String;
end Control_HTTP;
