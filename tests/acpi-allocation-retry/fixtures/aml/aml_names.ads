pragma Ada_2022;
with AML_Decode;
package AML_Names with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   --  AML encodes at most 255 segments in MultiNamePath. Parent prefixes are
   --  separately budgeted; excessive prefixes fail instead of wrapping.
   subtype Segment is String (1 .. 4);
   type Segment_Array is array (Positive range 1 .. 255) of Segment;
   type Parse_Status is (Accepted, Truncated, Malformed, Limit_Exceeded);
   type Name_Result (Kind : Parse_Status := Truncated) is record
      case Kind is
         when Accepted =>
            Rooted : Boolean;
            Parents : Natural range 0 .. 255;
            Count : Natural range 0 .. 255;
            Parts : Segment_Array;
            Consumed : Positive;
         when others => null;
      end case;
   end record;
   function Lead (C : AML_Decode.Byte) return Boolean is
     (C = 16#5F# or else C in 16#41# .. 16#5A#);
   function Tail (C : AML_Decode.Byte) return Boolean is
     (Lead (C) or else C in 16#30# .. 16#39#);
   function Valid (Part : Segment) return Boolean is
     (Lead (AML_Decode.Byte (Character'Pos (Part (1)))) and then
      (for all J in 2 .. 4 =>
         Tail (AML_Decode.Byte (Character'Pos (Part (J))))));
   function Read_Name (Data : AML_Decode.Bytes) return Name_Result
     with Post =>
       (if Read_Name'Result.Kind = Accepted then
          Read_Name'Result.Consumed <= Data'Length
          and then (if Read_Name'Result.Rooted then
                      Read_Name'Result.Parents = 0)
          and then (for all I in 1 .. Read_Name'Result.Count =>
                      Valid (Read_Name'Result.Parts (I))));
end AML_Names;
