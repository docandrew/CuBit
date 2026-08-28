with CuBit.UI.Editor.Documents;

package CuBit.UI.Editor.Buffers with SPARK_Mode is
   subtype Buffer_Capacity is Documents.Document_Capacity;

   type Candidate_Buffer (Capacity : Buffer_Capacity) is private;
   type Append_Result is (Appended, Capacity_Exceeded);

   procedure Initialize (Value : out Candidate_Buffer);
   function Length (Value : Candidate_Buffer) return Natural
   with Post => Length'Result <= Value.Capacity;
   function Remaining (Value : Candidate_Buffer) return Natural;
   function Content (Value : Candidate_Buffer) return String
   with Post =>
     Content'Result'First = 1 and then
     Content'Result'Length = Length (Value);

   procedure Append
     (Value : in out Candidate_Buffer; Text : String;
      Result : out Append_Result)
   with Post =>
     (if Text'Length <= Remaining (Value'Old) then
         Result = Appended and then
         Length (Value) = Length (Value'Old) + Text'Length
      else
         Result = Capacity_Exceeded and then
         Length (Value) = Length (Value'Old));

private
   type Candidate_Buffer (Capacity : Buffer_Capacity) is record
      Data : String (1 .. Capacity) := [others => ' '];
      Last : Natural := 0;
   end record
   with Type_Invariant => Candidate_Buffer.Last <= Candidate_Buffer.Capacity;
end CuBit.UI.Editor.Buffers;
