pragma Ada_2022;
with AML_Decode;
package AML_Interface_Policy with SPARK_Mode, Pure is
   Max_Interfaces : constant := 32;
   Max_Interface_Bytes : constant := 256;
   subtype Interface_Count is Natural range 0 .. Max_Interfaces;
   subtype Interface_Length is Natural range 0 .. Max_Interface_Bytes;
   type Policy is private;
   Empty : constant Policy;
   type Add_Status is (Added, Duplicate, Invalid_Name, Full);
   function Count (Item : Policy) return Interface_Count;
   function Supports (Item : Policy; Name : AML_Decode.Bytes) return Boolean
     with Global => null;
   procedure Add (Item : in out Policy; Name : AML_Decode.Bytes;
                  Status : out Add_Status)
     with Post => (if Status /= Added then Item = Item'Old
                   else Count (Item) = Count (Item'Old) + 1);
private
   subtype Interface_Index is Positive range 1 .. Max_Interfaces;
   type Entry_Record is record
      Length : Interface_Length := 0;
      Data : AML_Decode.Bytes (1 .. Max_Interface_Bytes) := [others => 0];
   end record;
   type Entries is array (Interface_Index) of Entry_Record;
   type Policy is record
      Used : Interface_Count := 0;
      Items : Entries;
   end record;
   Empty : constant Policy := (others => <>);
end AML_Interface_Policy;
