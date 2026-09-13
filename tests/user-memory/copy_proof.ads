with Interfaces; use Interfaces;
with User_Buffer_Copy;
with User_Page_Walk;
package Copy_Proof with SPARK_Mode is
   procedure Exercise_Copy (Source, Length : Unsigned_64; Allowed : Boolean; Success : out Boolean)
     with Post => (if Success then User_Buffer_Copy.Valid_Range (Source, Length));
   function Exercise_Walk (Root, Address, Physical_Last, Word : Unsigned_64) return Unsigned_64
     with Post => (if Exercise_Walk'Result /= 0 then
       Address < User_Page_Walk.User_Limit and then
       User_Page_Walk.RAM_Page (Exercise_Walk'Result, Physical_Last));
   procedure Exercise_Name
     (Source : Unsigned_64; Byte : Character; Allowed : Boolean;
      Name : out String; Success : out Boolean)
     with Post => (if Success and then Name'Length > 0 then User_Buffer_Copy.Valid_Range (Source, 1));
end Copy_Proof;
