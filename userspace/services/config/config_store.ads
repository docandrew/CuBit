pragma Ada_2022;

--  Owned volatile storage only. The IPC layer authenticates/authorizes each
--  operation; this model does not mint authority or promise durable writes.
package Config_Store with Pure, SPARK_Mode is
   Maximum_Key : constant := 128;
   Maximum_Value : constant := 4096;
   Maximum_Entries : constant := 256;
   subtype Slot is Positive range 1 .. Maximum_Entries;
   type Key_Text is record
      Data : String (1 .. Maximum_Key) := [others => ' '];
      Length : Natural range 0 .. Maximum_Key := 0;
   end record;
   type Value_Text is record
      Data : String (1 .. Maximum_Value) := [others => Character'Val (0)];
      Length : Natural range 0 .. Maximum_Value := 0;
   end record;
   type State is private;
   type Update_Result is (Stored, Invalid_Request, Capacity_Exceeded);

   --  A zero-length key denotes an unused slot; an empty VALUE is valid.
   function Key_At (Store : State; Index : Slot) return Key_Text;
   procedure Read
     (Store : State; Key : String; Value : out Value_Text; Found : out Boolean);
   function Has_Value (Store : State; Key, Value : String) return Boolean
     with Ghost;
   procedure Put
     (Store : in out State; Key, Value : String; Result : out Update_Result)
     with Post =>
       (if Result = Stored then Has_Value (Store, Key, Value)
        else Store = Store'Old);
   procedure Remove
     (Store : in out State; Key : String; Removed : out Boolean)
     with Post => (if not Removed then Store = Store'Old);
private
   type Entry_Data is record
      Key : Key_Text;
      Value : Value_Text;
   end record;
   type Entry_Array is array (Slot) of Entry_Data;
   type State is record
      Entries : Entry_Array;
   end record;
end Config_Store;
