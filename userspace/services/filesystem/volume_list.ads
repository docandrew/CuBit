with Interfaces; use Interfaces;

--  Internal block-volume names and bindings, not client authority.
--  Append-only for one filesystem-service lifetime: no replacement or reuse.
package Volume_List with SPARK_Mode is
   Maximum_Volumes : constant := 16;
   Maximum_Name_Bytes : constant := 48;
   subtype Volume_Reference is Natural range 0 .. Maximum_Volumes;
   subtype Volume_Index is Volume_Reference range 1 .. Maximum_Volumes;
   No_Volume : constant Volume_Reference := 0;
   subtype Endpoint_Slot is Unsigned_64 range 1 .. 62;
   subtype Transfer_Page_Count is Positive range 1 .. 4096;

   type Device_Binding is record
      Endpoint : Endpoint_Slot;
      Ready_Role : Unsigned_64 := 0; -- bootstrap readiness hint, not authority
      Transfer_Pages : Transfer_Page_Count;
   end record;
   type State is private;
   type Registration_Result is
     (Registered, Invalid_Name, Name_In_Use, Endpoint_In_Use, List_Full);
   type Path_Selection is
     (Unqualified, Known_Volume, Unknown_Volume, Invalid_Path);

   function Count (List : State) return Volume_Reference;
   function Name (List : State; Volume : Volume_Index) return String
     with Pre => Volume <= Count (List);
   function Binding (List : State; Volume : Volume_Index) return Device_Binding
     with Pre => Volume <= Count (List);
   procedure Register
     (List : in out State; Name : String; Device : Device_Binding;
      Volume : out Volume_Reference; Result : out Registration_Result);
   procedure Select_Path
     (List : State; Path : String; Selection : out Path_Selection;
      Volume : out Volume_Reference; Relative_First : out Integer)
     with Pre => Path'Last < Positive'Last;
   --  Relative_First uses the index base type: a null String can have bounds
   --  outside Positive. Nonempty qualified names always return a valid suffix.

private
   type Entry_Record is record
      Text : String (1 .. Maximum_Name_Bytes) := [others => ' '];
      Length : Natural range 0 .. Maximum_Name_Bytes := 0;
      Device : Device_Binding :=
        (Endpoint => 1, Ready_Role => 0, Transfer_Pages => 1);
   end record;
   type Entries is array (Volume_Index) of Entry_Record;
   type State is record
      Used : Volume_Reference := 0;
      Items : Entries;
   end record;
   function Count (List : State) return Volume_Reference is (List.Used);
end Volume_List;
