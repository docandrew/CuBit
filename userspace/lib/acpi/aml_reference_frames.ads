with AML_Frame_Handles;
generic
   type Value_Type is private;
   Empty_Value : Value_Type;
   Max_Frames : AML_Frame_Handles.Frame_Capacity := AML_Frame_Handles.Max_Active_Frames;
   Max_Generation : AML_Frame_Handles.Generation_Budget := AML_Frame_Handles.Frame_Generation'Last;
package AML_Reference_Frames with SPARK_Mode, Pure is
   use AML_Frame_Handles;
   type State is private;
   type Result_Status is (Ready, Frame_Limit, Generation_Limit, Invalid_Frame,
                         Not_Top_Frame, Invalid_Reference, No_Reference_Authority);
   type Read_Result is record
      Value : Value_Type := Empty_Value;
      Initialized : Boolean := False;
      Status : Result_Status := Invalid_Frame;
   end record;
   function Valid (Store : State) return Boolean;
   function Depth (Store : State) return Natural;
   function Last_Generation (Store : State) return Frame_Generation;
   -- Caller supplies a fresh invocation domain. Reconstruction under a reused
   -- authoritative domain is outside this registry's lifetime contract.
   function Empty (Domain : Invocation_Domain) return State with
     Post => Valid (Empty'Result) and then Depth (Empty'Result) = 0
       and then Last_Generation (Empty'Result) = 0;
   function Opened (Store, Prior : State; Frame : Frame_Handle) return Boolean with Ghost;
   function Closed (Store, Prior : State; Frame : Frame_Handle) return Boolean with Ghost;
   function Cell_Updated (Store, Prior : State; Frame : Frame_Handle;
                         Cell : Cell_ID; Value : Value_Type) return Boolean with Ghost;
   function Reference_Updated (Store, Prior : State; Reference : Cell_Handle;
                              Value : Value_Type) return Boolean with Ghost;
   procedure Open_Frame (Store : in out State; Frame : out Frame_Handle;
                         Status : out Result_Status) with
     Pre => Valid (Store),
     Post => Valid (Store) and then
       (if Status = Ready then Opened (Store, Store'Old, Frame)
        else Store = Store'Old and Frame = No_Frame)
       and then Status in Ready | Frame_Limit | Generation_Limit;
   procedure Close_Frame (Store : in out State; Frame : Frame_Handle;
                          Status : out Result_Status) with
     Pre => Valid (Store),
     Post => Valid (Store) and then
       (if Status = Ready then Closed (Store, Store'Old, Frame) else Store = Store'Old)
       and then Status in Ready | Invalid_Frame | Not_Top_Frame;
   function Cell_Read (Store : State; Frame : Frame_Handle; Cell : Cell_ID;
                       Result : Read_Result) return Boolean with Ghost;
   function Read_Cell (Store : State; Frame : Frame_Handle; Cell : Cell_ID)
                       return Read_Result with
     Pre => Valid (Store),
     Post => Cell_Read (Store, Frame, Cell, Read_Cell'Result);
   procedure Write_Cell (Store : in out State; Frame : Frame_Handle; Cell : Cell_ID;
                         Value : Value_Type; Status : out Result_Status) with
     Pre => Valid (Store),
     Post => Valid (Store) and then
       (if Status = Ready then Cell_Updated (Store, Store'Old, Frame, Cell, Value)
        else Store = Store'Old) and then Status in Ready | Invalid_Frame;
   procedure Make_Reference (Store : State; Frame : Frame_Handle; Cell : Cell_ID;
                             Reference : out Cell_Handle; Status : out Result_Status) with
     Pre => Valid (Store),
     Post => Status in Ready | Invalid_Frame | No_Reference_Authority and then
       (if Status = Ready then Matches (Reference, Frame) and Cell_Of (Reference) = Cell
        else Reference = No_Cell);
   function Reference_Read (Store : State; Reference : Cell_Handle;
                            Result : Read_Result) return Boolean with Ghost;
   function Read_Reference (Store : State; Reference : Cell_Handle) return Read_Result with
     Pre => Valid (Store),
     Post => Reference_Read (Store, Reference, Read_Reference'Result);
   procedure Write_Reference (Store : in out State; Reference : Cell_Handle;
                              Value : Value_Type; Status : out Result_Status) with
     Pre => Valid (Store),
     Post => Valid (Store) and then
       (if Status = Ready then Reference_Updated (Store, Store'Old, Reference, Value)
        else Store = Store'Old) and then Status in Ready | Invalid_Reference;
private
   subtype Active_Count is Natural range 0 .. Max_Frames;
   subtype Storage_Index is Positive range 1 .. Max_Frames;
   type Cell_Values is array (Cell_ID) of Value_Type;
   type Cell_Flags is array (Cell_ID) of Boolean;
   type Frame_Data is record
      Generation : Frame_Generation := 0;
      Values : Cell_Values := [others => Empty_Value];
      Initialized : Cell_Flags := [others => False];
   end record;
   type Frame_Array is array (Storage_Index) of Frame_Data;
   type State is record
      Domain : Invocation_Domain := No_Domain;
      Active : Active_Count := 0;
      Last : Frame_Generation := 0;
      Frames : Frame_Array := [others => <>];
   end record;
   function Depth (Store : State) return Natural is (Store.Active);
   function Last_Generation (Store : State) return Frame_Generation is (Store.Last);
end AML_Reference_Frames;
