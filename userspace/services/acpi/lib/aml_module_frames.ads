with AML_Frame_Handles;
package AML_Module_Frames with SPARK_Mode, Pure is
   use type AML_Frame_Handles.Frame_Handle;
   subtype Depth is Natural range 0 .. AML_Frame_Handles.Max_Active_Frames;
   type State is private;
   function Valid (Item : State) return Boolean;
   function Empty return State with Post => Valid (Empty'Result);
   function Count (Item : State) return Depth;
   function Top (Item : State) return AML_Frame_Handles.Frame_Handle;
   function Contains (Item : State; Frame : AML_Frame_Handles.Frame_Handle) return Boolean;
   function Belongs_To (Item : State; Domain : AML_Frame_Handles.Invocation_Domain) return Boolean;
   function Pushed (Current, Prior : State; Frame : AML_Frame_Handles.Frame_Handle) return Boolean with Ghost;
   function Popped (Current, Prior : State; Frame : AML_Frame_Handles.Frame_Handle) return Boolean with Ghost;
   -- The embedding owner separately authenticates a live declaration lease.
   -- This stack preserves nested module frames; it does not issue frame handles.
   procedure Push (Item : in out State; Frame : AML_Frame_Handles.Frame_Handle;
      Domain : AML_Frame_Handles.Invocation_Domain; Accepted : out Boolean)
     with Pre => Valid (Item), Post => Valid (Item)
       and then (if Accepted then Pushed (Item, Item'Old, Frame)
          and then Top (Item) = Frame and then Belongs_To (Item, Domain)
        else Item = Item'Old);
   procedure Pop (Item : in out State; Expected : AML_Frame_Handles.Frame_Handle;
      Accepted : out Boolean)
     with Pre => Valid (Item), Post => Valid (Item)
       and then (if Accepted then Popped (Item, Item'Old, Expected)
          and then not Contains (Item, Expected)
        else Item = Item'Old);
private
   type Frames is array (Positive range 1 .. Depth'Last) of AML_Frame_Handles.Frame_Handle;
   type State is record
      Used : Depth := 0;
      Items : Frames := [others => AML_Frame_Handles.No_Frame];
   end record;
end AML_Module_Frames;
