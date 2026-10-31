package body AML_Module_Frames with SPARK_Mode is
   function Valid (Item : State) return Boolean is
     (for all I in Item.Items'Range =>
       (if I <= Item.Used then AML_Frame_Handles.Present (Item.Items (I))
          and then (for all J in Item.Items'First .. I - 1 => Item.Items (J) /= Item.Items (I))
        else Item.Items (I) = AML_Frame_Handles.No_Frame));
   function Empty return State is ((others => <>));
   function Count (Item : State) return Depth is (Item.Used);
   function Top (Item : State) return AML_Frame_Handles.Frame_Handle is
     (if Item.Used = 0 then AML_Frame_Handles.No_Frame else Item.Items (Item.Used));
   function Contains (Item : State; Frame : AML_Frame_Handles.Frame_Handle) return Boolean is
     (AML_Frame_Handles.Present (Frame) and then
       (for some I in 1 .. Item.Used => Item.Items (I) = Frame));
   function Belongs_To (Item : State; Domain : AML_Frame_Handles.Invocation_Domain) return Boolean is
     (for all I in 1 .. Item.Used => AML_Frame_Handles.Belongs_To (Item.Items (I), Domain));
   function Pushed (Current, Prior : State; Frame : AML_Frame_Handles.Frame_Handle) return Boolean is
     (Prior.Used < Depth'Last and then Current =
       (Used => Prior.Used + 1, Items => (Prior.Items with delta Prior.Used + 1 => Frame)));
   function Popped (Current, Prior : State; Frame : AML_Frame_Handles.Frame_Handle) return Boolean is
     (Prior.Used > 0 and then Top (Prior) = Frame and then Current =
       (Used => Prior.Used - 1, Items => (Prior.Items with delta Prior.Used => AML_Frame_Handles.No_Frame)));
   procedure Push (Item : in out State; Frame : AML_Frame_Handles.Frame_Handle;
      Domain : AML_Frame_Handles.Invocation_Domain; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Item.Used = Depth'Last or else not AML_Frame_Handles.Has_Authority (Domain)
        or else not AML_Frame_Handles.Belongs_To (Frame, Domain)
        or else not Belongs_To (Item, Domain) or else Contains (Item, Frame) then return; end if;
      Item.Used := Item.Used + 1; Item.Items (Item.Used) := Frame; Accepted := True;
   end Push;
   procedure Pop (Item : in out State; Expected : AML_Frame_Handles.Frame_Handle;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Item.Used = 0 or else Top (Item) /= Expected then return; end if;
      Item.Items (Item.Used) := AML_Frame_Handles.No_Frame; Item.Used := Item.Used - 1;
      Accepted := True;
   end Pop;
end AML_Module_Frames;
