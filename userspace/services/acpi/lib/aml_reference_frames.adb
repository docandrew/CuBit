package body AML_Reference_Frames with SPARK_Mode is
   function Cleared (Frame : Frame_Data) return Boolean is
     (Frame.Generation = 0 and then
      (for all C in Cell_ID => not Frame.Initialized (C) and then Frame.Values (C) = Empty_Value));
   function Valid (Store : State) return Boolean is
   begin
      if Store.Last > Max_Generation then return False; end if;
      for I in Storage_Index loop
         if I <= Store.Active then
            if Store.Frames (I).Generation = 0 or else Store.Frames (I).Generation > Store.Last
              or else (I > 1 and then Store.Frames (I - 1).Generation >= Store.Frames (I).Generation)
            then return False; end if;
            for C in Cell_ID loop
               if not Store.Frames (I).Initialized (C) and then Store.Frames (I).Values (C) /= Empty_Value
               then return False; end if;
            end loop;
         elsif not Cleared (Store.Frames (I)) then return False;
         end if;
      end loop;
      return True;
   end Valid;
   function Empty (Domain : Invocation_Domain) return State is
     (Domain => Domain, others => <>);
   function Live (Store : State; Frame : Frame_Handle) return Boolean is
     (Belongs_To (Frame, Store.Domain) and then Index_Of (Frame) <= Frame_Index (Store.Active)
      and then Store.Frames (Storage_Index (Index_Of (Frame))).Generation = Generation_Of (Frame));
   function Live (Store : State; Reference : Cell_Handle) return Boolean is
     (Has_Authority (Store.Domain) and then Belongs_To (Reference, Store.Domain)
      and then Index_Of (Reference) <= Frame_Index (Store.Active)
      and then Store.Frames (Storage_Index (Index_Of (Reference))).Generation = Generation_Of (Reference));
   function Opened (Store, Prior : State; Frame : Frame_Handle) return Boolean is
     (Prior.Active < Max_Frames and then Prior.Last < Max_Generation
      and then Store.Domain = Prior.Domain and then Store.Active = Prior.Active + 1
      and then Store.Last = Prior.Last + 1
      and then Frame = Bind_Frame (Store.Domain, Frame_Index (Store.Active), Store.Last)
      and then (for all I in Storage_Index =>
        (if I = Store.Active then Store.Frames (I).Generation = Store.Last
          and then (for all C in Cell_ID => not Store.Frames (I).Initialized (C)
                    and then Store.Frames (I).Values (C) = Empty_Value)
         else Store.Frames (I) = Prior.Frames (I))));
   function Closed (Store, Prior : State; Frame : Frame_Handle) return Boolean is
     (Live (Prior, Frame) and then Index_Of (Frame) = Frame_Index (Prior.Active)
      and then Store.Domain = Prior.Domain and then Store.Last = Prior.Last
      and then Store.Active = Prior.Active - 1
      and then (for all I in Storage_Index =>
        (if I = Prior.Active then Cleared (Store.Frames (I)) else Store.Frames (I) = Prior.Frames (I))));
   function Cell_Updated (Store, Prior : State; Frame : Frame_Handle;
                         Cell : Cell_ID; Value : Value_Type) return Boolean is
     (Live (Prior, Frame) and then Store.Domain = Prior.Domain
      and then Store.Active = Prior.Active and then Store.Last = Prior.Last
      and then (for all I in Storage_Index =>
        (if I = Natural (Index_Of (Frame)) then Store.Frames (I).Generation = Prior.Frames (I).Generation
          and then (for all C in Cell_ID =>
            (if C = Cell then Store.Frames (I).Initialized (C) and then Store.Frames (I).Values (C) = Value
             else Store.Frames (I).Initialized (C) = Prior.Frames (I).Initialized (C)
               and then Store.Frames (I).Values (C) = Prior.Frames (I).Values (C)))
         else Store.Frames (I) = Prior.Frames (I))));
   function Reference_Updated (Store, Prior : State; Reference : Cell_Handle;
                              Value : Value_Type) return Boolean is
     (Live (Prior, Reference) and then Cell_Updated (Store, Prior,
       Bind_Frame (Prior.Domain, Index_Of (Reference), Generation_Of (Reference)), Cell_Of (Reference), Value));
   procedure Open_Frame (Store : in out State; Frame : out Frame_Handle;
                         Status : out Result_Status) is
   begin
      Frame := No_Frame;
      if Store.Active = Max_Frames then Status := Frame_Limit; return; end if;
      if Store.Last = Max_Generation then Status := Generation_Limit; return; end if;
      Store.Last := Store.Last + 1;
      Store.Active := Store.Active + 1;
      Store.Frames (Store.Active) := (Generation => Store.Last, others => <>);
      Frame := Bind_Frame (Store.Domain, Frame_Index (Store.Active), Store.Last);
      Status := Ready;
   end Open_Frame;
   procedure Close_Frame (Store : in out State; Frame : Frame_Handle;
                          Status : out Result_Status) is
   begin
      if not Live (Store, Frame) then Status := Invalid_Frame; return; end if;
      if Index_Of (Frame) /= Frame_Index (Store.Active) then Status := Not_Top_Frame; return; end if;
      Store.Frames (Store.Active) := (others => <>);
      Store.Active := Store.Active - 1;
      Status := Ready;
   end Close_Frame;
   function Cell_Read (Store : State; Frame : Frame_Handle; Cell : Cell_ID;
                       Result : Read_Result) return Boolean is
     (if Live (Store, Frame) then Result.Status = Ready
        and then Result.Initialized = Store.Frames (Storage_Index (Index_Of (Frame))).Initialized (Cell)
        and then Result.Value = Store.Frames (Storage_Index (Index_Of (Frame))).Values (Cell)
      else Result.Status = Invalid_Frame and then not Result.Initialized and then Result.Value = Empty_Value);
   function Read_Cell (Store : State; Frame : Frame_Handle; Cell : Cell_ID)
                       return Read_Result is
   begin
      if not Live (Store, Frame) then return (Value => Empty_Value, Initialized => False, Status => Invalid_Frame); end if;
      return (Value => Store.Frames (Storage_Index (Index_Of (Frame))).Values (Cell),
              Initialized => Store.Frames (Storage_Index (Index_Of (Frame))).Initialized (Cell), Status => Ready);
   end Read_Cell;
   procedure Write_Cell (Store : in out State; Frame : Frame_Handle; Cell : Cell_ID;
                         Value : Value_Type; Status : out Result_Status) is
   begin
      Status := Invalid_Frame;
      if not Live (Store, Frame) then return; end if;
      Store.Frames (Storage_Index (Index_Of (Frame))).Values (Cell) := Value;
      Store.Frames (Storage_Index (Index_Of (Frame))).Initialized (Cell) := True;
      Status := Ready;
   end Write_Cell;
   procedure Make_Reference (Store : State; Frame : Frame_Handle; Cell : Cell_ID;
                             Reference : out Cell_Handle; Status : out Result_Status) is
   begin
      Reference := No_Cell; Status := Invalid_Frame;
      if not Live (Store, Frame) then return; end if;
      Status := No_Reference_Authority;
      if not Has_Authority (Store.Domain) then return; end if;
      Reference := Bind_Cell (Frame, Cell); Status := Ready;
   end Make_Reference;
   function Reference_Read (Store : State; Reference : Cell_Handle;
                            Result : Read_Result) return Boolean is
     (if Live (Store, Reference) then Result.Status = Ready
        and then Result.Initialized = Store.Frames (Storage_Index (Index_Of (Reference))).Initialized (Cell_Of (Reference))
        and then Result.Value = Store.Frames (Storage_Index (Index_Of (Reference))).Values (Cell_Of (Reference))
      else Result.Status = Invalid_Reference and then not Result.Initialized and then Result.Value = Empty_Value);
   function Read_Reference (Store : State; Reference : Cell_Handle) return Read_Result is
   begin
      if not Live (Store, Reference) then return (Value => Empty_Value, Initialized => False, Status => Invalid_Reference); end if;
      return (Value => Store.Frames (Storage_Index (Index_Of (Reference))).Values (Cell_Of (Reference)),
              Initialized => Store.Frames (Storage_Index (Index_Of (Reference))).Initialized (Cell_Of (Reference)), Status => Ready);
   end Read_Reference;
   procedure Write_Reference (Store : in out State; Reference : Cell_Handle;
                              Value : Value_Type; Status : out Result_Status) is
   begin
      Status := Invalid_Reference;
      if not Live (Store, Reference) then return; end if;
      Store.Frames (Storage_Index (Index_Of (Reference))).Values (Cell_Of (Reference)) := Value;
      Store.Frames (Storage_Index (Index_Of (Reference))).Initialized (Cell_Of (Reference)) := True;
      Status := Ready;
   end Write_Reference;
end AML_Reference_Frames;
