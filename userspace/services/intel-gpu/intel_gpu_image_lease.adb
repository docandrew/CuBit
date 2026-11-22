package body Intel_GPU_Image_Lease is
   package Handles renames Intel_GPU_Buffer_Handles;
   package Images renames Intel_GPU_Image_Layout;
   function Current (Object : Lease) return State is (Object.Phase);
   procedure Prepare
     (Object : in out Lease; Buffers : in out Handles.Registry;
      Source : Handles.Retained_Reference; Key : Identity;
      Image : Images.Descriptor; Authorized, Producer_Quiescent : Boolean;
      Accepted : out Boolean) is
      Value : Intel_GPU_Buffer_Reply.Backing;
   begin
      Accepted := False;
      if Object.Phase /= Empty or else not Authorized or else not Producer_Quiescent or else
        Key.Adapter = 0 or else Key.Session = 0 or else Key.Allocation = 0 or else
        Key.Output_Epoch = 0 or else Key.Serial = 0 or else
        Key.Display_Instance = 0 or else Key.Consumer_Instance = 0 or else
        Key.Allocation > Unsigned_64 (Handles.Handle'Last) or else
        not Handles.Reference_Matches
          (Buffers, Source, Key.Session, Handles.Handle (Key.Allocation))
      then return; end if;
      Value := Handles.Referenced_Backing (Buffers, Source);
      if not Value.Ready or else not Images.Valid (Image, Value.Bytes) then return; end if;
      Handles.Retain_Referenced_Backing
        (Buffers, Source, Object.Pin, Accepted, Exclude_Writes => True);
      if not Accepted then return; end if;
      Object.Key := Key;
      Object.Image := Image;
      Object.Phase := Held;
   end Prepare;
   function Backing
     (Object : Lease; Buffers : Handles.Registry; Key : Identity)
      return Intel_GPU_Buffer_Reply.Backing is
   begin
      if Object.Phase /= Held or else Object.Key /= Key then return (Ready => False); end if;
      return Handles.Referenced_Backing (Buffers, Object.Pin);
   end Backing;
   function Layout (Object : Lease; Key : Identity) return Images.Descriptor is
   begin
      if Object.Phase /= Held or else Object.Key /= Key then return (others => <>); end if;
      return Object.Image;
   end Layout;
   procedure Retire
     (Object : in out Lease; Buffers : in out Handles.Registry; Key : Identity;
      GPU_Drained, CPU_Drained, Display_Drained : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Phase /= Held or else Object.Key /= Key or else
        not GPU_Drained or else not CPU_Drained or else not Display_Drained then return; end if;
      Handles.Return_Reference (Buffers, Object.Pin, True, Accepted);
      if Accepted then Object.Phase := Retired; end if;
   end Retire;
end Intel_GPU_Image_Lease;
