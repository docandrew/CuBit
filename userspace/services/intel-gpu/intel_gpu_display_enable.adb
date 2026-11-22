package body Intel_GPU_Display_Enable is
   use Interfaces;
   use Intel_GPU_Display_Topology;
   Attempted : Boolean := False;
   Held, Added_Request : Boolean := False;
   Control : constant Unsigned_32 := 16#45404#;
   Fuses : constant Unsigned_32 := 16#42000#;
   Workaround : constant Unsigned_32 := 16#46430#;
   Fuse_Mask : constant Unsigned_32 :=
     (case Item is
        when PW1 => 16#0400_0000#, when PW2 => 16#0200_0000#,
        when PWA => 16#0020_0000#, when PWB => 16#0010_0000#,
        when PWC => 16#0008_0000#, when PWD => 16#0004_0000#);
   procedure Execute
     (Prerequisites_Ready : Boolean; Poll_Limit : Positive;
      Added : out Boolean; Status : out Result)
   is
      Initial, Value : Unsigned_32;
      OK : Boolean;
      function Read_Valid (Offset : Unsigned_32; Data : out Unsigned_32) return Boolean is
      begin
         Data := Read_32 (Offset);
         if Data = Unsigned_32'Last then Status := Invalid_MMIO; return False; end if;
         return True;
      end Read_Valid;
      function Wait_Set (Offset, Mask : Unsigned_32) return Boolean is
         First : constant Unsigned_64 := Now_Us;
         Previous : Unsigned_64 := First;
         Stamp : Unsigned_64;
         Data : Unsigned_32;
         function In_Time return Boolean is
         begin
            Stamp := Now_Us;
            if Stamp = Unsigned_64'Last or else Stamp < Previous then
               Status := Invalid_Clock; return False;
            end if;
            Previous := Stamp;
            if Stamp - First >= 1_000 then Status := Deadline_Expired; return False; end if;
            return True;
         end In_Time;
      begin
         if First = Unsigned_64'Last then Status := Invalid_Clock; return False; end if;
         for N in 1 .. Poll_Limit loop
            if not In_Time or else not Read_Valid (Offset, Data) or else not In_Time then
               return False;
            end if;
            if (Data and Mask) = Mask then return True; end if;
            if N < Poll_Limit then Pause; end if;
         end loop;
         Status := Poll_Exhausted;
         return False;
      end Wait_Set;
   begin
      Added := False; Status := Rejected;
      if Attempted or else not Prerequisites_Ready then return; end if;
      Attempted := True;
      if Now_Us = Unsigned_64'Last then Status := Invalid_Clock; return; end if;
      if not Read_Valid (Control, Initial) then return; end if;
      if Item = PW1 then
         if not Read_Valid (Workaround, Value) then return; end if;
         Write_32 (Workaround, Value or 16#8000#, OK);
         if not OK then Status := Write_Failed; return; end if;
         if not Wait_Set (Fuses, 16#0800_0000#) then return; end if;
      end if;
      if not Read_Valid (Control, Value) then return; end if;
      if (Initial and Request_Mask (Item)) /= (Value and Request_Mask (Item)) then
         Status := Request_Changed; return;
      end if;
      if (Initial and Request_Mask (Item)) = 0 then
         Added := True;
         Write_32 (Control, Value or Request_Mask (Item), OK);
         if not OK then Status := Write_Failed; return; end if;
      end if;
      if not Wait_Set (Control, Request_Mask (Item) or State_Mask (Item)) then return; end if;
      if not Wait_Set (Fuses, Fuse_Mask) then return; end if;
      Post_Enable (OK);
      Status := (if OK then Ready else Post_Enable_Failed);
      if OK then Held := True; Added_Request := Added; end if;
   end Execute;
   procedure Release (Status : out Result) is
      Value : Unsigned_32;
      OK : Boolean;
   begin
      Status := Rejected;
      if not Held then return; end if;
      Held := False;
      if Added_Request then
         Pre_Disable (OK);
         if not OK then Status := Pre_Disable_Failed; return; end if;
         Value := Read_32 (Control);
         if Value = Unsigned_32'Last then Status := Invalid_MMIO; return; end if;
         if (Value and Request_Mask (Item)) = 0 then Status := Request_Changed; return; end if;
         Write_32 (Control, Value and not Request_Mask (Item), OK);
         if not OK then Status := Write_Failed; return; end if;
         Value := Read_32 (Control);
         if Value = Unsigned_32'Last then Status := Invalid_MMIO; return; end if;
         if (Value and Request_Mask (Item)) /= 0 then Status := Request_Changed; return; end if;
      end if;
      Added_Request := False;
      Attempted := False;
      Status := Released;
   end Release;
end Intel_GPU_Display_Enable;
