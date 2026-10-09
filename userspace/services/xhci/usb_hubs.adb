package body USB_Hubs with SPARK_Mode is
   --  USB 2.0 11.23.2.1: seven fixed bytes, then DeviceRemovable (one bit
   --  per port plus reserved bit zero), then PortPwrCtrlMask. The mask is
   --  obsolete (USB 1.0 compatibility) and unused here; devices size it
   --  differently (QEMU's eight-port hub sends ceil (ports / 8) bytes, ten
   --  in all), so it may be absent or up to DeviceRemovable's width.
   Fixed_Bytes          : constant := 7;
   Hub_Descriptor       : constant := 16#29#;
   SuperSpeed_Hub       : constant := 16#2A#;
   Shortest             : constant := Fixed_Bytes + 1;
   Longest              : constant := Fixed_Bytes + 2 * 32;

   procedure Decode (Data : Bytes; Value : out Descriptor; Result : out Decode_Result) is
   begin
      Value := (others => <>);
      Result := Malformed;
      if Data'Length not in Shortest .. Longest then return; end if;
      declare
         Frame : constant Bytes (1 .. Data'Length) := Data;
         Ports : constant Natural := Natural (Frame (3));
         Removable_Bytes : constant Natural := (Ports + 8) / 8;
         Characteristics : constant Unsigned_16 :=
           Unsigned_16 (Frame (4)) + 256 * Unsigned_16 (Frame (5));
      begin
         if Frame (2) = SuperSpeed_Hub then Result := Unsupported; return; end if;
         if Frame (2) /= Hub_Descriptor or else Ports = 0 or else
           Natural (Frame (1)) /= Frame'Length or else
           Natural (Frame (1)) not in
             Fixed_Bytes + Removable_Bytes .. Fixed_Bytes + 2 * Removable_Bytes
         then return; end if;
         case Characteristics and 3 is
            when 0 => Value.Power := Ganged;
            when 1 => Value.Power := Individual;
            when others => Value.Power := Always_On; -- USB2: 1x = no switching.
         end case;
         Value.Ports := Ports;
         Value.Power_Delay_MS := 2 * Natural (Frame (6));
         Value.TT_Think_Time := Natural (Shift_Right (Characteristics, 5) and 3);
         Result := Decoded;
      end;
   end Decode;

   function Action (Status : Unsigned_16) return Port_Action is
   begin
      if (Status and 8) /= 0 then return Overcurrent;
      elsif (Status and 1) = 0 then return Disconnected;
      elsif (Status and 16#0600#) = 16#0600# then return Invalid_Status;
      elsif (Status and 16#0100#) = 0 then return Power_Required;
      elsif (Status and 16#0010#) /= 0 then return Resetting;
      elsif (Status and 2) = 0 or else (Status and 4) /= 0 then return Reset_Required;
      else return Ready;
      end if;
   end Action;

   function Rate (Status : Unsigned_16) return XHCI_Topology.Speed is
   begin
      if (Status and 16#0400#) /= 0 then return XHCI_Topology.High_Speed;
      elsif (Status and 16#0200#) /= 0 then return XHCI_Topology.Low_Speed;
      else return XHCI_Topology.Full_Speed;
      end if;
   end Rate;
end USB_Hubs;
