package body Intel_Render_Admission with SPARK_Mode is
   package Async renames CuBit.Async_Requests;
   use type Async.Phase;
   procedure Start (Item : in out Transaction; Captured : Unsigned_64) is
   begin
      if Item.Current /= Idle then return; end if;
      if Item.Stopped or Captured mod 2 ** 32 = 0 or
        Captured / 2 ** 32 = 0 then
         Item.Current := Failed;
      else
         Item.Target := Captured;
         Item.Current := Reserve_Ready;
      end if;
   end Start;
   function Request (Item : Transaction) return Words is
     ([1, Item.Target, Item.Tag,
       (case Item.Current is
          when Reserve_Ready => 0, when Activate_Ready => 1,
          when others => 2)]);
   procedure Prepare (Item : in out Transaction; ID : Unsigned_64;
                      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Item.Current not in Reserve_Ready | Activate_Ready | Abort_Ready
      then return; end if;
      Async.Reserve (Item.Pending, ID, Accepted);
      if not Accepted then return; end if;
      Item.Current := (case Item.Current is
         when Reserve_Ready => Reserve_Pending,
         when Activate_Ready => Activate_Pending,
         when others => Abort_Pending);
   end Prepare;
   procedure Submitted (Item : in out Transaction; Accepted : Boolean) is
   begin
      if Async.State (Item.Pending) /= Async.Reserved then return; end if;
      Async.Submitted (Item.Pending, Accepted);
      if not Accepted then
         Item.Current := (if Item.Current = Reserve_Pending then Failed
           elsif Item.Current = Activate_Pending then Abort_Ready
           else Quarantined);
      end if;
   end Submitted;
   procedure Complete (Item : in out Transaction; ID : Unsigned_64;
     Envelope_OK : Boolean; Reply : Words; Consumed : out Boolean) is
      Prior : constant Phase := Item.Current;
   begin
      Async.Capture (Item.Pending, ID, True, Consumed);
      if not Consumed then return; end if;
      Async.Release (Item.Pending);
      if not Envelope_OK or Reply (0) > 4 or Reply (1) /= 1 or
        Reply (3) /= 0 or
        (Reply (0) /= 0 and Reply (2) /= 0) then
         Item.Current := Quarantined;
         return;
      end if;
      if Reply (0) /= 0 then
         Item.Current := (if Prior = Reserve_Pending then Failed
           elsif Prior = Activate_Pending then Abort_Ready else Quarantined);
         return;
      end if;
      if Reply (2) = 0 or (Prior /= Reserve_Pending and
        Reply (2) /= Item.Tag) then
         Item.Current := Quarantined;
         return;
      end if;
      case Prior is
         when Reserve_Pending =>
            Item.Tag := Reply (2);
            Item.Current := (if Item.Stopped then Abort_Ready
                             else Delegate_Ready);
         when Activate_Pending =>
            Item.Current := (if Item.Stopped then Abort_Ready else Active);
         when Abort_Pending => Item.Current := Failed;
         when others => Item.Current := Quarantined;
      end case;
   end Complete;
   procedure Delegated (Item : in out Transaction; Installed : Boolean) is
   begin
      Item.Current := (if Installed and not Item.Stopped then Activate_Ready
                       else Abort_Ready);
   end Delegated;
   procedure Cancel (Item : in out Transaction) is
   begin
      Item.Stopped := True;
      case Item.Current is
         when Idle | Reserve_Ready => Item.Current := Failed;
         when Delegate_Ready | Activate_Ready | Active =>
            Item.Current := Abort_Ready;
         when others => null;
      end case;
   end Cancel;
end Intel_Render_Admission;
