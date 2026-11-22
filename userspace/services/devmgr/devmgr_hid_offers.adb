pragma Ada_2022;

package body Devmgr_HID_Offers with SPARK_Mode is

   function Find_Receipt
     (Receipts : Receipt_Array; Owner, Epoch, Request : Unsigned_64) return Natural
   is
   begin
      for K in Receipts'Range loop
         pragma Loop_Invariant
           (for all J in Receipts'First .. K - 1 =>
              not (Receipts (J).Owner = Owner and then Receipts (J).Epoch = Epoch
                   and then Receipts (J).Request = Request));
         if Receipts (K).Owner = Owner and then Receipts (K).Epoch = Epoch
           and then Receipts (K).Request = Request
         then
            return K;
         end if;
      end loop;
      return 0;
   end Find_Receipt;

   function Match_Controller
     (PCI : HP.PCI_Observation; Inventory : PCI_Function_Array) return Controller_Match
   is
      Where : HP.PCI_Location;
   begin
      if not (HP.Fully_Resolved (PCI) and then HP.Valid_PCI (PCI)) then
         return (Outcome => Unresolved_Location, At_Function => 0);
      end if;
      Where := HP.Location (PCI);
      for K in Inventory'Range loop
         if Inventory (K).Location = Where then
            return (Outcome => (if Inventory (K).Driver_Bound then Bound_Controller
                                else Unbound_Controller),
                    At_Function => K);
         end if;
      end loop;
      return (Outcome => Absent_Location, At_Function => 0);
   end Match_Controller;

   function Find_GPIO
     (Offer : HP.Request; Owner : Unsigned_64; Records : GPIO_Record_Array) return Natural
   is
   begin
      for K in Records'Range loop
         pragma Loop_Invariant
           (for all J in Records'First .. K - 1 => not GPIO_Matches (Offer, Owner, Records (J)));
         if GPIO_Matches (Offer, Owner, Records (K)) then
            return K;
         end if;
      end loop;
      return 0;
   end Find_GPIO;

   function GPIO_Input (Offer : HP.Request) return GPIO_Channel_Badges.Interrupt_Input is
     ((Pin     => GPIO_Channel_Badges.Pin_Number (Offer.Pin),
       Trigger => (case Offer.IRQ_Trigger is
                     when HP.Level => GPIO_Channel_Badges.Level_Triggered,
                     when HP.Edge  => GPIO_Channel_Badges.Edge_Triggered),
       Active  => (case Offer.IRQ_Polarity is
                     when HP.Active_High => GPIO_Channel_Badges.Active_High,
                     when HP.Active_Low  => GPIO_Channel_Badges.Active_Low,
                     when HP.Active_Both => GPIO_Channel_Badges.Active_Both)));

   function I2C_Target (Offer : HP.Request) return I2C_Channel_Badges.Target is
     ((Mode       => (case Offer.Addressing is
                        when HP.Seven_Bit => I2C_Channel_Badges.Seven_Bit,
                        when HP.Ten_Bit   => I2C_Channel_Badges.Ten_Bit),
       Address    => I2C_Channel_Badges.Address_Value (Offer.Address),
       Maximum_Hz => I2C_Channel_Badges.Hertz (Offer.Maximum_Hz)));

   function Decide
     (Offer     : HP.Request;
      Owner     : Unsigned_64;
      Inventory : PCI_Function_Array;
      GPIOs     : GPIO_Record_Array;
      Queue     : Queued_Offer_Array) return Verdict
   is
      use type I2C_Channel_Badges.Encode_Status;
      use type GPIO_Channel_Badges.Encode_Status;
      Result : Verdict;
      Match  : constant Controller_Match := Match_Controller (Offer.PCI, Inventory);
      At_GPIO : Natural;
   begin
      --  The I2C controller: devmgr's own inventory decides, never the
      --  offer's source path.
      case Match.Outcome is
         when Unresolved_Location | Absent_Location =>
            Result.Answer := HX.Unknown_Controller;
            return Result;
         when Unbound_Controller =>
            Result.Answer := HX.Controller_Mismatch;
            Result.Controller := Match.At_Function;
            return Result;
         when Bound_Controller =>
            null;
      end case;
      --  The GpioInt's controller: verified for use, same sender and epoch.
      At_GPIO := Find_GPIO (Offer, Owner, GPIOs);
      if At_GPIO = 0 or else Offer.Pin = No_Pin then
         Result.Answer := HX.GPIO_Not_Admitted;
         return Result;
      end if;
      pragma Assert (GPIO_Matches (Offer, Owner, GPIOs (At_GPIO)));
      --  One queued offer per canonical device path per (sender, epoch).
      for K in Queue'Range loop
         pragma Loop_Invariant
           (At_GPIO in GPIOs'Range and then GPIO_Matches (Offer, Owner, GPIOs (At_GPIO))
            and then Match.Outcome = Bound_Controller
            and then (for all J in Queue'First .. K - 1 =>
                        not Same_Device (Queue (J), Owner, Offer.Epoch, Offer.Device_Path)));
         if Same_Device (Queue (K), Owner, Offer.Epoch, Offer.Device_Path) then
            Result.Answer := HX.Duplicate_Device;
            return Result;
         end if;
      end loop;
      --  The channel badges the launch mints (the codec has checked the
      --  address, mode and speed; a refusal here is a malformed target).
      if Offer.Address > HP.Ten_Bit_Last then
         Result.Answer := HX.Malformed;
         Result.Detail := HP.Address_Offset;
         return Result;
      end if;
      declare
         Target : constant I2C_Channel_Badges.Encoding :=
           I2C_Channel_Badges.Encode (I2C_Target (Offer));
         Input  : constant GPIO_Channel_Badges.Encoding :=
           GPIO_Channel_Badges.Encode (GPIO_Input (Offer));
      begin
         if Target.Status /= I2C_Channel_Badges.Encoded then
            Result.Answer := HX.Malformed;
            Result.Detail := HP.Address_Offset;
            return Result;
         elsif Input.Status /= GPIO_Channel_Badges.Encoded then
            Result.Answer := HX.Malformed;
            Result.Detail := HP.Polarity_Offset;
            return Result;
         end if;
         Result.Controller := Match.At_Function;
         Result.GPIO := At_GPIO;
         pragma Assert (GPIO_Matches (Offer, Owner, GPIOs (Result.GPIO)));
         Result.I2C_Badge := Target.Badge;
         Result.GPIO_Badge := Input.Badge;
      end;
      Result.Answer := HX.Queued;
      return Result;
   end Decide;

   function Provider_At
     (Drivers : Launched_Driver_Array; Location : HP.PCI_Location) return Unsigned_64
   is
      Found : Unsigned_64 := No_Process_Word;
   begin
      for I in Drivers'Range loop
         pragma Loop_Invariant
           ((if Found = No_Process_Word then
               (for all J in Drivers'First .. I - 1 => not Serves_At (Drivers (J), Location))
             else
               (for some J in Drivers'First .. I - 1 =>
                  Serves_At (Drivers (J), Location) and then Drivers (J).Process = Found)
               and then
               (for all J in Drivers'First .. I - 1 =>
                  (if Serves_At (Drivers (J), Location) then Drivers (J).Process = Found))));
         if Serves_At (Drivers (I), Location) then
            if Found = No_Process_Word then
               Found := Drivers (I).Process;
            elsif Drivers (I).Process /= Found then
               return No_Process_Word;   --  two drivers for one function: refuse
            end if;
         end if;
      end loop;
      return Found;
   end Provider_At;
end Devmgr_HID_Offers;
