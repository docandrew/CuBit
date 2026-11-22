pragma Ada_2022;
with ACPI_HID_I2C_Candidate;
with I2C_Channel_Badges;
with GPIO_Channel_Badges;
generic
   with package Candidates is new ACPI_HID_I2C_Candidate (<>);
package ACPI_HID_I2C_Channels with Pure, SPARK_Mode is
   use type Candidates.Candidate;
   package I renames I2C_Channel_Badges;
   package G renames GPIO_Channel_Badges;
   use type I.Target, G.Interrupt_Input;
   use type Candidates.S.Resource_Role, Candidates.S.Initiator;
   use type Candidates.S.Address_Mode, Candidates.S.Slave_Address, Candidates.S.Rate;
   use type Candidates.G.Resource_Role, Candidates.G.Pin_Number;
   use type Candidates.G.Interrupt_Mode, Candidates.G.Interrupt_Polarity;
   type Outcome is (Prepared, Unsupported_I2C_Role, Unsupported_Initiator,
      Unsupported_GPIO_Role, Invalid_I2C_Target, Invalid_GPIO_Input);
   type Preparation (Status : Outcome := Invalid_I2C_Target) is record
      case Status is
         when Prepared =>
            I2C : I.Target;
            GPIO : G.Interrupt_Input;
            Facts : Candidates.Candidate;
         when others => null;
      end case;
   end record;
   -- Data conversion only: neither these records nor encoded badge words
   -- create endpoint authority. The owning controller supplies a complete,
   -- current discovery observation; the launcher separately admits both
   -- controllers, timing/pin configuration, dependencies and device lifetime.
   -- Preserve every copied fact, including sharing/vendor/wake configuration.
   -- No bus-speed clamping, pin translation, hardware defaults or IO occurs.
   function Prepare (Item : Candidates.Candidate) return Preparation
     with Global => null,
       Post =>
         ((Prepare'Result.Status = Prepared) =
           (Item.I2C_Role = Candidates.S.Consumes_Resource
            and then Item.Initiated_By = Candidates.S.Controller_Initiated
            and then Item.Interrupt_Role = Candidates.G.Consumes_Resource
            and then Item.I2C.Connection_Speed /= 0
            and then (Item.I2C.Addressing = Candidates.S.Ten_Bit
              or else Item.I2C.Address <= Candidates.S.Slave_Address (I.Seven_Bit_Last))
            and then Item.Pin <= Candidates.G.Pin_Number (G.Pin_Number'Last)
            and then (Item.Interrupt.Mode = Candidates.G.Edge_Triggered
              or else Item.Interrupt.Polarity /= Candidates.G.Active_Both)))
         and then (if Prepare'Result.Status = Prepared then
           I.Valid (Prepare'Result.I2C) and then G.Valid (Prepare'Result.GPIO)
           and then Prepare'Result.Facts = Item
           and then Prepare'Result.I2C = I.Target'
             (Mode => (case Item.I2C.Addressing is
                when Candidates.S.Seven_Bit => I.Seven_Bit,
                when Candidates.S.Ten_Bit => I.Ten_Bit),
              Address => I.Address_Value (Item.I2C.Address),
              Maximum_Hz => I.Hertz (Item.I2C.Connection_Speed))
           and then Item.Pin <= Candidates.G.Pin_Number (G.Pin_Number'Last)
           and then Prepare'Result.GPIO = G.Interrupt_Input'
             (Pin => G.Pin_Number (Item.Pin),
              Trigger => (case Item.Interrupt.Mode is
                when Candidates.G.Level_Triggered => G.Level_Triggered,
                when Candidates.G.Edge_Triggered => G.Edge_Triggered),
              Active => (case Item.Interrupt.Polarity is
                when Candidates.G.Active_High => G.Active_High,
                when Candidates.G.Active_Low => G.Active_Low,
                when Candidates.G.Active_Both => G.Active_Both)));
end ACPI_HID_I2C_Channels;
