pragma Ada_2022;
package body ACPI_HID_I2C_Channels with SPARK_Mode is
   package S renames Candidates.S;
   package P renames Candidates.G;

   function Prepare (Item : Candidates.Candidate) return Preparation is
      Target : constant I.Target :=
        (Mode => (case Item.I2C.Addressing is
           when S.Seven_Bit => I.Seven_Bit, when S.Ten_Bit => I.Ten_Bit),
         Address => I.Address_Value (Item.I2C.Address),
         Maximum_Hz => I.Hertz (Item.I2C.Connection_Speed));
   begin
      if Item.I2C_Role /= S.Consumes_Resource then
         return (Status => Unsupported_I2C_Role);
      elsif Item.Initiated_By /= S.Controller_Initiated then
         return (Status => Unsupported_Initiator);
      elsif Item.Interrupt_Role /= P.Consumes_Resource then
         return (Status => Unsupported_GPIO_Role);
      elsif not I.Valid (Target) then
         return (Status => Invalid_I2C_Target);
      elsif Item.Pin > P.Pin_Number (G.Pin_Number'Last) then
         return (Status => Invalid_GPIO_Input);
      end if;
      declare
         Input : constant G.Interrupt_Input :=
           (Pin => G.Pin_Number (Item.Pin),
            Trigger => (case Item.Interrupt.Mode is
               when P.Level_Triggered => G.Level_Triggered,
               when P.Edge_Triggered => G.Edge_Triggered),
            Active => (case Item.Interrupt.Polarity is
               when P.Active_High => G.Active_High,
               when P.Active_Low => G.Active_Low,
               when P.Active_Both => G.Active_Both));
      begin
         if not G.Valid (Input) then return (Status => Invalid_GPIO_Input); end if;
         return (Status => Prepared, I2C => Target, GPIO => Input, Facts => Item);
      end;
   end Prepare;
end ACPI_HID_I2C_Channels;
