pragma Ada_2022;

package body CuBit.Doom_Mixer with SPARK_Mode is

   Sample_Bias : constant := 128;
   --  An 8-bit sample times a 7-bit volume, doubled, spans 16 bits.
   Volume_Gain : constant := 2;

   procedure Pan (Vol, Sep : Integer; Left, Right : out Volume) is
      V : constant Volume :=
        (if Vol < 0 then 0 elsif Vol > Maximum_Volume then Maximum_Volume
         else Vol);
      S : constant Natural range 0 .. Maximum_Separation :=
        (if Sep < 0 then 0
         elsif Sep > Maximum_Separation then Maximum_Separation else Sep);
   begin
      Left := V * (Maximum_Separation - S) / Maximum_Separation;
      Right := V * S / Maximum_Separation;
   end Pan;

   function Started (Length : Sample_Count; Rate : Sample_Rate;
                     Vol, Sep : Integer) return Channel
   is
      Left, Right : Volume;
   begin
      Pan (Vol, Sep, Left, Right);
      return (Active => True, Length => Length, At_Sample => 0,
              Advance => Position (Rate) * One / Output_Rate,
              Left => Left, Right => Right);
   end Started;

   function Saturated (Sum : Integer_32) return Mix_Value is
     (if Sum < Mix_Value'First then Mix_Value'First
      elsif Sum > Mix_Value'Last then Mix_Value'Last else Sum)
     with Pre => Sum in -2 ** 25 .. 2 ** 25;

   procedure Mix (Samples : Byte_Array; Item : in out Channel;
                  Into : in out Mix_Buffer)
   is
      Length : constant Sample_Count := Item.Length;
      Where : Position := Item.At_Sample;
      Index : Natural;
      Raw   : Integer_32 range -Sample_Bias .. Sample_Bias - 1;
   begin
      for Frame in 0 .. Mix_Frames - 1 loop
         if Where / One >= Position (Length) then
            Item.Active := False;
            exit;
         end if;
         Index := Natural (Where / One);
         Raw := Integer_32 (Samples (Index)) - Sample_Bias;
         Into (2 * Frame) := Saturated
           (Into (2 * Frame) + Raw * Integer_32 (Item.Left) * Volume_Gain);
         Into (2 * Frame + 1) := Saturated
           (Into (2 * Frame + 1) + Raw * Integer_32 (Item.Right) * Volume_Gain);
         --  Index < Length <= 2**31, so this stays below 2**48.
         Where := Where + Item.Advance;
      end loop;
      Item.At_Sample := Where;
   end Mix;

   function Clamped (Sum : Mix_Value) return Integer_16 is
     (if Sum > Mix_Value (Integer_16'Last) then Integer_16'Last
      elsif Sum < Mix_Value (Integer_16'First) then Integer_16'First
      else Integer_16 (Sum));

end CuBit.Doom_Mixer;
