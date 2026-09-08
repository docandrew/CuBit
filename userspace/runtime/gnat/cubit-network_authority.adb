pragma Ada_2022;
package body CuBit.Network_Authority with SPARK_Mode is
   function Mask (Prefix : Prefix_Length) return Unsigned_32 is
     (if Prefix = 0 then 0 else Shift_Left (Unsigned_32'Last, 32 - Prefix));

   function Valid (Item : Scope) return Boolean is
     (Item.First_Port /= 0 and then Item.Last_Port >= Item.First_Port and then
      (Item.Network and Mask (Item.Prefix)) = Item.Network and then
      (if Item.Action = Listen_TCP then
         Item.Prefix = 32 and then Item.Network /= 0 and then
         Shift_Right (Item.Network, 24) < 224 and then
         Item.First_Port = Item.Last_Port and then not Item.Resolve_Names));

   function Allows
     (Item : Scope; Action : Operation; Address : Unsigned_32;
      Port : Unsigned_16) return Boolean is
     (Valid (Item) and then Item.Action = Action and then
      (Address and Mask (Item.Prefix)) = Item.Network and then
      Port >= Item.First_Port and then Port <= Item.Last_Port);

   function Includes (Ceiling, Requested : Scope) return Boolean is
     (Valid (Ceiling) and then Valid (Requested) and then
      Ceiling.Action = Requested.Action and then
      Requested.Prefix >= Ceiling.Prefix and then
      (Requested.Network and Mask (Ceiling.Prefix)) = Ceiling.Network and then
      Requested.First_Port >= Ceiling.First_Port and then
      Requested.Last_Port <= Ceiling.Last_Port and then
      (not Requested.Resolve_Names or else Ceiling.Resolve_Names));

   function Descriptor (Item : Scope) return Unsigned_64 is
     (Unsigned_64 (Item.First_Port) or
      Shift_Left (Unsigned_64 (Item.Last_Port), 16) or
      Shift_Left (Unsigned_64 (Item.Prefix), 32) or
      Shift_Left (Unsigned_64 (Operation'Enum_Rep (Item.Action)), 40) or
      (if Item.Resolve_Names then Shift_Left (Unsigned_64'(1), 48) else 0));

   procedure Decode
     (Address, Descriptor : Unsigned_64; Item : out Scope;
      Success : out Boolean)
   is
      Prefix : constant Unsigned_64 := Shift_Right (Descriptor, 32) and 255;
      Action : constant Unsigned_64 := Shift_Right (Descriptor, 40) and 255;
   begin
      Item := Denied_Scope;
      Success := False;
      if Address > Unsigned_64 (Unsigned_32'Last) or else Prefix > 32 or else
        Action not in 1 .. 2 or else Shift_Right (Descriptor, 49) /= 0
      then
         return;
      end if;
      Item :=
        (Action => (if Action = 1 then Connect_TCP else Listen_TCP),
         Network => Unsigned_32 (Address), Prefix => Prefix_Length (Prefix),
         First_Port => Unsigned_16 (Descriptor and 16#FFFF#),
         Last_Port => Unsigned_16 (Shift_Right (Descriptor, 16) and 16#FFFF#),
         Resolve_Names =>
           (Descriptor and Shift_Left (Unsigned_64'(1), 48)) /= 0);
      Success := Valid (Item);
      if not Success then
         Item := Denied_Scope;
      end if;
   end Decode;
end CuBit.Network_Authority;
