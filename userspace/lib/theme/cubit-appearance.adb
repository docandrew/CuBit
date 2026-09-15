package body CuBit.Appearance with SPARK_Mode is
   function Encode (Item : Preferences) return Encoding is
     ([Character'Val (Character'Pos ('0') + Color_Scheme'Pos (Item.Scheme)),
       Character'Val (Character'Pos ('0') + Background'Pos (Item.Backdrop)),
       Character'Val (Character'Pos ('0') + Placement'Pos (Item.Position))]);
   function Valid (Text : String) return Boolean is
     (Text'Length = 3 and then Text (Text'First) in '0' .. '1'
      and then Text (Text'First + 1) in '0' .. '3'
      and then Text (Text'First + 2) in '0' .. '2');
   function Decode (Text : String) return Preferences is
     ((Color_Scheme'Val (Character'Pos (Text (Text'First)) - Character'Pos ('0')),
       Background'Val (Character'Pos (Text (Text'First + 1)) - Character'Pos ('0')),
       Placement'Val (Character'Pos (Text (Text'First + 2)) - Character'Pos ('0'))));
end CuBit.Appearance;
