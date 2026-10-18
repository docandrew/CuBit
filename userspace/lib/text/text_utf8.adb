package body Text_UTF8 with SPARK_Mode is
   function Byte (C : Character) return Code_Point is (Code_Point (Character'Pos (C)));

   procedure Decode
     (Text : String; Index : Positive; Code : out Code_Point; Size : out Sequence_Length; Valid : out Boolean)
   is
      Lead : constant Code_Point := Byte (Text (Index));
      Needed : Natural;
      Minimum : Code_Point;
      Value : Natural;
   begin
      Code := Lead;
      Size := 1;
      Valid := Lead < 16#80#;
      if Valid then
         return;
      end if;
      if Lead in 16#C2# .. 16#DF# then
         Needed := 1;
         Minimum := 16#80#;
         Value := Natural (Lead) - 16#C0#;
      elsif Lead in 16#E0# .. 16#EF# then
         Needed := 2;
         Minimum := 16#800#;
         Value := Natural (Lead) - 16#E0#;
      elsif Lead in 16#F0# .. 16#F4# then
         Needed := 3;
         Minimum := 16#1_0000#;
         Value := Natural (Lead) - 16#F0#;
      else
         return;
      end if;
      if Needed > Text'Last - Index then
         return;
      end if;
      for K in 1 .. Needed loop
         pragma Loop_Invariant (Value < 2 ** (5 + 6 * (K - 1)));
         if not Is_Continuation (Text (Index + K)) then
            return;
         end if;
         Value := Value * 64 + (Character'Pos (Text (Index + K)) - 16#80#);
      end loop;
      if Value < Natural (Minimum) or else Value > MAXIMUM_CODE_POINT or else Value in 16#D800# .. 16#DFFF# then
         return;
      end if;
      Code := Code_Point (Value);
      Size := Needed + 1;
      Valid := True;
   end Decode;

   function Extends (Code : Code_Point) return Boolean is
     (Code in 16#0300# .. 16#036F#     --  combining diacritical marks
             | 16#0483# .. 16#0489#
             | 16#0591# .. 16#05BD#
             | 16#0610# .. 16#061A#
             | 16#064B# .. 16#065F#
             | 16#0E31# | 16#0E34# .. 16#0E3A# | 16#0E47# .. 16#0E4E#
             | 16#1AB0# .. 16#1AFF#
             | 16#1DC0# .. 16#1DFF#
             | 16#200C#                 --  zero-width non-joiner
             | 16#20D0# .. 16#20FF#
             | 16#302A# .. 16#302F#
             | 16#3099# .. 16#309A#
             | 16#FE00# .. 16#FE0F#     --  variation selectors
             | 16#FE20# .. 16#FE2F#
             | 16#1F3FB# .. 16#1F3FF#   --  emoji skin-tone modifiers
             | 16#E0020# .. 16#E007F#   --  tag characters (flag sequences)
             | 16#E0100# .. 16#E01EF#);

   function Width (Code : Code_Point) return Cell_Width is
     (if Code in 16#1100# .. 16#115F# | 16#2E80# .. 16#303E# | 16#3041# .. 16#33FF# | 16#3400# .. 16#4DBF#
                | 16#4E00# .. 16#9FFF# | 16#A000# .. 16#A4CF# | 16#AC00# .. 16#D7A3# | 16#F900# .. 16#FAFF#
                | 16#FE30# .. 16#FE4F# | 16#FF00# .. 16#FF60# | 16#FFE0# .. 16#FFE6#
                | 16#1F300# .. 16#1F64F# | 16#1F900# .. 16#1F9FF# | 16#20000# .. 16#3FFFD#
      then 2
      elsif Extends (Code) or else Code = ZERO_WIDTH_JOINER then 0
      else 1);

   function Cluster_Size (Text : String; Index : Positive) return Cluster_Length is
      Code, Next_Code : Code_Point;
      Size, Next_Size : Sequence_Length;
      Valid, Next_Valid : Boolean;
      Total : Natural;
      Regional : Boolean;
      Joined : Boolean := False;
   begin
      Decode (Text, Index, Code, Size, Valid);
      Total := Size;
      if not Valid then
         return 1;
      end if;
      --  CR LF is one cluster.
      if Code = Character'Pos (ASCII.CR) then
         return (if Index < Text'Last and then Text (Index + 1) = ASCII.LF then 2 else 1);
      end if;
      if Code < 16#20# then
         return 1;
      end if;
      Regional := Is_Regional_Indicator (Code);
      while Index <= Text'Last - Total loop
         pragma Loop_Invariant (Total in 1 .. MAXIMUM_CLUSTER_BYTES and then Total <= Text'Last - Index);
         pragma Loop_Variant (Increases => Total);
         Decode (Text, Index + Total, Next_Code, Next_Size, Next_Valid);
         exit when not Next_Valid;
         exit when Total + Next_Size > MAXIMUM_CLUSTER_BYTES;
         if Joined then
            --  The code point after a zero-width joiner joins.
            Joined := False;
         elsif Next_Code = ZERO_WIDTH_JOINER then
            Joined := True;
         elsif Regional and then Is_Regional_Indicator (Next_Code) then
            Regional := False;
         elsif not Extends (Next_Code) then
            exit;
         else
            Regional := False;
         end if;
         Total := Total + Next_Size;
      end loop;
      return Total;
   end Cluster_Size;

   procedure Cluster_Face
     (Text : String; Index : Positive; Code : out Code_Point; Columns : out Cell_Width; Valid : out Boolean)
   is
      Size : Sequence_Length;
   begin
      Decode (Text, Index, Code, Size, Valid);
      Columns := (if Valid then Width (Code) else 2);
      if Valid and then Columns = 0 then
         --  A mark with nothing before it still takes a cell.
         Columns := 1;
      end if;
   end Cluster_Face;
end Text_UTF8;
