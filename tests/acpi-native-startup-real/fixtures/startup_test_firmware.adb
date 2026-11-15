pragma Ada_2022;
with Owner_Test_Control;with Root_Test_Support;use Root_Test_Support;
with Interfaces;use Interfaces;with AML_Decode;
package body Startup_Test_Firmware is
   package T renames Owner_Test_Control;
   use type AML_Decode.Integer_Width,Bytes;
   Header_Bytes : constant Positive:=36;
   FADT_Bytes : constant Positive:=276;
   Revision_Byte : constant Positive:=9;
   Checksum_Byte : constant Positive:=10;
   Hardware_Reduced_Flag_Byte : constant Positive:=115;
   Hardware_Reduced_Flag : constant Unsigned_8:=16#10#;
   Fatal : constant Bytes := [16#5B#,16#32#,16#A7#,16#78#,16#56#,16#34#,16#F2#,16#0A#,16#55#];
   Recovery : constant Bytes := [8] & Enc("DERR") & [0,8] & Enc("RSM0") &
     [0,16#70#,16#78#,1] & Enc("DERR") & [0,0] & Enc("DERR") & [16#70#,16#0A#,91] & Enc("RSM0");
   procedure Seal(Raw:in out Bytes;Signature:String;Revision:Unsigned_8) is
      Sum:Unsigned_8:=0;
   begin
      Raw(1..4):=Enc(Signature);
      for I in 1..4 loop Raw(4+I):=Unsigned_8((Raw'Length/256**(I-1))mod 256);end loop;
      Raw(Revision_Byte):=Revision;Raw(Checksum_Byte):=0;
      for B of Raw loop Sum:=Sum+B;end loop;Raw(Checksum_Byte):=0-Sum;
   end Seal;
   function Image(ID:Positive) return Bytes is
   begin
      if ID=FADT_ID then
         declare Raw:Bytes(1..FADT_Bytes):=[others=>0];begin
            -- Hardware-reduced structural FADT, without sleep-control registers,
            -- FACS, NVS or IRQ. It makes no claim to usable power hardware.
            Raw(Hardware_Reduced_Flag_Byte):=Hardware_Reduced_Flag;
            Seal(Raw,"FACP",6);return Raw;
         end;
      elsif ID=DSDT_ID then
         declare
            Payload:constant Bytes := (case T.Selected is
              when T.Recovered|T.Recovery_Log_Failure=>Recovery,
              when T.Recovered_Then_Failed=>Recovery & Bytes'[16#5B#,16#21#,1],
              when T.Stalled_Initializer=>Block([16#5B#,16#82#],Enc("DEV0") & Method("_INI",[16#5B#,16#21#,1])),
              when T.Module_Fatal=>Fatal,
              when T.Root_Fatal=>Method("_INI",Fatal),
              when T.Bus_Fatal=>Block([16#10#],Enc("_SB_") & Method("_INI",Fatal)),
              when T.Device_Fatal=>Block([16#5B#,16#82#],Enc("DEV0") & Method("_INI",Fatal)),
              when T.Ordinary_Error=>Block([16#5B#,16#82#],Enc("DEV0") & Method("_INI",[16#FE#])),
              when T.Unconfigured_Region=>[16#5B#,16#80#] & Enc("REG0") & [3,0,1] &
                Block([16#5B#,16#81#],Enc("REG0") & [1] & Enc("CELL") & [8]) &
                Block([16#5B#,16#82#],Enc("DEV0") & Method("_INI",[16#70#] & Enc("CELL") & [16#60#] & Fatal)),
              when others=>[16#A3#]);
            Raw:Bytes(1..Header_Bytes+Payload'Length):=[others=>0];
         begin
            Raw(Header_Bytes+1..Raw'Last):=Payload;
            Seal(Raw,"DSDT",(if T.Width=AML_Decode.Bits_32 then 1 else 2));return Raw;
         end;
      else raise Program_Error with "unknown fixture table";end if;
   end Image;
end Startup_Test_Firmware;
