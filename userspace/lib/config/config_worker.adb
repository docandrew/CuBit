with Interfaces; use Interfaces;
with CBOR;

package body Config_Worker with SPARK_Mode is
   package P renames Config_Worker_Protocol;
   package Codec renames CCL.Objects.Persistence;
   use type Codec.Outcome;
   use type P.Operation;
   use type P.Reply_Kind;

   procedure Handle
     (Worker : in out State; Request : P.Frame;
      Contract : CCL.Objects.Binding; Response : out P.Frame; Accepted : out Boolean)
   is
      Input : Codec.Packet;
      Output : Config_Worker_Storage.Reply;
      Value : CCL.Objects.Image := CCL.Objects.Empty (Contract);
      Encoded : Codec.Outcome;
      Action : P.Operation;
      Kind : P.Reply_Kind;

      procedure Fail is
      begin
         Worker.Recovery_Required := True;
         P.Make_Reply (Request, (if Request.Action = P.Operation'Enum_Rep (P.Load)
                                then P.Load_Failed else P.Uncertain),
                       0, Contract, Value, Response, Accepted);
      end Fail;
   begin
      Response := (Value => CCL.Objects.Empty (Contract), others => <>);
      Accepted := False;
      if not P.Valid_Request (Request, Contract) then return; end if;
      if Worker.Recovery_Required then Fail; return; end if;
      Action := (if Request.Action = P.Operation'Enum_Rep (P.Load) then P.Load else P.Commit);
      if Action = P.Commit then
         Codec.Encode (Request.Value, Contract, Input, Encoded);
         if Encoded /= Codec.Success then Fail; return; end if;
      end if;
      Invoke (Action, Request.Name (1 .. Natural (Request.Name_Length)),
              Request.Context (1 .. Natural (Request.Context_Length)),
              Request.Revision, CCL.Objects.Identity (Contract), Input, Output);
      if Output.Code not in P.Reply_Kind'Enum_Rep (P.Loaded) .. P.Reply_Kind'Enum_Rep (P.Uncertain)
        or else Output.Length > Codec.Maximum_Encoded_Bytes
      then Fail; return; end if;
      Kind := P.Reply_Kind'Enum_Val (Output.Code);
      if Kind = P.Loaded then
         if Action /= P.Load then Fail; return; end if;
         Codec.Decode (Output.Data (1 .. CBOR.SE_Offset (Output.Length)), Contract, Value, Encoded);
         if Encoded /= Codec.Success then Fail; return; end if;
      elsif Output.Length /= 0 then
         Fail; return;
      end if;
      P.Make_Reply (Request, Kind, Output.Revision, Contract, Value, Response, Accepted);
      if not Accepted or else Kind in P.Load_Failed | P.Uncertain then Fail; end if;
   end Handle;
end Config_Worker;
