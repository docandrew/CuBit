with CCL.Objects.Schemas;
with Interfaces;

package body Config_Schema_Worker with SPARK_Mode is
   package P renames Config_Schema_Protocol;
   use type Interfaces.Unsigned_32;
   use type P.Operation;
   use type P.Reply_Kind;
   procedure Handle
     (Object : in out State; Request : P.Frame;
      Response : out P.Frame; Accepted : out Boolean)
   is
      Contract, Recovered : CCL.Objects.Binding;
      Action : P.Operation;
      Result : P.Reply_Kind;
      Good : Boolean;
      procedure Fail is
      begin
         Object.Failed := True;
         P.Make_Reply (Request,
           (if Request.Action = P.Operation'Enum_Rep (P.Create) then P.Uncertain else P.Load_Failed),
           Contract, Response, Accepted);
      end Fail;
   begin
      Response := (others => <>); Accepted := False;
      if not P.Valid_Request (Request) then return; end if;
      if Object.Failed then Fail; return; end if;
      Action := (if Request.Action = P.Operation'Enum_Rep (P.Create) then P.Create else P.Recover);
      if Action = P.Create then
         CCL.Objects.Schemas.Read (Request.Metadata, Contract, Good);
         if not Good then return; end if;
      end if;
      Invoke (Action, Request.Name (1 .. Natural (Request.Name_Length)),
        Request.Context (1 .. Natural (Request.Context_Length)), Contract, Recovered, Result);
      P.Make_Reply (Request, Result, Recovered, Response, Accepted);
      if not Accepted or else Result in P.Uncertain | P.Load_Failed then Fail; end if;
   end Handle;
end Config_Schema_Worker;
