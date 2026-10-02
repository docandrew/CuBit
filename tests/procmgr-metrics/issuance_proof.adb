with Interfaces; use Interfaces;
with CuBit.Authority_Policy; use CuBit.Authority_Policy;
package body Issuance_Proof with SPARK_Mode is
   procedure Check (Requested, Installation, Session, Issuer : Boolean;
                    ID : CuBit.Metric_Protocol.Issuance) is
      package P renames CuBit.Metric_Protocol;
   begin
      pragma Assert (Bootstrap_Approves (Metric_Publication, Session));
      pragma Assert (Bootstrap_Approves (Metric_Observation, Session) = Session);
      pragma Assert ((Evaluate (Requested, Installation, Session, Issuer) = Approved)
                     = (Requested and Installation and Session and Issuer));
      pragma Assert (P.Is_Publisher (P.Publisher_Tag (ID)));
      pragma Assert (P.Is_Observer (P.Observer_Tag (ID)));
      pragma Assert (P.Publisher_Tag (ID) /= P.Observer_Tag (ID));
      pragma Assert (P.May_Invoke (P.Publisher_Tag (ID), P.Publish_Batch));
      pragma Assert (not P.May_Invoke (P.Publisher_Tag (ID), P.Query_Summaries));
      pragma Assert (P.May_Invoke (P.Observer_Tag (ID), P.Query_Summaries));
      pragma Assert (not P.May_Invoke (P.Observer_Tag (ID), P.Publish_Batch));
      pragma Assert (not P.Is_Publisher (0) and not P.Is_Observer (0));
   end Check;
end Issuance_Proof;
