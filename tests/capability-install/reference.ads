-- An independent, plainly written statement of what may be copied, for the
-- differential test: 128-bit arithmetic instead of the overflow-guarded
-- expressions Capability_Derivation uses, and no shared helper.
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Capability_Derivation;

package Reference is
   function Derivable
     (Source : Capability; Item : Capability_Derivation.Request) return Boolean;
end Reference;
