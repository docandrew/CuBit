-- An independent, plainly written statement of what may be copied, for the
-- differential test: 128-bit arithmetic instead of the overflow-guarded
-- expressions Capability_Install uses, and no shared helper.
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Capability_Install;

package Reference is
   function Derivable
     (Source : Capability; Item : Capability_Install.Request) return Boolean;
end Reference;
