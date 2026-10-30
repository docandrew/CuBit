
Channel admission now decodes I2C_Channel_Badges after the required native
Authorized callback. The decoded seven-bit target and max-Hz ceiling are passed
to the controller for this request only. Invalid_Channel and Unsupported_Channel
are typed refusals before input/output grants or MMIO. Message contents never
replace the kernel-stamped badge. Saved reply/caller/grant ownership continues
through completion while competing submissions receive Busy. The codec's pure
proof does not prove native authority stamping or this stateful composition.
