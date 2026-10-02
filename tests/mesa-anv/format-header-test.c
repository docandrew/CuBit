/* Compile-only CuBit C/C++ regression; no Linux DRM ABI is provided. */
#include "drm-uapi/drm_fourcc.h"
#ifdef _DRM_H_
#error "format identifiers must not import the DRM device-control ABI"
#endif
#ifdef DRM_IOCTL_VERSION
#error "unexpected ioctl definitions"
#endif
#ifdef __cplusplus
#define CHECK static_assert
#else
#define CHECK _Static_assert
#endif
CHECK(sizeof(__u32) == 4, "fourcc width");
CHECK(sizeof(__u64) == 8, "modifier width");
CHECK(DRM_FORMAT_XRGB8888 == UINT32_C(0x34325258), "XRGB8888");
CHECK(DRM_FORMAT_MOD_LINEAR == UINT64_C(0), "linear");
CHECK(DRM_FORMAT_MOD_INVALID == UINT64_C(0x00ffffffffffffff), "invalid");
CHECK(I915_FORMAT_MOD_X_TILED == UINT64_C(0x0100000000000001), "Intel X");
CHECK(I915_FORMAT_MOD_Y_TILED == UINT64_C(0x0100000000000002), "Intel Y");
CHECK(I915_FORMAT_MOD_Y_TILED_GEN12_RC_CCS ==
      UINT64_C(0x0100000000000006), "Gen12 render compression");
