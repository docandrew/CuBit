// Host-only regression of the actual SWGL span routine, not the browser.
// load_shader is stubbed: this test never dispatches a shader or creates a GL context.
// Output buffers include sentinels; byte equality is checked by the Python runner.
#include "gl.cc"
#include <chrono>
#include <vector>
#include <fstream>
int main(int argc, char** argv) {
  if (argc != 2) return 2;
  alignas(16) float data[128] = {};
  float colors[] = {0.7f,0.1f,0.2f,1.0f, 0.05f,0.4f,0.1f,0.5f, 0.1f,0.2f,0.9f,1.0f, 0,0,0,0};
  memcpy(data + 4, colors, sizeof(colors));
  sampler2D_impl sampler;
  sampler.buf = reinterpret_cast<uint32_t*>(data);
  sampler.width = 32; sampler.height = 1; sampler.stride = 128;
  sampler.format = TextureFormat::RGBA32F;
  std::ofstream output(argv[1], std::ios::binary);
  size_t cases = 0;
  for (int count : {2,3}) {
    int offsets = 4 + count*4;
    for (bool hard : {false,true}) {
      // Restore colors because changing stop count overlaps the offset storage.
      memcpy(data + 4, colors, sizeof(colors));
      data[offsets]=0; data[offsets+1]=count==2 ? 1 : (hard ? 0 : 0.5f);
      if (count==3) data[offsets+2]=1;
      for (bool repeat : {false,true}) for (int width : {4,8,12,128,800,1024})
      for (float y : {-0.125f,0.0f,0.1f,0.5f,0.999f,1.0f,1.125f})
      for (float delta : {0.0f,1.0f/128,-1.0f/128}) {
        std::vector<uint32_t> pixels(width+8,0xdeadbeef);
        vec2 pos(Float(0,1,2,3), Float(y));
        if (!commitLinearGradientFromStops<false,false>(&sampler,offsets,4,float(count),repeat,pos,vec2_scalar(delta,1),0,pixels.data()+4,width)) return 3;
        for (int i=0;i<4;i++) if(pixels[i]!=0xdeadbeef || pixels[width+4+i]!=0xdeadbeef) return 4;
        output.write(reinterpret_cast<char*>(pixels.data()+4),width*4); cases++;
      }
    }
  }
  output.close();
  memcpy(data+4,colors,sizeof(colors)); data[12]=0;data[13]=1;
  std::vector<uint32_t> pixels(800);
  volatile uint64_t checksum = 0;
  for (int trial=0;trial<5;trial++) {
    auto start=std::chrono::steady_clock::now();
    for(int row=0;row<5000;row++) {
      float y=float(row%311)/311;
      commitLinearGradientFromStops<false,false>(&sampler,12,4,2,false,vec2(Float(0,1,2,3),Float(y)),vec2_scalar(0,1),0,pixels.data(),800);
      checksum = checksum + pixels[row%800];
    }
    double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
    printf("trial=%d ms=%.3f\n",trial,ms);
  }
  printf("cases=%zu checksum=%llu\n",cases,(unsigned long long)checksum);
}
